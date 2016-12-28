#include "JIT.h"

#include <mutex>

#include "preprocessor/llvm_includes_start.h"
#include <llvm/IR/Module.h>
#include <llvm/IR/Mangler.h>
#include <llvm/ADT/StringSwitch.h>
#include <llvm/ADT/Triple.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/ManagedStatic.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/LambdaResolver.h>
#include "preprocessor/llvm_includes_end.h"

#include "Compiler.h"
#include "Optimizer.h"
#include "Cache.h"
#include "ExecStats.h"
#include "Utils.h"
#include "BuildInfo.gen.h"


static_assert(sizeof(evm_uint256be) == 32, "evm_uint256be is too big");
static_assert(sizeof(evm_uint160be) == 20, "evm_uint160be is too big");
static_assert(sizeof(evm_result) <= 64, "evm_result does not fit cache line");

// Check enums match int size.
// On GCC/clang the underlying type should be unsigned int, on MSVC int
static_assert(sizeof(evm_query_key)  == sizeof(int), "Enum `evm_query_key` is not the size of int");
static_assert(sizeof(evm_update_key) == sizeof(int), "Enum `evm_update_key` is not the size of int");
static_assert(sizeof(evm_call_kind)  == sizeof(int), "Enum `evm_call_kind` is not the size of int");
static_assert(sizeof(evm_mode)       == sizeof(int), "Enum `evm_mode` is not the size of int");


namespace dev
{
namespace evmjit
{
using namespace eth::jit;

namespace
{
using ExecFunc = ReturnCode(*)(ExecutionContext*);

char modeToChar(evm_mode mode)
{
	switch (mode)
	{
	case EVM_FRONTIER: return 'F';
	case EVM_HOMESTEAD: return 'H';
	case EVM_ANTI_DOS: return 'A';
	case EVM_CLEARING: return 'C';
	}
	LLVM_BUILTIN_UNREACHABLE;
}

/// Combine code hash and compatibility mode into a printable code identifier.
std::string makeCodeId(evm_uint256be codeHash, evm_mode mode)
{
	static const auto hexChars = "0123456789abcdef";
	std::string str;
	str.reserve(sizeof(codeHash) * 2 + 1);
	for (auto b: codeHash.bytes)
	{
		str.push_back(hexChars[b & 0xf]);
		str.push_back(hexChars[b >> 4]);
	}
	str.push_back(modeToChar(mode));
	return str;
}

void printVersion()
{
	std::cout << "Ethereum EVM JIT Compiler (http://github.com/ethereum/evmjit):\n"
			  << "  EVMJIT version " << EVMJIT_VERSION << "\n"
#ifdef NDEBUG
			  << "  Optimized build, "
#else
			  << "  DEBUG build, "
#endif
			  << __DATE__ << " (" << __TIME__ << ")\n"
			  << std::endl;
}

namespace cl = llvm::cl;
cl::opt<bool> g_optimize{"O", cl::desc{"Optimize"}};
cl::opt<CacheMode> g_cache{"cache", cl::desc{"Cache compiled EVM code on disk"},
	cl::values(
		clEnumValN(CacheMode::off,   "0", "Disabled"),
		clEnumValN(CacheMode::on,    "1", "Enabled"),
		clEnumValN(CacheMode::read,  "r", "Read only. No new objects are added to cache."),
		clEnumValN(CacheMode::write, "w", "Write only. No objects are loaded from cache."),
		clEnumValN(CacheMode::clear, "c", "Clear the cache storage. Cache is disabled."),
		clEnumValN(CacheMode::preload, "p", "Preload all cached objects."),
		clEnumValEnd)};
cl::opt<bool> g_stats{"st", cl::desc{"Statistics"}};
cl::opt<bool> g_dump{"dump", cl::desc{"Dump LLVM IR module"}};

void parseOptions()
{
	static llvm::llvm_shutdown_obj shutdownObj{};
	cl::AddExtraVersionPrinter(printVersion);
	cl::ParseEnvironmentOptions("evmjit", "EVMJIT", "Ethereum EVM JIT Compiler");
}

class EVMJIT
{
	std::unique_ptr<llvm::TargetMachine> m_tm;
	const llvm::DataLayout m_dl;
	llvm::orc::ObjectLinkingLayer<> m_linker;
	llvm::orc::IRCompileLayer<decltype(m_linker)> m_compiler;

public:
	using ModuleHandle = decltype(m_compiler)::ModuleSetHandleT;

	evm_query_fn queryFn = nullptr;
	evm_update_fn updateFn = nullptr;
	evm_call_fn callFn = nullptr;

	EVMJIT():
			m_tm(llvm::EngineBuilder{}.selectTarget()),
			m_dl(m_tm->createDataLayout()),
			m_compiler(m_linker, llvm::orc::SimpleCompiler(*m_tm))
	{
		auto f = m_tm->getTargetFeatureString().str();
		std::cerr << m_tm->getTargetTriple().str() << " " << f;
	}

	static EVMJIT& instance()
	{
		static EVMJIT s_instance;
		return s_instance;
	}

	ModuleHandle addModule(std::unique_ptr<llvm::Module> module)
	{
		auto Resolver = llvm::orc::createLambdaResolver(
				[&](const std::string &Name) {
					// TODO: We don't expect any local symbols.
					if (auto Sym = m_compiler.findSymbol(Name, false))
						return Sym.toRuntimeDyldSymbol();
					return llvm::RuntimeDyld::SymbolInfo(nullptr);
				},
				[this](std::string const& name) -> llvm::RuntimeDyld::SymbolInfo
				{
					auto addr = llvm::StringSwitch<uint64_t>(name)
							.Case("env_sha3", reinterpret_cast<uint64_t>(&dev::evmjit::keccak))
							.Case("free", reinterpret_cast<uint64_t>(&std::free))
							.Case("realloc", reinterpret_cast<uint64_t>(&std::realloc))
							.Case("malloc", reinterpret_cast<uint64_t>(&std::malloc))
							.Case("memcpy", reinterpret_cast<uint64_t>(&std::memcpy))
							.Case("memset", reinterpret_cast<uint64_t>(&std::memset))
							.Case("evm.query", reinterpret_cast<uint64_t>(this->queryFn))
							.Case("evm.update", reinterpret_cast<uint64_t>(this->updateFn))
							.Case("evm.call", reinterpret_cast<uint64_t>(this->callFn))
							.Default(0);
					return {addr, llvm::JITSymbolFlags::Exported};
				});

		// Build a singlton module set to hold our module.
		std::vector<std::unique_ptr<llvm::Module>> Ms;
		Ms.push_back(std::move(module));

		// Add the set to the JIT with the resolver we created above and a newly
		// created SectionMemoryManager.
		return m_compiler.addModuleSet(std::move(Ms),
		                               llvm::make_unique<llvm::SectionMemoryManager>(),
		                               std::move(Resolver));
	}

	llvm::orc::JITSymbol findSymbol(const std::string& Name) {
		std::string MangledName;
		llvm::raw_string_ostream MangledNameStream(MangledName);
		llvm::Mangler::getNameWithPrefix(MangledNameStream, Name, m_dl);
		return m_compiler.findSymbol(MangledNameStream.str(), false);
	}

	static llvm::LLVMContext& getLLVMContext()
	{
		// TODO: This probably should be thread_local, but for now that causes
		// a crash when MCJIT is destroyed.
		static llvm::LLVMContext llvmContext;
		return llvmContext;
	}

	ExecFunc compile(evm_mode _mode, byte const* _code, uint64_t _codeSize, std::string const& _codeIdentifier)
	{
		auto module = Cache::getObject(_codeIdentifier, getLLVMContext());
		if (!module)
		{
			// TODO: Listener support must be redesigned. These should be a feature of JITImpl
			//listener->stateChanged(ExecState::Compilation);
			assert(_code || !_codeSize);
			//TODO: Can the Compiler be stateless?
			module = Compiler({}, _mode, getLLVMContext()).compile(_code, _code + _codeSize, _codeIdentifier);

			if (g_optimize)
			{
				//listener->stateChanged(ExecState::Optimization);
				optimize(*module);
			}

			prepare(*module);
		}
		if (g_dump)
			module->dump();

		addModule(std::move(module));
		return getExecFunc(_codeIdentifier);
	}

	ExecFunc getExecFunc(std::string const& _codeIdentifier)
	{
		auto sym = findSymbol(_codeIdentifier);
		return (ExecFunc)sym.getAddress();
	}
};


class SymbolResolver : public llvm::SectionMemoryManager
{
	void reportMemorySize(size_t _addedSize)
	{
		if (!g_stats)
			return;

		m_totalMemorySize += _addedSize;
		if (m_totalMemorySize >= m_printMemoryLimit)
		{
			static const auto M = 1024 * 1024;
			auto value = double(m_totalMemorySize) / M;
			std::cerr << "EVMJIT total memory size: " << value << '\n';
			m_printMemoryLimit += M;
		}
	}

	uint8_t* allocateCodeSection(uintptr_t _size, unsigned _a, unsigned _id,
	                             llvm::StringRef _name) override
	{
		reportMemorySize(_size);
		return llvm::SectionMemoryManager::allocateCodeSection(_size, _a, _id, _name);
	}

	uint8_t* allocateDataSection(uintptr_t _size, unsigned _a, unsigned _id,
	                             llvm::StringRef _name, bool _ro) override
	{
		reportMemorySize(_size);
		return llvm::SectionMemoryManager::allocateDataSection(_size, _a, _id, _name, _ro);
	}

	size_t m_totalMemorySize = 0;
	size_t m_printMemoryLimit = 1024 * 1024;
};

} // anonymous namespace


ExecutionContext::~ExecutionContext() noexcept
{
	if (m_memData)
		std::free(m_memData); // Use helper free to check memory leaks
}

bytes_ref ExecutionContext::getReturnData() const
{
	auto data = m_data->callData;
	auto size = static_cast<size_t>(m_data->callDataSize);

	if (data < m_memData || data >= m_memData + m_memSize || size == 0)
	{
		assert(size == 0); // data can be an invalid pointer only if size is 0
		m_data->callData = nullptr;
		return {};
	}

	return bytes_ref{data, size};
}


extern "C"
{

static evm_instance* create(evm_query_fn queryFn, evm_update_fn updateFn,
	evm_call_fn callFn)
{
	parseOptions();

	// FIXME: Move it from here.
	llvm::InitializeNativeTarget();
	llvm::InitializeNativeTargetAsmPrinter();

	// Let's always return the same instance. It's a bit of faking, but actually
	// this might be a compliant implementation.
	auto& jit = EVMJIT::instance();
	jit.queryFn = queryFn;
	jit.updateFn = updateFn;
	jit.callFn = callFn;
	return reinterpret_cast<evm_instance*>(&jit);
}

static void destroy(evm_instance* instance)
{
	(void)instance;
	assert(instance == static_cast<void*>(&EVMJIT::instance()));
}

static void release_result(evm_result const* result)
{
	if (result->internal_memory)
		std::free(result->internal_memory);
}

static evm_result execute(evm_instance* instance, evm_env* env, evm_mode mode,
	evm_uint256be code_hash, uint8_t const* code, size_t code_size,
	int64_t gas, uint8_t const* input, size_t input_size, evm_uint256be value)
{
	auto& jit = *reinterpret_cast<EVMJIT*>(instance);

	RuntimeData rt;
	rt.code = code;
	rt.codeSize = code_size;
	rt.gas = gas;
	rt.callData = input;
	rt.callDataSize = input_size;
	std::memcpy(&rt.apparentValue, &value, sizeof(value));

	ExecutionContext ctx{rt, env};

	evm_result result;
	result.code = EVM_SUCCESS;
	result.gas_left = 0;
	result.output_data = nullptr;
	result.output_size = 0;
	result.internal_memory = nullptr;
	result.release = release_result;

	auto codeIdentifier = makeCodeId(code_hash, mode);
	auto execFunc = jit.getExecFunc(codeIdentifier);
	if (!execFunc)
		execFunc = jit.compile(mode, ctx.code(), ctx.codeSize(), codeIdentifier);

	auto returnCode = execFunc(&ctx);

	if (returnCode == ReturnCode::OutOfGas)
	{
		// EVMJIT does not provide information what exactly type of failure
		// it was, so use generic EVM_FAILURE.
		result.code = EVM_FAILURE;
	}
	else
	{
		// In case of success return the amount of gas left.
		result.gas_left = rt.gas;
	}

	if (returnCode == ReturnCode::Return)
	{
		auto out = ctx.getReturnData();
		result.output_data = std::get<0>(out);
		result.output_size = std::get<1>(out);
	}

	// Take care of the internal memory.
	result.internal_memory = ctx.m_memData;
	ctx.m_memData = nullptr;

	return result;
}

static int set_option(evm_instance* instance, char const* name,
	char const* value)
{
	(void)instance, (void)name, (void)value;
	return 0;
}

static evm_code_status get_code_status(evm_instance* instance,
	evm_mode mode, evm_uint256be code_hash)
{
	auto& jit = *reinterpret_cast<EVMJIT*>(instance);
	auto codeIdentifier = makeCodeId(code_hash, mode);
	if (jit.getExecFunc(codeIdentifier) != nullptr)
		return EVM_READY;
	// TODO: Add support for EVM_CACHED.
	return EVM_UNKNOWN;
}

static void prepare_code(evm_instance* instance, evm_mode mode,
	evm_uint256be code_hash, unsigned char const* code, size_t code_size)
{
	auto& jit = *reinterpret_cast<EVMJIT*>(instance);
	auto codeIdentifier = makeCodeId(code_hash, mode);
	jit.compile(mode, code, code_size, codeIdentifier);
}

EXPORT evm_interface evmjit_get_interface()
{
	return {EVM_ABI_VERSION, create, destroy, execute,
			get_code_status, prepare_code, set_option};
}

}  // extern "C"
}
}
