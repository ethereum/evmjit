#include "evmjit/JIT.h"

#include <mutex>

#include "preprocessor/llvm_includes_start.h"
#include <llvm/IR/Module.h>
#include <llvm/ADT/StringSwitch.h>
#include <llvm/ADT/Triple.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/ManagedStatic.h>
#include "preprocessor/llvm_includes_end.h"

#include "Compiler.h"
#include "Optimizer.h"
#include "Cache.h"
#include "ExecStats.h"
#include "Utils.h"
#include "BuildInfo.gen.h"

namespace dev
{
namespace evmjit
{
using namespace eth::jit;

namespace
{
using ExecFunc = ReturnCode(*)(ExecutionContext*);

/// Combine code hash and compatibility mode into a printable code identifier.
std::string makeCodeId(evm_hash256 codeHash, evm_mode mode)
{
	static const auto hexChars = "0123456789abcdef";
	std::string str;
	str.reserve(sizeof(codeHash) * 2 + 1);
	for (auto b: codeHash.bytes)
	{
		str.push_back(hexChars[b & 0xf]);
		str.push_back(hexChars[b >> 4]);
	}
	str.push_back(mode == EVM_FRONTIER ? 'F' : 'H');
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
		clEnumValN(CacheMode::on,    "1", "Enabled"),
		clEnumValN(CacheMode::off,   "0", "Disabled"),
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

class JITImpl
{
	std::unique_ptr<llvm::ExecutionEngine> m_engine;
	mutable std::mutex x_codeMap;
	std::unordered_map<std::string, ExecFunc> m_codeMap;

	static llvm::LLVMContext& getLLVMContext()
	{
		// TODO: This probably should be thread_local, but for now that causes
		// a crash when MCJIT is destroyed.
		static llvm::LLVMContext llvmContext;
		return llvmContext;
	}

public:
	static JITImpl& instance()
	{
		// We need to keep this a singleton.
		// so we only call changeVersion on it.
		static JITImpl s_instance;
		return s_instance;
	}

	JITImpl();

	llvm::ExecutionEngine& engine() { return *m_engine; }

	ExecFunc getExecFunc(std::string const& _codeIdentifier) const;
	void mapExecFunc(std::string const& _codeIdentifier, ExecFunc _funcAddr);

	ExecFunc compile(evm_mode _mode, byte const* _code, uint64_t _codeSize, std::string const& _codeIdentifier);

	evm_query_fn queryFn = nullptr;
	evm_update_fn updateFn = nullptr;
	evm_call_fn callFn = nullptr;
};


class SymbolResolver : public llvm::SectionMemoryManager
{
	llvm::RuntimeDyld::SymbolInfo findSymbol(std::string const& _name) override
	{
		auto& jit = JITImpl::instance();
		auto addr = llvm::StringSwitch<uint64_t>(_name)
			.Case("env_sha3", reinterpret_cast<uint64_t>(&keccak))
			.Case("evm.query", reinterpret_cast<uint64_t>(jit.queryFn))
			.Case("evm.update", reinterpret_cast<uint64_t>(jit.updateFn))
			.Case("evm.call", reinterpret_cast<uint64_t>(jit.callFn))
			.Default(0);
		if (addr)
			return {addr, llvm::JITSymbolFlags::Exported};

		// Fallback to default implementation that would search for the symbol
		// in the current process.
		// TODO: In the future we should control the whole set of requested
		//       symbols (like memcpy, memset, etc) to improve performance.
		return llvm::SectionMemoryManager::findSymbol(_name);
	}
};


JITImpl::JITImpl()
{
	parseOptions();

	bool preloadCache = g_cache == CacheMode::preload;
	if (preloadCache)
		g_cache = CacheMode::on;

	llvm::InitializeNativeTarget();
	llvm::InitializeNativeTargetAsmPrinter();

	auto module = llvm::make_unique<llvm::Module>("", getLLVMContext());

	// FIXME: LLVM 3.7: test on Windows
	auto triple = llvm::Triple(llvm::sys::getProcessTriple());
	if (triple.getOS() == llvm::Triple::OSType::Win32)
		triple.setObjectFormat(llvm::Triple::ObjectFormatType::ELF);  // MCJIT does not support COFF format
	module->setTargetTriple(triple.str());

	llvm::EngineBuilder builder(std::move(module));
	builder.setEngineKind(llvm::EngineKind::JIT);
	builder.setMCJITMemoryManager(llvm::make_unique<SymbolResolver>());
	builder.setOptLevel(g_optimize ? llvm::CodeGenOpt::Default : llvm::CodeGenOpt::None);
	#ifndef NDEBUG
	builder.setVerifyModules(true);
	#endif

	m_engine.reset(builder.create());

	// TODO: Update cache listener
	m_engine->setObjectCache(Cache::init(g_cache, nullptr));

	// FIXME: Disabled during API changes
	//if (preloadCache)
	//	Cache::preload(*m_engine, funcCache);
}

ExecFunc JITImpl::getExecFunc(std::string const& _codeIdentifier) const
{
	std::lock_guard<std::mutex> lock{x_codeMap};
	auto it = m_codeMap.find(_codeIdentifier);
	if (it != m_codeMap.end())
		return it->second;
	return nullptr;
}

void JITImpl::mapExecFunc(std::string const& _codeIdentifier, ExecFunc _funcAddr)
{
	std::lock_guard<std::mutex> lock{x_codeMap};
	m_codeMap.emplace(_codeIdentifier, _funcAddr);
}

ExecFunc JITImpl::compile(evm_mode _mode, byte const* _code, uint64_t _codeSize,
	std::string const& _codeIdentifier)
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

	m_engine->addModule(std::move(module));
	//listener->stateChanged(ExecState::CodeGen);
	return (ExecFunc)m_engine->getFunctionAddress(_codeIdentifier);
}

} // anonymous namespace


extern "C" void ext_free(void* _data) noexcept;

ExecutionContext::~ExecutionContext() noexcept
{
	if (m_memData)
		ext_free(m_memData); // Use helper free to check memory leaks
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
	// Let's always return the same instance. It's a bit of faking, but actually
	// this might be a compliant implementation.
	auto& jit = JITImpl::instance();
	jit.queryFn = queryFn;
	jit.updateFn = updateFn;
	jit.callFn = callFn;
	return reinterpret_cast<evm_instance*>(&jit);
}

static void destroy(evm_instance* instance)
{
    (void)instance;
	assert(instance == static_cast<void*>(&JITImpl::instance()));
}

static evm_result execute(evm_instance* instance, evm_env* env,
	evm_mode mode, evm_hash256 code_hash, uint8_t const* code, size_t code_size,
	int64_t gas, uint8_t const* input, size_t input_size, evm_uint256 value)
{
	auto& jit = *reinterpret_cast<JITImpl*>(instance);

	RuntimeData rt;
	rt.code = code;
	rt.codeSize = code_size;
	rt.gas = gas;
	rt.callData = input;
	rt.callDataSize = input_size;
	std::memcpy(&rt.apparentValue, &value, sizeof(value));

	ExecutionContext ctx{rt, env};

	evm_result result;
	result.outcome = EVM_EXCEPTION;
	result.gas_left = 0;
	result.output_data = nullptr;
	result.output_size = 0;
	result.internal_memory = nullptr;

	auto codeIdentifier = makeCodeId(code_hash, mode);
	auto execFunc = jit.getExecFunc(codeIdentifier);
	if (!execFunc)
	{
		execFunc = jit.compile(mode, ctx.code(), ctx.codeSize(), codeIdentifier);
		if (!execFunc)
			return result;
		jit.mapExecFunc(codeIdentifier, execFunc);
	}

	auto returnCode = execFunc(&ctx);

	switch (returnCode)
	{
		case ReturnCode::Return:
		{
			auto out = ctx.getReturnData();
			result.output_data = std::get<0>(out);
			result.output_size = std::get<1>(out);
		}
		case ReturnCode::Stop:
			result.gas_left = rt.gas;
			result.outcome = EVM_SUCCESS;
			break;
		case ReturnCode:OutOfGas:
			result.outcome = EVM_OUT_OF_GAS;
			break;
		default:
			break;
	}

	// Take care of the internal memory.
	result.internal_memory = ctx.m_memData;
	ctx.m_memData = nullptr;

	return result;
}

static void release_result(evm_result const* result)
{
	if (result->internal_memory)
		ext_free(result->internal_memory);  // FIXME: Check what is ext_free about.
}

static int set_option(evm_instance* instance, char const* name,
	char const* value)
{
	(void)instance, (void)name, (void)value;
	return 0;
}

static evm_code_status get_code_status(evm_instance* instance,
	evm_mode mode, evm_hash256 code_hash)
{
	auto& jit = *reinterpret_cast<JITImpl*>(instance);
	auto codeIdentifier = makeCodeId(code_hash, mode);
	if (jit.getExecFunc(codeIdentifier) != nullptr)
		return EVM_READY;
	// TODO: Add support for EVM_CACHED.
	return EVM_UNKNOWN;
}

static void prepare_code(evm_instance* instance, evm_mode mode,
	unsigned char const* code, size_t code_size, evm_hash256 code_hash)
{
	auto& jit = *reinterpret_cast<JITImpl*>(instance);
	auto codeIdentifier = makeCodeId(code_hash, mode);
	auto execFunc = jit.compile(mode, code, code_size, codeIdentifier);
	if (execFunc) // FIXME: What with error?
		jit.mapExecFunc(codeIdentifier, execFunc);
}

EXPORT evm_interface evmjit_get_interface()
{
	return {create, destroy, execute, release_result, get_code_status, prepare_code, set_option};
}

}  // extern "C"
}
}
