#include "JIT.h"

#include <cstddef>
#include <mutex>

#include "preprocessor/llvm_includes_start.h"
#include <llvm/IR/Module.h>
#include <llvm/ADT/StringSwitch.h>
#include <llvm/ADT/Triple.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/Support/TargetSelect.h>
#include <evm.h>
#include "preprocessor/llvm_includes_end.h"

#include "Ext.h"
#include "Compiler.h"
#include "Optimizer.h"
#include "Cache.h"
#include "ExecStats.h"
#include "Utils.h"
#include "BuildInfo.gen.h"


static_assert(sizeof(evm_uint256be) == 32, "evm_uint256be is too big");
static_assert(sizeof(evm_uint160be) == 20, "evm_uint160be is too big");
static_assert(sizeof(evm_result) == 64, "evm_result does not fit cache line");
static_assert(sizeof(evm_message) <= 18*8, "evm_message not optimally packed");
static_assert(offsetof(evm_message, code_hash) % 8 == 0, "evm_message.code_hash not aligned");

// Check enums match int size.
// On GCC/clang the underlying type should be unsigned int, on MSVC int
static_assert(sizeof(evm_query_key)  == sizeof(int), "Enum `evm_query_key` is not the size of int");
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
	case EVM_METROPOLIS: return 'M';
	}
	LLVM_BUILTIN_UNREACHABLE;
}

/// Combine code hash and compatibility mode into a printable code identifier.
std::string makeCodeId(evm_uint256be codeHash, evm_mode mode, uint32_t flags)
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
	if (flags & EVM_STATIC)
		str.push_back('S');
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

class JITImpl: public evm_instance
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

	ExecFunc compile(evm_mode _mode, bool _staticCall, byte const* _code, uint64_t _codeSize, std::string const& _codeIdentifier);

	evm_query_state_fn queryFn = nullptr;
	evm_get_storage_fn getStorageFn = nullptr;
	evm_set_storage_fn setStorageFn = nullptr;
	evm_selfdestruct_fn selfdestructFn = nullptr;
	evm_call_fn callFn = nullptr;
	evm_get_tx_context_fn getTxContextFn = nullptr;
	evm_get_block_hash_fn getBlockHashFn = nullptr;
	evm_log_fn logFn = nullptr;

	evm_message const* currentMsg = nullptr;
	std::vector<uint8_t> returnBuffer;
};

static int64_t call_v2(
	evm_env* _opaqueEnv,
	evm_call_kind _kind,
	int64_t _gas,
	evm_uint160be const* _address,
	evm_uint256be const* _value,
	uint8_t const* _inputData,
	size_t _inputSize,
	uint8_t* _outputData,
	size_t _outputSize,
	uint8_t const** o_bufData,
	size_t* o_bufSize
) noexcept
{
	auto& jit = JITImpl::instance();

	evm_message msg;
	msg.kind = _kind;
	msg.address = *_address;
	msg.sender = _kind != EVM_DELEGATECALL ? jit.currentMsg->address : jit.currentMsg->sender;
	msg.value = _kind != EVM_DELEGATECALL ? *_value : jit.currentMsg->value;
	msg.input = _inputData;
	msg.input_size = _inputSize;
	msg.gas = _gas;
	msg.depth = jit.currentMsg->depth + 1;
	msg.flags = jit.currentMsg->flags;
	if (_kind == EVM_STATICCALL)
	{
		msg.kind = EVM_CALL;
		msg.flags |= EVM_STATIC;
	}

	// FIXME: Handle code hash.
	evm_result result;
	jit.callFn(&result, _opaqueEnv, &msg);
	// FIXME: Clarify when gas_left is valid.
	int64_t r = result.gas_left;
	if (result.code == EVM_SUCCESS || result.code == EVM_REVERT)
	{
		auto size = std::min(_outputSize, result.output_size);
		std::copy(result.output_data, result.output_data + size, _outputData);
		jit.returnBuffer = {result.output_data, result.output_data + result.output_size};
	}
	else
		jit.returnBuffer.clear();

	*o_bufData = jit.returnBuffer.data();
	*o_bufSize = jit.returnBuffer.size();

	if (result.code != EVM_SUCCESS)
		r |= EVM_CALL_FAILURE;

	if (result.release)
		result.release(&result);
	return r;
}


class SymbolResolver : public llvm::SectionMemoryManager
{
	llvm::RuntimeDyld::SymbolInfo findSymbol(std::string const& _name) override
	{
		auto& jit = JITImpl::instance();
		auto addr = llvm::StringSwitch<uint64_t>(_name)
			.Case("env_sha3", reinterpret_cast<uint64_t>(&keccak))
			.Case("evm.query", reinterpret_cast<uint64_t>(jit.queryFn))
			.Case("evm.sload", reinterpret_cast<uint64_t>(jit.getStorageFn))
			.Case("evm.sstore", reinterpret_cast<uint64_t>(jit.setStorageFn))
			.Case("evm.selfdestruct", reinterpret_cast<uint64_t>(jit.selfdestructFn))
			.Case("evm.call", reinterpret_cast<uint64_t>(call_v2))
			.Case("evm.get_tx_context", reinterpret_cast<uint64_t>(jit.getTxContextFn))
			.Case("evm.blockhash", reinterpret_cast<uint64_t>(jit.getBlockHashFn))
			.Case("evm.log", reinterpret_cast<uint64_t>(jit.logFn))
			.Default(0);
		if (addr)
			return {addr, llvm::JITSymbolFlags::Exported};

		// Fallback to default implementation that would search for the symbol
		// in the current process.
		// TODO: In the future we should control the whole set of requested
		//       symbols (like memcpy, memset, etc) to improve performance.
		return llvm::SectionMemoryManager::findSymbol(_name);
	}

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

ExecFunc JITImpl::compile(evm_mode _mode, bool _staticCall, byte const* _code, uint64_t _codeSize,
	std::string const& _codeIdentifier)
{
	auto module = Cache::getObject(_codeIdentifier, getLLVMContext());
	if (!module)
	{
		// TODO: Listener support must be redesigned. These should be a feature of JITImpl
		//listener->stateChanged(ExecState::Compilation);
		assert(_code || !_codeSize);
		//TODO: Can the Compiler be stateless?
		module = Compiler({}, _mode, _staticCall, getLLVMContext()).compile(_code, _code + _codeSize, _codeIdentifier);

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


ExecutionContext::~ExecutionContext() noexcept
{
	if (m_memData)
		std::free(m_memData);
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

static evm_instance* create(
	evm_query_state_fn queryFn,
	evm_get_storage_fn getStorageFn,
	evm_set_storage_fn setStorageFn,
	evm_selfdestruct_fn selfdestructFn,
	evm_call_fn callFn,
	evm_get_tx_context_fn getTxContextFn,
	evm_get_block_hash_fn getBlockHashFn,
	evm_log_fn logFn
)
{
	// Let's always return the same instance. It's a bit of faking, but actually
	// this might be a compliant implementation.
	auto& jit = JITImpl::instance();
	jit.queryFn = queryFn;
	jit.getStorageFn = getStorageFn;
	jit.setStorageFn = setStorageFn;
	jit.selfdestructFn = selfdestructFn;
	jit.callFn = callFn;
	jit.getTxContextFn = getTxContextFn;
	jit.getBlockHashFn = getBlockHashFn;
	jit.logFn = logFn;
	return &jit;
}

static void destroy(evm_instance* instance)
{
	(void)instance;
	assert(instance == static_cast<void*>(&JITImpl::instance()));
}

static evm_result execute(evm_instance* instance, evm_env* env, evm_mode mode,
	evm_message const* msg, uint8_t const* code, size_t code_size)
{
	auto& jit = *reinterpret_cast<JITImpl*>(instance);

	// TODO: Temporary keep track of the current message.
	evm_message const* prevMsg = jit.currentMsg;
	jit.currentMsg = msg;

	RuntimeData rt;
	rt.code = code;
	rt.codeSize = code_size;
	rt.gas = msg->gas;
	rt.callData = msg->input;
	rt.callDataSize = msg->input_size;
	std::memcpy(&rt.apparentValue, &msg->value, sizeof(msg->value));
	std::memset(&rt.address, 0, 12);
	std::memcpy(&rt.address[12], &msg->address, sizeof(msg->address));
	std::memset(&rt.caller, 0, 12);
	std::memcpy(&rt.caller[12], &msg->sender, sizeof(msg->sender));
	rt.depth = msg->depth;

	ExecutionContext ctx{rt, env};

	evm_result result;
	result.code = EVM_SUCCESS;
	result.gas_left = 0;
	result.output_data = nullptr;
	result.output_size = 0;
	result.release = nullptr;

	auto codeIdentifier = makeCodeId(msg->code_hash, mode, msg->flags);
	auto execFunc = jit.getExecFunc(codeIdentifier);
	if (!execFunc)
	{
		bool const staticCall = (msg->flags & EVM_STATIC) != 0;
		execFunc = jit.compile(mode, staticCall, ctx.code(), ctx.codeSize(), codeIdentifier);
		if (!execFunc)
			return result;
		jit.mapExecFunc(codeIdentifier, execFunc);
	}

	auto returnCode = execFunc(&ctx);

	if (returnCode == ReturnCode::Revert)
	{
		result.code = EVM_REVERT;
		result.gas_left = rt.gas;
	}
	else if (returnCode == ReturnCode::OutOfGas)
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

	if (returnCode == ReturnCode::Return || returnCode == ReturnCode::Revert)
	{
		auto out = ctx.getReturnData();
		result.output_data = std::get<0>(out);
		result.output_size = std::get<1>(out);
	}

	// Take care of the internal memory.
	if (ctx.m_memData)
	{
		// Use result's reserved data to store the memory pointer.
		result.reserved.context = ctx.m_memData;

		// Set pointer to the destructor that will release the memory.
		result.release = [](evm_result const* result)
		{
			std::free(result->reserved.context);
		};
		ctx.m_memData = nullptr;
	}

	jit.currentMsg = prevMsg;
	return result;
}

static int set_option(evm_instance* instance, char const* name,
	char const* value)
{
	(void)instance, (void)name, (void)value;
	return 0;
}

static evm_code_status get_code_status(evm_instance* instance,
	evm_mode mode, uint32_t flags, evm_uint256be code_hash)
{
	auto& jit = *reinterpret_cast<JITImpl*>(instance);
	auto codeIdentifier = makeCodeId(code_hash, mode, flags);
	if (jit.getExecFunc(codeIdentifier) != nullptr)
		return EVM_READY;
	// TODO: Add support for EVM_CACHED.
	return EVM_UNKNOWN;
}

static void prepare_code(evm_instance* instance, evm_mode mode, uint32_t flags,
	evm_uint256be code_hash, unsigned char const* code, size_t code_size)
{
	auto& jit = *reinterpret_cast<JITImpl*>(instance);
	auto codeIdentifier = makeCodeId(code_hash, mode, flags);
	bool const staticCall = (flags & EVM_STATIC) != 0;
	auto execFunc = jit.compile(mode, staticCall, code, code_size, codeIdentifier);
	if (execFunc) // FIXME: What with error?
		jit.mapExecFunc(codeIdentifier, execFunc);
}

EXPORT evm_factory evmjit_get_factory()
{
	return {EVM_ABI_VERSION, create};
}

}  // extern "C"

JITImpl::JITImpl():
		evm_instance({evmjit::destroy,
		              evmjit::execute,
		              evmjit::get_code_status,
		              evmjit::prepare_code,
		              evmjit::set_option})
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

}
}
