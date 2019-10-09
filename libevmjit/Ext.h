#pragma once

#include <array>

#include "CompilerHelper.h"
#include "JIT.h"

namespace dev
{
namespace eth
{
namespace jit
{
/// The flag indicating call failure in evmc_call_fn() -- highest bit set.
constexpr int64_t EVM_CALL_FAILURE = 0x8000000000000000;

/// The hackish constant indicating EVM_CALL + EVM_STATIC flag.
constexpr int EVM_STATICCALL = EVMC_CREATE + 1;

class Memory;

struct MemoryRef
{
    llvm::Value* ptr;
    llvm::Value* size;
};

template <typename _EnumT>
struct sizeOf
{
    static const size_t value = static_cast<size_t>(_EnumT::_size);
};

enum class EnvFunc
{
    sload,
    sstore,
    sha3,
    balance,
    create,
    call,
    log,
    blockhash,
    extcode,

    _size
};

class Ext : public RuntimeHelper
{
public:
    Ext(RuntimeManager& _runtimeManager, Memory& _memoryMan);

    llvm::Value* sload(llvm::Value* _index);
    void sstore(llvm::Value* _index, llvm::Value* _value);

    llvm::Value* balance(llvm::Value* _address);
    llvm::Value* exists(llvm::Value* _address);
    llvm::Value* calldataload(llvm::Value* _index);
    std::tuple<llvm::Value*, llvm::Value*> create(
        llvm::Value* _gas, llvm::Value* _endowment, llvm::Value* _initOff, llvm::Value* _initSize);
    llvm::Value* blockHash(llvm::Value* _number);

    llvm::Value* sha3(llvm::Value* _inOff, llvm::Value* _inSize);
    MemoryRef extcode(llvm::Value* _addr);
    llvm::Value* extcodesize(llvm::Value* _addr);

    void log(llvm::Value* _memIdx, llvm::Value* _numBytes, llvm::ArrayRef<llvm::Value*> _topics);
    void selfdestruct(llvm::Value* _beneficiary);

    llvm::Value* call(int _kind, llvm::Value* _gas, llvm::Value* _addr, llvm::Value* _value,
        llvm::Value* _inOff, llvm::Value* _inSize, llvm::Value* _outOff, llvm::Value* _outSize);

private:
    Memory& m_memoryMan;

    llvm::Value* m_size;

    std::array<llvm::Function*, sizeOf<EnvFunc>::value> m_funcs;
    std::array<llvm::Value*, 8> m_argAllocas;
    size_t m_argCounter = 0;

    /// Memory for array of up to 4 log topics
    /// TODO: Merge this memory with args allocas.
    llvm::Value* m_topics = nullptr;

    llvm::CallInst* createCall(EnvFunc _funcId, std::initializer_list<llvm::Value*> const& _args);
    llvm::Value* getArgAlloca();

    llvm::Value* createCABICall(
        llvm::Function* _func, std::initializer_list<llvm::Value*> const& _args);
};


}  // namespace jit
}  // namespace eth
}  // namespace dev
