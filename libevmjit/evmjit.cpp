#include "evmjit/JIT.h"

static_assert(sizeof(evm_uint256) == 32, "evm_uint256 is too big");
static_assert(sizeof(evm_hash256) == 32, "evm_hash256 is too big");
static_assert(sizeof(evm_hash160) == 20, "evm_hash160 is too big");
static_assert(sizeof(evm_result) == 32, "evm_result is too big");

// Check enums match int size.
// On GCC/clang the underlying type should be unsigned int, on MSVC int
static_assert(sizeof(evm_query_key)  == sizeof(int), "Enum `evm_query_key` is not the size of int");
static_assert(sizeof(evm_update_key) == sizeof(int), "Enum `evm_update_key` is not the size of int");
static_assert(sizeof(evm_call_kind)  == sizeof(int), "Enum `evm_call_kind` is not the size of int");
static_assert(sizeof(evm_info_key)   == sizeof(int), "Enum `evm_info_key` is not the size of int");
static_assert(sizeof(evm_mode)       == sizeof(int), "Enum `evm_mode` is not the size of int");

#ifndef _MSC_VER  // alignof is broken on MSVC in static_assert context.
static_assert(alignof(evm_hash256) == 8, "");
#endif

extern "C"
{

EXPORT char const* evm_get_info(evm_info_key key)
{
    switch (key)
    {
    case EVM_NAME:    return "evmjit";
    case EVM_VERSION: return "0.9.0";
    }
    return "";
}

}  // extern "C"


