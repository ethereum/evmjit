#include "evm.h"

static_assert(sizeof(evm_uint256) == 32, "evm_uint256 is too big");
static_assert(sizeof(evm_hash256) == 32, "evm_hash256 is too big");
static_assert(sizeof(evm_hash160) == 20, "evm_hash160 is too big");
static_assert(sizeof(evm_result) == 32 + 8, "evm_result is too big");

#ifndef _MSC_VER
static_assert(alignof(evm_hash256) == 8, "");
#endif

char const* evm_get_info(enum evm_info_key key)
{
    switch (key)
    {
    case EVM_NAME:    return "evmjit";
    case EVM_VERSION: return "0.9.0";
    }
    return "";
}
