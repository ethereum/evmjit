#pragma once

#ifdef _MSC_VER
#ifdef evmjit_EXPORTS
#define EXPORT __declspec(dllexport)
#else
#define EXPORT __declspec(dllimport)
#endif

#else
#define EXPORT [[gnu::visibility("default")]]
#endif

// FIXME: evm.h uses EXPORT temporary.
#include <evm.h>

#if __cplusplus
extern "C" {
#endif

/// Get EVMJIT's function table for EVM-C interface.
///
/// @return  EVMJIT's function table.
EXPORT struct evm_fn_table evmjit_get_fn_table();

#if __cplusplus
}
#endif