#pragma once

#include <evm.h>

#ifdef _MSC_VER
#ifdef evmjit_EXPORTS
#define EXPORT __declspec(dllexport)
#else
#define EXPORT __declspec(dllimport)
#endif

#else
#define EXPORT __attribute__ ((visibility ("default")))
#endif

#if __cplusplus
extern "C" {
#endif

/// Get EVMJIT instance.
///
/// @return  The EVMJIT instance.
EXPORT struct evm_factory evmjit_get_factory(void);

#if __cplusplus
}
#endif
