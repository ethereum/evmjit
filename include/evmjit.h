#pragma once

#include <evm.h>

#ifdef _MSC_VER
#ifdef evmjit_EXPORTS
#define EXPORT __declspec(dllexport)
#else
#define EXPORT
#endif

#else
#define EXPORT __attribute__ ((visibility ("default")))
#endif

#if __cplusplus
extern "C" {
#endif

/// Create EVMJIT instance.
///
/// @return  The EVMJIT instance.
EXPORT struct evm_instance* evmjit_create(void);

#if __cplusplus
}
#endif
