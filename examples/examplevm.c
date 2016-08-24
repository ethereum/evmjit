#include <stdlib.h>
#include <string.h>
#include "evm.h"

struct evm_instance {
    evm_query_fn query_fn;
    evm_update_fn update_fn;
    evm_call_fn call_fn;
};

EXPORT char const* evm_get_info(enum evm_info_key key)
{
  switch(key) {
    case EVM_NAME: return "ExampleVM"; break;
    case EVM_VERSION: return "git"; break;
  }

  return "";
}

static struct evm_instance* evm_create(evm_query_fn query_fn,
                                       evm_update_fn update_fn,
                                       evm_call_fn call_fn)
{
    struct evm_instance *ret = calloc(1, sizeof(struct evm_instance));

    if (ret) {
      ret->query_fn = query_fn;
      ret->update_fn = update_fn;
      ret->call_fn = call_fn;
    }

    return ret;
}

static void evm_destroy(struct evm_instance* evm)
{
    free(evm);
}

/// Example options.
///
/// VMs are allowed to omit this function implementation.
int evm_set_option(struct evm_instance* evm,
                   char const* name,
                   char const* value)
{
    if (strcmp(name, "example-option") == 0)
        return 1;
    return 0;
}

static struct evm_result evm_execute(struct evm_instance* instance,
                                     struct evm_env* env,
                                     enum evm_mode mode,
                                     struct evm_hash256 code_hash,
                                     uint8_t const* code,
                                     size_t code_size,
                                     int64_t gas,
                                     uint8_t const* input,
                                     size_t input_size,
                                     struct evm_uint256 value)
{
    struct evm_result ret;

    memset(&ret, 0, sizeof(struct evm_result));

    // Execute code and refer to callbacks: instance->query_fn()

    ret.outcome = EVM_EXCEPTION;
    ret.gas_left = 0;

    return ret;
}

static void evm_release_result(struct evm_result const* result)
{
}

EXPORT struct evm_interface examplevm_get_interface()
{
    struct evm_interface intf;
    memset(&intf, 0, sizeof(struct evm_result));
    intf.create = evm_create;
    intf.destroy = evm_destroy;
    intf.execute = evm_execute;
    intf.release_result = evm_release_result;
    intf.set_option = evm_set_option;
    return intf;
}