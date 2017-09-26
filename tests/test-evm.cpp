#include <evm.h>

evm_result test_call_failure(int64_t gas)
{
    evm_result result{};
    result.status_code = EVM_FAILURE;
    result.gas_left = gas;
    return result;
}
