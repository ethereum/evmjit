#include <stdio.h>
#include <evmjit.h>

int main()
{
    struct evmc_instance* instance = evmjit_create();
    if (EVMC_ABI_VERSION != instance->abi_version)
    {
        printf("Error: expected ABI version %u!\n", EVMC_ABI_VERSION);
        return 1;
    }
    printf("EVMJIT ABI version %u\n", instance->abi_version);
    return 0;
}
