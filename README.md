# The Ethereum EVM JIT

[![Join the chat at https://gitter.im/ethereum/evmjit](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/ethereum/evmjit?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![GitPOAP Badge](https://public-api.gitpoap.io/v1/repo/ethereum/evmjit/badge)](https://www.gitpoap.io/gh/ethereum/evmjit)

EVM JIT is a library for just-in-time compilation of Ethereum EVM code.
It can be used to substitute classic interpreter-like EVM Virtual Machine in Ethereum client.

## Maintainers

**NOTE: This project is not maintained. Do not use it for anything important.**

Looking for maintainers! [Please state your interest here.](https://github.com/ethereum/evmjit/issues/184)

## Build

The EVMJIT project uses **CMake** tool to configure the build and depends only on the LLVM library.
LLVM installation is not needed, because CMake will download and build LLVM from source.
However, LLVM requires **Python** interpreter to be built.

```sh
git submodule update --init --recursive
mkdir build
cd build
cmake ..
cmake --build . --config RelWithDebInfo
```

## Options

Options to evmjit library can be passed by environmental variable, e.g. `EVMJIT="-help" testeth --jit`.
