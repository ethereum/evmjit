#pragma once

#include <cstdint>
#include <iostream>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/Twine.h>

namespace dev {
namespace evmjit {

void keccak(uint8_t const *_data, uint64_t _size, uint8_t *o_hash);
bool userCacheDirectory(llvm::SmallVectorImpl<char> &_result,
													const llvm::Twine &_path1,
													const llvm::Twine &Path2 = "",
													const llvm::Twine &Path3 = "");


// The same as assert, but expression is always evaluated and result returned
#define CHECK(expr) (assert(expr), expr)

#if !defined(NDEBUG) // Debug

std::ostream &getLogStream(char const *_channel);

#define DLOG(CHANNEL) ::dev::evmjit::getLogStream(#CHANNEL)

#else // Release

struct Voider
{
    void operator=(std::ostream const&) {}
};

#define DLOG(CHANNEL) true ? (void)0 : ::dev::evmjit::Voider{} = std::cerr

#endif

}
}
