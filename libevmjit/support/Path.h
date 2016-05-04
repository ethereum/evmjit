#pragma once

#include <string>

#if __APPLE__
	#define UTILS_OS_MAC 1
#else
	#define UTILS_OS_MAC 0
#endif

#if defined(_WIN32)
	#define UTILS_OS_WINDOWS 1
#else
	#define UTILS_OS_WINDOWS 0
#endif

namespace dev
{
namespace path
{
	std::string user_cache_directory();
}
}
