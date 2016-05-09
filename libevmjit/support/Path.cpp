#include "Path.h"
#include <cstdlib>

#if UTILS_OS_WINDOWS
#include <ShlObj.h>
#endif

namespace dev
{
namespace path
{
	std::string user_cache_directory()
	{
		#if UTILS_OS_WINDOWS
			wchar_t* utf16Path = nullptr;
			if (SHGetKnownFolderPath(FOLDERID_LocalAppData, 0, nullptr, &utf16Path) != S_OK)
				return {};

			char utf8Path[4 * MAX_PATH];
			int len = WideCharToMultiByte(CP_UTF8, 0, utf16Path, lstrlenW(utf16Path), utf8Path, sizeof(utf8Path), nullptr, nullptr);
			CoTaskMemFree(utf16Path);
			return {utf8Path, static_cast<size_t>(len)};
		#else
			auto cache_home = std::getenv("XDG_CACHE_HOME");
			if (cache_home && *cache_home != '\0')
				return cache_home;

			auto home = std::getenv("HOME");
			if (home && *home != '\0')
			{
				auto suffix = UTILS_OS_MAC ? "/Library/Caches" : "/.cache";
				return std::string{home} + suffix;
			}
			return {};
		#endif
	}
}
}
