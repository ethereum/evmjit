cmake_minimum_required(VERSION 3.0.0)

cmake_policy(SET CMP0042 OLD)	# Fix MACOSX_RPATH.
cmake_policy(SET CMP0048 NEW)	# Allow VERSION argument in project().
if (POLICY CMP0054)
	cmake_policy(SET CMP0054 NEW)	# No longer implicitly dereference variables.
endif()

project(EVMJIT VERSION 0.9.0.2 LANGUAGES CXX)

message(STATUS "EVM JIT ${EVMJIT_VERSION_MAJOR}.${EVMJIT_VERSION_MINOR}.${EVMJIT_VERSION_PATCH}")

set_property(GLOBAL PROPERTY USE_FOLDERS ON)
set(CMAKE_AUTOMOC OFF)

if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
	# Always use Release variant of C++ runtime.
	# We don't want to provide Debug variants of all dependencies. Some default
	# flags set by CMake must be tweaked.
	string(REPLACE "/MDd" "/MD" CMAKE_CXX_FLAGS_DEBUG ${CMAKE_CXX_FLAGS_DEBUG})
	string(REPLACE "/D_DEBUG" "" CMAKE_CXX_FLAGS_DEBUG ${CMAKE_CXX_FLAGS_DEBUG})
	string(REPLACE "/RTC1" "" CMAKE_CXX_FLAGS_DEBUG ${CMAKE_CXX_FLAGS_DEBUG})
	set_property(GLOBAL PROPERTY DEBUG_CONFIGURATIONS OFF)
else()
	set(CMAKE_CXX_FLAGS "-std=c++11 -Wall -Wextra -Wconversion -Wno-sign-conversion -Wno-unknown-pragmas ${CMAKE_CXX_FLAGS}")
endif()

if (CMAKE_SYSTEM_NAME STREQUAL "Linux" AND NOT SANITIZE)
	# Do not allow unresolved symbols in shared library (default on linux)
	# unless sanitizer is used (sanity checks produce unresolved function calls)
	set(CMAKE_SHARED_LINKER_FLAGS "-Wl,--no-undefined")
endif()

# LLVM
if (DEFINED APPLE)
	set(LLVM_DIR "/usr/local/opt/llvm37/lib/llvm-3.7/share/llvm/cmake")
endif()

find_package(LLVM REQUIRED CONFIG)
if (LLVM_VERSION VERSION_LESS 3.7)
	message(FATAL_ERROR "Incompatible LLVM version ${LLVM_VERSION}")
endif()
message(STATUS "Using LLVM ${LLVM_VERSION} (${LLVM_DIR})")
llvm_map_components_to_libnames(LLVM_LIBS core support mcjit x86asmparser x86codegen ipo)

add_subdirectory(libevmjit)
