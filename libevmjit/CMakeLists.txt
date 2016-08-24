get_filename_component(EVMJIT_INCLUDE_DIR ../include ABSOLUTE)

set(SOURCES
	evmjit.cpp			${EVMJIT_INCLUDE_DIR}/evm.h
	JIT.cpp				${EVMJIT_INCLUDE_DIR}/evmjit/JIT.h
	Arith256.cpp		Arith256.h
	Array.cpp			Array.h
	BasicBlock.cpp		BasicBlock.h
	Cache.cpp			Cache.h
						Common.h
	Compiler.cpp		Compiler.h
	CompilerHelper.cpp	CompilerHelper.h
	Endianness.cpp		Endianness.h
	ExecStats.cpp		ExecStats.h
	Ext.cpp				Ext.h
	GasMeter.cpp		GasMeter.h
	Instruction.cpp		Instruction.h
	Memory.cpp			Memory.h
	Optimizer.cpp		Optimizer.h
	RuntimeManager.cpp	RuntimeManager.h
	Type.cpp			Type.h
	Utils.cpp			Utils.h
)
source_group("" FILES ${SOURCES})

if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
else()
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti -fvisibility=hidden")
	if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
		set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,--exclude-libs,ALL") # Do not export symbols from dependies, mostly LLVM libs
	endif()
endif()

if(EVMJIT_VERSION_MAJOR EQUAL 0)
	set(EVMJIT_SOVERSION "0.${EVMJIT_VERSION_MINOR}")
else()
	set(EVMJIT_SOVERSION ${EVMJIT_VERSION_MAJOR})
endif()


string(COMPARE EQUAL "${LLVM_ENABLE_ASSERTIONS}" "ON" LLVM_DEBUG)
configure_file(BuildInfo.h.in ${CMAKE_CURRENT_BINARY_DIR}/gen/BuildInfo.gen.h)

add_library(evmjit-objs OBJECT ${SOURCES} gen/BuildInfo.gen.h)
target_include_directories(evmjit-objs PUBLIC ${EVMJIT_INCLUDE_DIR})
target_include_directories(evmjit-objs PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/gen)
set_target_properties(evmjit-objs PROPERTIES POSITION_INDEPENDENT_CODE On)

add_library(evmjit-static STATIC $<TARGET_OBJECTS:evmjit-objs>)
target_link_libraries(evmjit-static PRIVATE llvm)

add_library(evmjit SHARED $<TARGET_OBJECTS:evmjit-objs>)
set_target_properties(evmjit PROPERTIES
		VERSION ${EVMJIT_VERSION} SOVERSION ${EVMJIT_SOVERSION}
		FOLDER "libs")
target_link_libraries(evmjit PRIVATE llvm)

install(TARGETS evmjit LIBRARY DESTINATION lib ARCHIVE DESTINATION lib RUNTIME DESTINATION bin)
install(DIRECTORY ${EVMJIT_INCLUDE_DIR} DESTINATION include)
