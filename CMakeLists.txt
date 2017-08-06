# Minimum required version of CMake
cmake_minimum_required(VERSION 3.0.2)

# Set CMake project name
project(BIGZ)

set(VERSION 1.6.2)

option(USE_PGO OFF "Profile Guided Optimization")
#fprofile-generate fprofile-use

include(CheckTypeSize)
check_type_size(int SIZEOF_INT)
check_type_size(long SIZEOF_LONG)
check_type_size(size_t SIZEOF_SIZE_T)
set(SIZEOF_VOID_P "${CMAKE_SIZEOF_VOID_P}")

include(CheckIncludeFiles)
check_include_files(string.h HAVE_STRING_H)
check_include_files(stdint.h HAVE_STDINT_H)

configure_file(config.h.in ${CMAKE_CURRENT_BINARY_DIR}/config.h)
add_definitions(-DHAVE_CONFIG_H=1)
include_directories(${CMAKE_CURRENT_BINARY_DIR})

add_library(bigz SHARED bignum.c)
add_library(bigz-static STATIC bignum.c)

include(CheckCCompilerFlag)
function(add_flag target flag)
	check_c_compiler_flag(-${flag} HAVE_${flag})
	if(HAVE_${flag})
		target_compile_options(${target} PRIVATE -${flag})
	endif(HAVE_${flag})
endfunction(add_flag flag)

# Optimization and Warnings options
set(tflags m64 O3 Wall Wextra Wshadow Wunused Wunused-value
	Wunused-variable Wunused-parameter Wunused-label Wunused-function
	fomit-frame-pointer)
foreach(tf ${tflags})
	add_flag(bigz ${tf})
endforeach(tf ${tflags})

if(NOT OSX)
	set(nflags finline-functions finline-limit=4096)
	foreach(nf ${nflags})
		add_flag(bigz ${nf})
	endforeach(nf ${nflags})
endif(NOT OSX)

