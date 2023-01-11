cmake_minimum_required(VERSION 3.19)
project(scummvm-tools CXX)
include(CheckCXXCompilerFlag)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if (WIN32)
	add_definitions(-DWIN32)
else()
	add_definitions(-DPOSIX)
endif()

find_package(SDL REQUIRED)

function(add_tool NAME)
    add_executable(${NAME} ${ARGN})
    target_include_directories(${NAME} PUBLIC .)
    target_link_libraries(${NAME} SDL::SDL)
endfunction()

set(COMMON_SRC
    common/file.cpp
    common/hashmap.cpp
    common/md5.cpp
    common/memorypool.cpp
    common/str.cpp
    common/stream.cpp
    common/util.cpp
    sound/adpcm.cpp
    sound/audiostream.cpp
    sound/voc.cpp
    sound/wave.cpp
)

set(DETWINE_SRC
    ${COMMON_SRC}
    engines/twine/detwine.cpp
    engines/twine/lba1.cpp
    engines/twine/lba2.cpp
    engines/twine/hqr.cpp
)
add_tool(detwine ${DETWINE_SRC})
