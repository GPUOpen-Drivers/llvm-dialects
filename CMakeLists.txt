cmake_minimum_required(VERSION 3.13.3)
project(llvm-dialects)

# Options intended for configuration by the user:
#  - CMAKE_BUILD_TYPE
#  - LLVM_DIR
#
# The build system is intended to be usable both as an external subproject of
# the llvm-project build tree, and as a separate subdirectory / subproject.

message(STATUS "Begin configuring llvm-dialects...")

if(NOT DEFINED LLVM_VERSION_MAJOR)
    find_package(LLVM REQUIRED CONFIG)

    list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
    include(AddLLVM)
    include(TableGen)

    message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
    message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

    set(llvm_dialects_is_in_llvm_build_tree OFF)
else()
    message(STATUS "Assuming build as external project for LLVM ${LLVM_VERSION_MAJOR}.${LLVM_VERSION_MINOR} in same build tree")

    set(LLVM_INCLUDE_DIRS ${LLVM_MAIN_INCLUDE_DIR} ${LLVM_INCLUDE_DIR})

    set(llvm_dialects_is_in_llvm_build_tree ON)
endif()

message(STATUS "LLVM includes: ${LLVM_INCLUDE_DIRS}")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})
add_definitions("-DHAVE_LLVM_VERSION_MAJOR=${LLVM_VERSION_MAJOR}")

if (llvm_dialects_is_in_llvm_build_tree)
    add_llvm_library(llvm_dialects)

    add_llvm_library(llvm_dialects_tablegen)

    set(LLVM_LINK_COMPONENTS Support TableGen)
    add_llvm_tool(llvm-dialects-tblgen DISABLE_LLVM_LINK_LLVM_DYLIB)
    target_link_libraries(llvm-dialects-tblgen PRIVATE llvm_dialects_tablegen)
else()
    add_library(llvm_dialects)
    llvm_update_compile_flags(llvm_dialects)

    add_library(llvm_dialects_tablegen)
    llvm_update_compile_flags(llvm_dialects_tablegen)

    add_executable(llvm-dialects-tblgen)
    llvm_update_compile_flags(llvm-dialects-tblgen)

    llvm_map_components_to_libnames(llvm_libs Support TableGen)
    target_link_libraries(llvm-dialects-tblgen PRIVATE llvm_dialects_tablegen ${llvm_libs})
endif()

target_include_directories(llvm_dialects PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>)

target_include_directories(llvm_dialects PRIVATE
    ${LLVM_INCLUDE_DIRS})

target_sources(llvm_dialects PRIVATE
    lib/Dialect/Builder.cpp
    lib/Dialect/Dialect.cpp
    lib/Dialect/OpDescription.cpp
    lib/Dialect/Utils.cpp
    lib/Dialect/Visitor.cpp)

target_include_directories(llvm_dialects_tablegen PRIVATE
    include
    ${LLVM_INCLUDE_DIRS})

target_sources(llvm_dialects_tablegen PRIVATE
    lib/TableGen/Common.cpp
    lib/TableGen/Format.cpp
    lib/TableGen/GenDialect.cpp)

target_include_directories(llvm-dialects-tblgen PRIVATE
    include
    ${LLVM_INCLUDE_DIRS})

target_sources(llvm-dialects-tblgen PRIVATE
    utils/llvm-dialects-tblgen.cpp)

message(STATUS "End configuring llvm-dialects")
