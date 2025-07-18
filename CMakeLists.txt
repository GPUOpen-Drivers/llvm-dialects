##
 #######################################################################################################################
 # Copyright (c) 2024 Advanced Micro Devices, Inc. All Rights Reserved.
 #
 # Licensed under the Apache License, Version 2.0 (the "License");
 # you may not use this file except in compliance with the License.
 # You may obtain a copy of the License at
 #
 #     http://www.apache.org/licenses/LICENSE-2.0
 #
 # Unless required by applicable law or agreed to in writing, software
 # distributed under the License is distributed on an "AS IS" BASIS,
 # WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 # See the License for the specific language governing permissions and
 # limitations under the License.
 #######################################################################################################################
 ##


cmake_minimum_required(VERSION 3.13.3)
project(llvm-dialects)

# Options intended for configuration by the user:
#  - CMAKE_BUILD_TYPE
#  - LLVM_ROOT
#
# The build system is intended to be usable both as an external subproject of
# the llvm-project build tree, and as a separate subdirectory / subproject.

message(STATUS "Begin configuring llvm-dialects...")

function(set_compiler_options PROJECT_NAME)
  # Output with color if in terminal: https://github.com/ninja-build/ninja/wiki/FAQ
  if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    target_compile_options("${PROJECT_NAME}" PRIVATE -fdiagnostics-color=always)
  elseif(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
      target_compile_options("${PROJECT_NAME}" PRIVATE -fcolor-diagnostics)
  endif()
endfunction()

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

set(LLVM_DIALECTS_TABLEGEN_SOURCES
    lib/TableGen/Common.cpp
    lib/TableGen/Constraints.cpp
    lib/TableGen/Dialects.cpp
    lib/TableGen/DialectType.cpp
    lib/TableGen/Evaluator.cpp
    lib/TableGen/Format.cpp
    lib/TableGen/GenDialect.cpp
    lib/TableGen/NamedValue.cpp
    lib/TableGen/Operations.cpp
    lib/TableGen/Predicates.cpp
    lib/TableGen/SymbolTable.cpp
    lib/TableGen/Traits.cpp
    )

set(llvm_deps
    Support
    TableGen
)

if (llvm_dialects_is_in_llvm_build_tree)
    add_llvm_library(llvm_dialects
        LINK_COMPONENTS
        Core
        Support
    )
    add_llvm_library(llvm_dialects_tablegen
        LINK_COMPONENTS
        ${llvm_deps}
    )

    set(PROJECT_NAME LLVM)
    set(LLVM_LINK_COMPONENTS Support)
    add_tablegen(llvm-dialects-tblgen LLVM_DIALECTS
        DESTINATION "${LLVM_TOOLS_INSTALL_DIR}"
        EXPORT LLVM
    )
    if(CMAKE_CROSSCOMPILING)
        set(LLVM_DIALECTS_TABLEGEN_EXE_HOST "${LLVM_DIALECTS_TABLEGEN_EXE}" CACHE
                STRING "Native llvm-dialects TableGen executable.")
        set(LLVM_DIALECTS_TABLEGEN_TARGET_HOST "llvm-dialects-tblgen-host" CACHE
                STRING "Native llvm-dialects TableGen target.")
    endif()

else()
    add_library(llvm_dialects)
    llvm_update_compile_flags(llvm_dialects)

    add_library(llvm_dialects_tablegen)
    llvm_update_compile_flags(llvm_dialects_tablegen)

    add_executable(llvm-dialects-tblgen)
    llvm_update_compile_flags(llvm-dialects-tblgen)

    llvm_map_components_to_libnames(llvm_libs ${llvm_deps})
    target_link_libraries(llvm-dialects-tblgen PRIVATE ${llvm_libs})
endif()

target_link_libraries(llvm-dialects-tblgen PRIVATE llvm_dialects_tablegen)
set_compiler_options(llvm-dialects-tblgen)

# The llvm_dialects library build depends on llvm/IR/Attributes.inc
add_dependencies(llvm_dialects intrinsics_gen)

target_include_directories(llvm_dialects PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>)

target_include_directories(llvm_dialects PRIVATE
    ${LLVM_INCLUDE_DIRS})

target_sources(llvm_dialects PRIVATE
    lib/Dialect/Builder.cpp
    lib/Dialect/ContextExtension.cpp
    lib/Dialect/Dialect.cpp
    lib/Dialect/OpDescription.cpp
    lib/Dialect/Utils.cpp
    lib/Dialect/Verifier.cpp
    lib/Dialect/Visitor.cpp)

target_include_directories(llvm_dialects_tablegen PRIVATE
    include
    ${LLVM_INCLUDE_DIRS})

target_sources(llvm_dialects_tablegen PRIVATE ${LLVM_DIALECTS_TABLEGEN_SOURCES})

target_include_directories(llvm-dialects-tblgen PRIVATE
    include
    ${LLVM_INCLUDE_DIRS})

target_sources(llvm-dialects-tblgen PRIVATE
    utils/llvm-dialects-tblgen.cpp)

add_subdirectory(example EXCLUDE_FROM_ALL)
add_subdirectory(test EXCLUDE_FROM_ALL)

message(STATUS "End configuring llvm-dialects")
