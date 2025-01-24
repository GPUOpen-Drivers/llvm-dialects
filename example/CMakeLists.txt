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



### Example application

add_executable(llvm-dialects-example
    ExampleDialect.cpp
    ExampleMain.cpp)
llvm_update_compile_flags(llvm-dialects-example)

target_include_directories(llvm-dialects-example
    PRIVATE
    ${CMAKE_CURRENT_BINARY_DIR})

llvm_map_components_to_libnames(llvm_libs AsmParser Support Core)
target_link_libraries(llvm-dialects-example
    PRIVATE
    llvm_dialects
    ${llvm_libs})
set_compiler_options(llvm-dialects-example)

### TableGen for the Example dialect

set(EXAMPLE_TABLEGEN_EXE llvm-dialects-tblgen)
set(EXAMPLE_TABLEGEN_TARGET llvm-dialects-tblgen)
set(LLVM_TARGET_DEFINITIONS ExampleDialect.td)

tablegen(EXAMPLE ExampleDialect.h.inc -gen-dialect-decls --dialect xd.ir
    EXTRA_INCLUDES ${CMAKE_CURRENT_SOURCE_DIR}/../include)
tablegen(EXAMPLE ExampleDialect.cpp.inc -gen-dialect-defs --dialect xd.ir
    EXTRA_INCLUDES ${CMAKE_CURRENT_SOURCE_DIR}/../include)
add_public_tablegen_target(ExampleDialectTableGen)

add_dependencies(llvm-dialects-example ExampleDialectTableGen)
