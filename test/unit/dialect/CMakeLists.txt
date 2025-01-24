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



### TableGen for the test dialect

set(TEST_TABLEGEN_EXE ${LLVM_TOOLS_BINARY_DIR}/llvm-dialects-tblgen)
set(TEST_TABLEGEN_TARGET llvm-dialects-tblgen)
set(LLVM_TARGET_DEFINITIONS TestDialect.td)

tablegen(TEST TestDialect.h.inc -gen-dialect-decls --dialect test
    EXTRA_INCLUDES ${CMAKE_CURRENT_SOURCE_DIR}/../../../include)
tablegen(TEST TestDialect.cpp.inc -gen-dialect-defs --dialect test
    EXTRA_INCLUDES ${CMAKE_CURRENT_SOURCE_DIR}/../../../include)
add_public_tablegen_target(TestDialectTableGen)
