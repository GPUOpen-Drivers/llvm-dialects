##
 #######################################################################################################################
 # Copyright (c) 2020-2023 Advanced Micro Devices, Inc. All Rights Reserved.
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

set(LLVM_DIALECTS_TEST_DEPENDS FileCheck count not llvm-dialects-example)
add_custom_target(llvm-dialects-test-depends DEPENDS ${LLVM_DIALECTS_TEST_DEPENDS})
set_target_properties(llvm-dialects-test-depends PROPERTIES FOLDER "Tests")

# required by lit.site.cfg.py.in
set(LLVM_DIALECTS_TEST_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(LLVM_DIALECTS_TEST_BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR})

# required by configure_lit_site_cfg
set(LLVM_LIT_OUTPUT_DIR ${LLVM_TOOLS_BINARY_DIR})
configure_lit_site_cfg(
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.py.in
  ${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg.py
  MAIN_CONFIG
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.cfg.py
)

add_lit_testsuite(check-llvm-dialects-lit "Running the llvm-dialects regression tests"
  ${CMAKE_CURRENT_BINARY_DIR}
  ${exclude_from_check_all}
  DEPENDS ${LLVM_DIALECTS_TEST_DEPENDS}
)
set_target_properties(check-llvm-dialects-lit PROPERTIES FOLDER "Tests")

add_lit_testsuites(LLVM_DIALECTS ${CMAKE_CURRENT_SOURCE_DIR}
  ${exclude_from_check_all}
  DEPENDS ${LLVM_DIALECTS_TEST_DEPENDS}
)

add_subdirectory(unit)

# Alias for all relevant tests
add_custom_target(check-llvm-dialects
  DEPENDS check-llvm-dialects-lit check-llvm-dialects-units
)
