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


add_custom_target(DialectsUnitTests)
set_target_properties(DialectsUnitTests PROPERTIES FOLDER "Dialects Unit Tests")

llvm_map_components_to_libnames(llvm_libs Support Core)

# Inputs for lit.cfg.site.py.in
set(DIALECTS_UNIT_TEST_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(DIALECTS_UNIT_TEST_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})

function(add_dialects_unit_test test_folder)
    add_unittest(DialectsUnitTests ${test_folder} ${ARGN})
    target_link_libraries(${test_folder} PRIVATE ${llvm_libs} llvm_dialects)

    # Link to the generated dialect sources
    target_sources(${test_folder}
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/../dialect/TestDialect.cpp)
        
    target_include_directories(${test_folder}
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/../dialect
        ${CMAKE_CURRENT_BINARY_DIR}/../dialect)
endfunction()

add_subdirectory(dialect)
add_subdirectory(interface)

# Let lit discover the GTest tests
configure_lit_site_cfg(
    ${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.py.in
    ${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg.py
    MAIN_CONFIG
    ${CMAKE_CURRENT_SOURCE_DIR}/lit.cfg.py
)

add_lit_testsuite(check-llvm-dialects-units "Running the llvm-dialects unit tests"
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${EXCLUDE_FROM_CHECK_ALL}
        DEPENDS TestDialectTableGen DialectsUnitTests)
