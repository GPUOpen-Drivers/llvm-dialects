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
