
### TableGen for the test dialect

set(TEST_TABLEGEN_EXE ${LLVM_TOOLS_BINARY_DIR}/llvm-dialects-tblgen)
set(TEST_TABLEGEN_TARGET llvm-dialects-tblgen)
set(LLVM_TARGET_DEFINITIONS TestDialect.td)

tablegen(TEST TestDialect.h.inc -gen-dialect-decls --dialect test
    EXTRA_INCLUDES ${CMAKE_CURRENT_SOURCE_DIR}/../../../include)
tablegen(TEST TestDialect.cpp.inc -gen-dialect-defs --dialect test
    EXTRA_INCLUDES ${CMAKE_CURRENT_SOURCE_DIR}/../../../include)
add_public_tablegen_target(TestDialectTableGen)
