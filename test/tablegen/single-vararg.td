// RUN: llvm-dialects-tblgen -gen-dialect-decls -dialect=test -I%S/../../include %s 2>&1 | FileCheck --check-prefixes=CHECK %s

// CHECK:      #ifdef GET_INCLUDES

include "llvm-dialects/Dialect/Dialect.td"

def TestDialect : Dialect {
  let name = "test";
  let cppNamespace = "test";
}

def SingleVarArgOp : Op<TestDialect, "return", []> {
  let arguments = (ins varargs:$args);
  let results = (outs);

  let defaultBuilderHasExplicitResultType = true;

  let summary = "Has only a single variadic argument and no other arguments";
}
