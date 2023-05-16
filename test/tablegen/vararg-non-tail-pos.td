// RUN: not --crash llvm-dialects-tblgen -gen-dialect-defs -dialect=test -I%S/../../include %s 2>&1 | FileCheck --check-prefixes=CHECK %s

// CHECK:      No other argument can follow a definition of 'varargs'
// CHECK-NEXT: ... in operation VarArgNoneTailOp
// CHECK-NEXT: LLVM ERROR: error parsing operation

include "llvm-dialects/Dialect/Dialect.td"

def TestDialect : Dialect {
  let name = "test";
  let cppNamespace = "test";
}

def VarArgNoneTailOp : Op<TestDialect, "return", []> {
  let arguments = (ins varargs:$args, value:$data);
  let results = (outs);

  let defaultBuilderHasExplicitResultType = true;

  let summary = "Has a variadic argument before the tail of the argument list";
}
