// RUN: not --crash llvm-dialects-tblgen -gen-dialect-defs -dialect=test -I%S/../../include %s 2>&1 | FileCheck --check-prefixes=CHECK %s

// CHECK:      Operation result cannot be 'type'
// CHECK-NEXT: ... in: (outs type:$result)
// CHECK-NEXT: ... in operation MyReturnOp

include "llvm-dialects/Dialect/Dialect.td"

def TestDialect : Dialect {
  let name = "test";
  let cppNamespace = "test";
}

def MyReturnOp : Op<TestDialect, "return", []> {
  let arguments = (ins);
  let results = (outs type:$result); // <-- this should be 'value'

  let defaultBuilderHasExplicitResultType = true;

  let summary = "returns a value";
}
