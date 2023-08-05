// RUN: llvm-dialects-tblgen -gen-dialect-defs -dialect=test -I%S/../../include %s 2>&1 | FileCheck --check-prefixes=CHECK %s

// Only check that the generation runs through without error.

// CHECK: #endif // GET_DIALECT_DEFS

include "llvm-dialects/Dialect/Dialect.td"

def TestDialect : Dialect {
  let name = "test";
  let cppNamespace = "test";
}

def TestOp : Op<TestDialect, "test", []> {
  let results = (outs);
  let arguments = (ins value:$x);

  let verifier = [
    (or (I32 $x), (FixedVectorType $x, I32, any)),
  ];

  let summary = "add two integers or vectors of integers";
  let description = [{
    Demonstrates that a TgConstant can be used inside of a logic or constraint
    and won't be treated as a free variable.
  }];
}
