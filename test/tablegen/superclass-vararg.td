// RUN: not --crash llvm-dialects-tblgen -gen-dialect-decls -dialect=test -I%S/../../include %s 2>&1 | FileCheck --check-prefixes=CHECK %s

// CHECK:      Superclass SuperClassVarArgOp already has variadic arguments, cannot have any other arguments.
// CHECK-NEXT: ... in operation SubOp
// CHECK-NEXT: LLVM ERROR: error parsing operation

include "llvm-dialects/Dialect/Dialect.td"

def TestDialect : Dialect {
  let name = "test";
  let cppNamespace = "test";
}

def SuperClassVarArgOp : OpClass<TestDialect> {
  let arguments = (ins varargs:$args);

  let summary = "Superclass with variadic argument";
}

class SubClass<string op> : Op<TestDialect, "return", []> {
  let superclass = SuperClassVarArgOp;
  let arguments = (ins superclass, varargs:$args);
  let results = (outs);

  let summary = "Subclass with variadic argument";
}

def SubOp : SubClass<"subop">;