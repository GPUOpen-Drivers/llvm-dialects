/*
 ***********************************************************************************************************************
 * Copyright (c) 2024 Advanced Micro Devices, Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ***********************************************************************************************************************
 */

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