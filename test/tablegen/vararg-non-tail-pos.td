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
