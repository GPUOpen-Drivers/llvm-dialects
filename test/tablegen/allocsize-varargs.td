/*
 ***********************************************************************************************************************
 * Copyright (c) 2026 Advanced Micro Devices, Inc. All Rights Reserved.
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

// The declaration of an operation with a variadic argument list has no fixed
// parameters, and LLVM's IR verifier rejects allocsize argument indices that
// do not refer to fixed parameters of the declaration.

// RUN: not --crash llvm-dialects-tblgen -gen-dialect-defs -dialect=test -I%S/../../include %s 2>&1 | FileCheck --check-prefixes=CHECK %s

// CHECK: LLVM ERROR: AllocSize: cannot be used on operation 'VarArgAllocOp' which has a variadic argument list

include "llvm-dialects/Dialect/Dialect.td"

def TestDialect : Dialect {
  let name = "test";
  let cppNamespace = "test";
}

def VarArgAllocOp : Op<TestDialect, "vararg.alloc", [AllocSize<0>]> {
  let results = (outs Ptr:$result);
  let arguments = (ins I64:$size, varargs:$args);

  let summary = "op with a variadic argument list and allocsize";
}
