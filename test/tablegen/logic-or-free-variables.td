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
