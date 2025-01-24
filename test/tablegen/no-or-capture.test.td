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

// CHECK: failed to generate verifier method for IAddOp:

include "llvm-dialects/Dialect/Dialect.td"

def TestDialect : Dialect {
  let name = "test";
  let cppNamespace = "test";
}

def IAddOp : Op<TestDialect, "iadd", []> {
  let results = (outs (ScalarOrFixedVector IntegerType:$scalar_type):$result);
  let arguments = (ins value:$lhs, value:$rhs);

  let verifier = [
    (eq $lhs, $result),
    (or (eq $rhs, $scalar_type), (eq $rhs, $result)),
  ];

  let summary = "add two integers or vectors of integers";
  let description = [{
    Demonstrates the limitations of (or ...) in the consteraint languages.
    The idea is that:

    - The `result` can be an integer or a fixed vector of integers.
    - The `lhs` must be of the same type as the result.
    - The `rhs` must be the same type as the result or, if the result is a
      vector, of the corresponding scalar type.

    However, $scalar_type only appears (indirectly) inside logical `or`
    expressions and so cannot be captured.
  }];
}
