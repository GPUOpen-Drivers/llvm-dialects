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

// RUN: not --crash llvm-dialects-tblgen -gen-dialect-defs -dialect=dups -I%S/../../include %s 2>&1 | FileCheck --check-prefixes=CHECK %s

// CHECK:      Found op with non-unique mnemonic: duplicate.op
// CHECK-NEXT: Found op with non-unique mnemonic: duplicate.op.1
// CHECK-NEXT: LLVM ERROR: Aborting dialect generation since non-unique mnemonics were found! 

include "llvm-dialects/Dialect/Dialect.td"

def TestDialect : Dialect {
  let name = "dups";
  let cppNamespace = "dups";
}

class TestOp<string mnemonic_, list<Trait> traits_>
    : Op<TestDialect, mnemonic_, traits_>;

def DuplicateOp : TestOp<"duplicate.op",
                       []> {
  let results = (outs);
  let arguments = (ins);
}

def DuplicateOp1 : TestOp<"duplicate.op",
                       []> {
  let results = (outs);
  let arguments = (ins);
}

def DuplicateOp2 : TestOp<"duplicate.op.1",
                       []> {
  let results = (outs);
  let arguments = (ins);
}

def DuplicateOp3 : TestOp<"duplicate.op.1",
                       []> {
  let results = (outs);
  let arguments = (ins);
}

def UniqueOp : TestOp<"unique.op",
                       []> {
  let results = (outs);
  let arguments = (ins);
}
