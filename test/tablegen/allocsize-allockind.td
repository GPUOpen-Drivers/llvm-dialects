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

// RUN: llvm-dialects-tblgen -gen-dialect-defs -dialect=test -I%S/../../include %s | FileCheck --check-prefixes=CHECK %s

// CHECK-DAG: attrBuilder.addAllocSizeAttr(0, std::nullopt);
// CHECK-DAG: attrBuilder.addAllocKindAttr(::llvm::AllocFnKind::Alloc | ::llvm::AllocFnKind::Uninitialized);
// CHECK-DAG: attrBuilder.addAllocSizeAttr(1, 0);
// CHECK-DAG: attrBuilder.addAllocKindAttr(::llvm::AllocFnKind::Realloc | ::llvm::AllocFnKind::Aligned);
// CHECK-DAG: attrBuilder.addAllocKindAttr(::llvm::AllocFnKind::Free);

include "llvm-dialects/Dialect/Dialect.td"

def TestDialect : Dialect {
  let name = "test";
  let cppNamespace = "test";
}

class TestOp<string mnemonic_, list<Trait> traits_>
    : Op<TestDialect, mnemonic_, traits_>;

def AllocOp : TestOp<"alloc",
                     [AllocSize<0>, AllocKind<["alloc", "uninitialized"]>]> {
  let results = (outs Ptr:$result);
  let arguments = (ins I64:$size);

  let summary = "allocate uninitialized memory";
}

def ReallocOp : TestOp<"realloc",
                       [AllocSize<1, 0>, AllocKind<["realloc", "aligned"]>]> {
  let results = (outs Ptr:$result);
  let arguments = (ins I32:$count, I32:$elem_size, Ptr:$ptr, I32:$align);

  let summary = "reallocate an aligned array";
}

def FreeOp : TestOp<"free", [AllocKind<["free"]>]> {
  let results = (outs);
  let arguments = (ins Ptr:$ptr);

  let summary = "free an allocation";
}
