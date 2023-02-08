/*
 * Copyright (c) 2022 Advanced Micro Devices, Inc. All Rights Reserved.
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
 */

#pragma once

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/STLFunctionalExtras.h"

namespace llvm {
class raw_ostream;
class Instruction;
class Type;
} // namespace llvm

namespace llvm_dialects {

/// Returns true if the given types are all equal. See the SameTypes predicate
/// in TableGen.
bool areTypesEqual(llvm::ArrayRef<llvm::Type *> types);

std::string getMangledName(llvm::StringRef name,
                           llvm::ArrayRef<llvm::Type *> overloadTypes);

bool runInstructionVerifier(
    llvm::function_ref<bool (llvm::raw_ostream &)> verifier,
    llvm::Instruction *instruction,
    llvm::raw_ostream *errs = nullptr);

} // namespace llvm_dialects
