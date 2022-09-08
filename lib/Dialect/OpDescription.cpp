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

#include "llvm-dialects/Dialect/OpDescription.h"

#include "llvm-dialects/Dialect/Dialect.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"

using namespace llvm_dialects;
using namespace llvm;

bool OpDescription::matchInstruction(Instruction &inst) const {
  if (auto *call = dyn_cast<CallInst>(&inst)) {
    if (auto *fn = call->getCalledFunction())
      return matchDeclaration(*fn);
  }
  return false;
}

bool OpDescription::matchDeclaration(Function &decl) const {
  if (m_hasOverloads)
    return llvm_dialects::detail::isOverloadedOperationDecl(&decl, m_mnemonic);
  return llvm_dialects::detail::isSimpleOperationDecl(&decl, m_mnemonic);
}
