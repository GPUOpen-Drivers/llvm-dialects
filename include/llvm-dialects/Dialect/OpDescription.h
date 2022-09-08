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

#include "llvm/ADT/StringRef.h"

namespace llvm {
class Function;
class Instruction;
} // namespace llvm

namespace llvm_dialects {

/// @brief Reflect an operation defined by a dialect
class OpDescription {
public:
  OpDescription(bool hasOverloads, llvm::StringRef mnemonic)
      : m_hasOverloads(hasOverloads), m_mnemonic(mnemonic) {}

  template <typename OpT>
  static const OpDescription& get();

  bool matchInstruction(llvm::Instruction &inst) const;
  bool matchDeclaration(llvm::Function &decl) const;

private:
  bool m_hasOverloads;
  llvm::StringRef m_mnemonic;
};

} // namespace llvm_dialects
