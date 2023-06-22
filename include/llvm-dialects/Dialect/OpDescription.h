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
#include "llvm/ADT/StringRef.h"

#include <variant>
#include <vector>

namespace llvm {
class Function;
class Instruction;
} // namespace llvm

namespace llvm_dialects {

/// @brief Reflect an operation.
class OpDescription {
public:
  enum class Kind {
    Core,
    Dialect,
    DialectWithOverloads,
    Intrinsic,
  };

public:
  OpDescription(bool hasOverloads, llvm::StringRef mnemonic)
      : m_kind(hasOverloads ? Kind::DialectWithOverloads : Kind::Dialect),
        m_op(mnemonic) {}
  OpDescription(Kind kind, unsigned opcode) : m_kind(kind), m_op(opcode) {}
  OpDescription(Kind kind, llvm::MutableArrayRef<unsigned> opcodes);

  template <typename OpT>
  static const OpDescription& get();

  Kind getKind() const { return m_kind; }
  llvm::ArrayRef<unsigned> getOpcodes() const;

  bool matchInstruction(llvm::Instruction &inst) const;
  bool matchDeclaration(llvm::Function &decl) const;

  bool canMatchDeclaration() const {
    return m_kind == Kind::Dialect || m_kind == Kind::DialectWithOverloads ||
           m_kind == Kind::Intrinsic;
  }

private:
  bool matchIntrinsic(unsigned intrinsicId) const;

  Kind m_kind;

  // Holds one of:
  //  - core instruction opcode or intrinsic ID
  //  - sorted array of opcodes or intrinsic IDs
  //  - mnemonic
  std::variant<unsigned, llvm::ArrayRef<unsigned>, llvm::StringRef> m_op;
};

} // namespace llvm_dialects
