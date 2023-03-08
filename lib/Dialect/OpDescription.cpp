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
#include "llvm/IR/IntrinsicInst.h"

using namespace llvm_dialects;
using namespace llvm;

OpDescription::OpDescription(Kind kind, MutableArrayRef<unsigned> opcodes)
    : m_kind(kind), m_op(opcodes) {
  llvm::sort(opcodes);
}

ArrayRef<unsigned> OpDescription::getOpcodes() const {
  assert(m_kind == Kind::Core || m_kind == Kind::Intrinsic);

  if (auto *op = std::get_if<unsigned>(&m_op))
    return *op;

  return std::get<ArrayRef<unsigned>>(m_op);
}

bool OpDescription::matchInstruction(Instruction &inst) const {
  if (m_kind == Kind::Intrinsic) {
    if (auto *intr = dyn_cast<IntrinsicInst>(&inst))
      return matchIntrinsic(intr->getIntrinsicID());
    return false;
  }

  if (m_kind == Kind::Core) {
    if (auto *op = std::get_if<unsigned>(&m_op))
      return inst.getOpcode() == *op;

    auto opcodes = std::get<ArrayRef<unsigned>>(m_op);
    auto it = llvm::lower_bound(opcodes, inst.getOpcode());
    return it != opcodes.end() && *it == inst.getOpcode();
  }

  if (auto *call = dyn_cast<CallInst>(&inst)) {
    if (auto *fn = call->getCalledFunction())
      return matchDeclaration(*fn);
  }
  return false;
}

bool OpDescription::matchDeclaration(Function &decl) const {
  if (auto *mnemonic = std::get_if<StringRef>(&m_op)) {
    if (m_kind == Kind::DialectWithOverloads)
      return llvm_dialects::detail::isOverloadedOperationDecl(&decl, *mnemonic);
    assert(m_kind == Kind::Dialect);
    return llvm_dialects::detail::isSimpleOperationDecl(&decl, *mnemonic);
  }

  assert(m_kind == Kind::Intrinsic);
  return matchIntrinsic(decl.getIntrinsicID());
}

bool OpDescription::matchIntrinsic(unsigned intrinsicId) const {
  assert(m_kind == Kind::Intrinsic);

  if (auto *op = std::get_if<unsigned>(&m_op))
    return *op == intrinsicId;

  auto opcodes = std::get<ArrayRef<unsigned>>(m_op);
  auto it = llvm::lower_bound(opcodes, intrinsicId);
  return it != opcodes.end() && *it == intrinsicId;
}

// ============================================================================
// Descriptions of core instructions.

template <> const OpDescription &OpDescription::get<UnaryInstruction>() {
  static unsigned opcodes[] = {
      Instruction::Alloca,
      Instruction::Load,
      Instruction::VAArg,
      Instruction::ExtractValue,

#define HANDLE_UNARY_INST(num, opcode, Class) Instruction::opcode,
#define HANDLE_CAST_INST(num, opcode, Class) Instruction::opcode,
#include "llvm/IR/Instruction.def"
  };
  static const OpDescription desc{Kind::Core, opcodes};
  return desc;
}

template <> const OpDescription &OpDescription::get<BinaryOperator>() {
  static unsigned opcodes[] = {
#define HANDLE_BINARY_INST(num, opcode, Class) Instruction::opcode,
#include "llvm/IR/Instruction.def"
  };
  static const OpDescription desc{Kind::Core, opcodes};
  return desc;
}

// Generate OpDescription for all dedicate instruction classes.
#define HANDLE_USER_INST(...)
#define HANDLE_UNARY_INST(...)
#define HANDLE_BINARY_INST(...)
#define HANDLE_INST(num, opcode, Class)                                        \
  template <> const OpDescription &OpDescription::get<Class>() {               \
    static const OpDescription desc{Kind::Core, Instruction::opcode};          \
    return desc;                                                               \
  }
#include "llvm/IR/Instruction.def"

// ============================================================================
// Descriptions of intrinsic facades implemented in LLVM

template <> const OpDescription &OpDescription::get<LifetimeIntrinsic>() {
  static unsigned opcodes[] = {
      Intrinsic::lifetime_start,
      Intrinsic::lifetime_end,
  };
  static const OpDescription desc{Kind::Intrinsic, opcodes};
  return desc;
}

template <> const OpDescription &OpDescription::get<DbgInfoIntrinsic>() {
  static unsigned opcodes[] = {
      Intrinsic::dbg_declare, Intrinsic::dbg_value, Intrinsic::dbg_label,
      Intrinsic::dbg_assign,
      // Intrinsic::dbg_addr, <-- add this back for sufficiently recent LLVM
  };
  static const OpDescription desc{Kind::Intrinsic, opcodes};
  return desc;
}

template <> const OpDescription &OpDescription::get<DbgVariableIntrinsic>() {
  static unsigned opcodes[] = {
      Intrinsic::dbg_declare,
      Intrinsic::dbg_value,
      // Intrinsic::dbg_addr, <-- add this back for sufficiently recent LLVM
      Intrinsic::dbg_assign,
  };
  static const OpDescription desc{Kind::Intrinsic, opcodes};
  return desc;
}

template <> const OpDescription &OpDescription::get<DbgDeclareInst>() {
  static const OpDescription desc{Kind::Intrinsic, Intrinsic::dbg_declare};
  return desc;
}

template <> const OpDescription &OpDescription::get<DbgValueInst>() {
  static const OpDescription desc{Kind::Intrinsic, Intrinsic::dbg_value};
  return desc;
}

// Add this back for sufficiently recent LLVM
// template <> const OpDescription &OpDescription::get<DbgAddrIntrinsic>() {
//   static const OpDescription desc{Kind::Intrinsic, Intrinsic::dbg_addr};
//   return desc;
// }

template <> const OpDescription &OpDescription::get<DbgAssignIntrinsic>() {
  static const OpDescription desc{Kind::Intrinsic, Intrinsic::dbg_assign};
  return desc;
}

template <> const OpDescription &OpDescription::get<DbgLabelInst>() {
  static const OpDescription desc{Kind::Intrinsic, Intrinsic::dbg_label};
  return desc;
}

template <> const OpDescription &OpDescription::get<AtomicMemIntrinsic>() {
  static unsigned opcodes[] = {
      Intrinsic::memcpy_element_unordered_atomic,
      Intrinsic::memmove_element_unordered_atomic,
      Intrinsic::memset_element_unordered_atomic,
  };
  static const OpDescription desc{Kind::Intrinsic, opcodes};
  return desc;
}

template <> const OpDescription &OpDescription::get<AtomicMemSetInst>() {
  static const OpDescription desc{Kind::Intrinsic,
                                  Intrinsic::memset_element_unordered_atomic};
  return desc;
}

template <> const OpDescription &OpDescription::get<AtomicMemTransferInst>() {
  static unsigned opcodes[] = {
      Intrinsic::memcpy_element_unordered_atomic,
      Intrinsic::memmove_element_unordered_atomic,
  };
  static const OpDescription desc{Kind::Intrinsic, opcodes};
  return desc;
}

template <> const OpDescription &OpDescription::get<AtomicMemCpyInst>() {
  static const OpDescription desc{Kind::Intrinsic,
                                  Intrinsic::memcpy_element_unordered_atomic};
  return desc;
}

template <> const OpDescription &OpDescription::get<AtomicMemMoveInst>() {
  static const OpDescription desc{Kind::Intrinsic,
                                  Intrinsic::memmove_element_unordered_atomic};
  return desc;
}

template <> const OpDescription &OpDescription::get<MemIntrinsic>() {
  static unsigned opcodes[] = {
      Intrinsic::memcpy,        Intrinsic::memmove,       Intrinsic::memset,
      Intrinsic::memset_inline, Intrinsic::memcpy_inline,
  };
  static const OpDescription desc{Kind::Intrinsic, opcodes};
  return desc;
}

template <> const OpDescription &OpDescription::get<MemSetInst>() {
  static unsigned opcodes[] = {
      Intrinsic::memset,
      Intrinsic::memset_inline,
  };
  static const OpDescription desc{Kind::Intrinsic, opcodes};
  return desc;
}

template <> const OpDescription &OpDescription::get<MemSetInlineInst>() {
  static const OpDescription desc{Kind::Intrinsic, Intrinsic::memset_inline};
  return desc;
}

template <> const OpDescription &OpDescription::get<MemTransferInst>() {
  static unsigned opcodes[] = {
      Intrinsic::memcpy,
      Intrinsic::memmove,
      Intrinsic::memcpy_inline,
  };
  static const OpDescription desc{Kind::Intrinsic, opcodes};
  return desc;
}

template <> const OpDescription &OpDescription::get<MemCpyInst>() {
  static unsigned opcodes[] = {
      Intrinsic::memcpy,
      Intrinsic::memcpy_inline,
  };
  static const OpDescription desc{Kind::Intrinsic, opcodes};
  return desc;
}

template <> const OpDescription &OpDescription::get<MemMoveInst>() {
  static const OpDescription desc{Kind::Intrinsic, Intrinsic::memmove};
  return desc;
}

template <> const OpDescription &OpDescription::get<MemCpyInlineInst>() {
  static const OpDescription desc{Kind::Intrinsic, Intrinsic::memcpy_inline};
  return desc;
}

template <> const OpDescription &OpDescription::get<AnyMemIntrinsic>() {
  static unsigned opcodes[] = {
      Intrinsic::memcpy,
      Intrinsic::memcpy_inline,
      Intrinsic::memmove,
      Intrinsic::memset,
      Intrinsic::memset_inline,
      Intrinsic::memcpy_element_unordered_atomic,
      Intrinsic::memmove_element_unordered_atomic,
      Intrinsic::memset_element_unordered_atomic,
  };
  static const OpDescription desc{Kind::Intrinsic, opcodes};
  return desc;
}

template <> const OpDescription &OpDescription::get<AnyMemSetInst>() {
  static unsigned opcodes[] = {
      Intrinsic::memset,
      Intrinsic::memset_inline,
      Intrinsic::memset_element_unordered_atomic,
  };
  static const OpDescription desc{Kind::Intrinsic, opcodes};
  return desc;
}

template <> const OpDescription &OpDescription::get<AnyMemTransferInst>() {
  static unsigned opcodes[] = {
      Intrinsic::memcpy,
      Intrinsic::memcpy_inline,
      Intrinsic::memmove,
      Intrinsic::memcpy_element_unordered_atomic,
      Intrinsic::memmove_element_unordered_atomic,
  };
  static const OpDescription desc{Kind::Intrinsic, opcodes};
  return desc;
}

template <> const OpDescription &OpDescription::get<AnyMemCpyInst>() {
  static unsigned opcodes[] = {
      Intrinsic::memcpy,
      Intrinsic::memcpy_inline,
      Intrinsic::memcpy_element_unordered_atomic,
  };
  static const OpDescription desc{Kind::Intrinsic, opcodes};
  return desc;
}

template <> const OpDescription &OpDescription::get<AnyMemMoveInst>() {
  static unsigned opcodes[] = {
      Intrinsic::memmove,
      Intrinsic::memmove_element_unordered_atomic,
  };
  static const OpDescription desc{Kind::Intrinsic, opcodes};
  return desc;
}

// TODO: Is completing this list worth it?
