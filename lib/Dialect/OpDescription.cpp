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

unsigned OpDescription::getOpcode() const {
  const ArrayRef<unsigned> opcodes{getOpcodes()};
  assert(!opcodes.empty() && "OpDescription does not contain any opcode!");

  return opcodes.front();
}

ArrayRef<unsigned> OpDescription::getOpcodes() const {
  assert(m_kind == Kind::Core || m_kind == Kind::Intrinsic);

  if (auto *op = std::get_if<unsigned>(&m_op))
    return *op;

  return std::get<ArrayRef<unsigned>>(m_op);
}

bool OpDescription::matchInstruction(const Instruction &inst) const {
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

bool OpDescription::matchDeclaration(const Function &decl) const {
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

#define HANDLE_INTRINSIC_DESC(Class, opcode)                                   \
  template <> const OpDescription &OpDescription::get<Class>() {               \
    static const OpDescription desc{Kind::Intrinsic, Intrinsic::opcode};       \
    return desc;                                                               \
  }
#define HANDLE_INTRINSIC_DESC_OPCODE_SET(Class, ...)                           \
  template <> const OpDescription &OpDescription::get<Class>() {               \
    static unsigned opcodes[] = {__VA_ARGS__};                                 \
    static const OpDescription desc{Kind::Intrinsic, opcodes};                 \
    return desc;                                                               \
  }

// ============================================================================
// Descriptions of intrinsic facades implemented in LLVM

HANDLE_INTRINSIC_DESC_OPCODE_SET(LifetimeIntrinsic, Intrinsic::lifetime_start,
                                 Intrinsic::lifetime_end)

// Add Intrinsic::dbg_addr back for sufficiently recent LLVM
HANDLE_INTRINSIC_DESC_OPCODE_SET(DbgInfoIntrinsic, Intrinsic::dbg_declare,
                                 Intrinsic::dbg_value, Intrinsic::dbg_label,
                                 Intrinsic::dbg_assign)

// Add Intrinsic::dbg_addr back for sufficiently recent LLVM
HANDLE_INTRINSIC_DESC_OPCODE_SET(DbgVariableIntrinsic, Intrinsic::dbg_declare,
                                 Intrinsic::dbg_value, Intrinsic::dbg_assign)

HANDLE_INTRINSIC_DESC(DbgDeclareInst, dbg_declare)
HANDLE_INTRINSIC_DESC(DbgValueInst, dbg_value)

// Add this back for sufficiently recent LLVM
// HANDLE_INTRINSIC_DESC(DbgAddrIntrinsic, dbg_addr)

HANDLE_INTRINSIC_DESC(DbgAssignIntrinsic, dbg_assign)
HANDLE_INTRINSIC_DESC(DbgLabelInst, dbg_label)

HANDLE_INTRINSIC_DESC_OPCODE_SET(AtomicMemIntrinsic,
                                 Intrinsic::memcpy_element_unordered_atomic,
                                 Intrinsic::memmove_element_unordered_atomic,
                                 Intrinsic::memset_element_unordered_atomic)

HANDLE_INTRINSIC_DESC(AtomicMemSetInst, memset_element_unordered_atomic)

HANDLE_INTRINSIC_DESC_OPCODE_SET(AtomicMemTransferInst,
                                 Intrinsic::memcpy_element_unordered_atomic,
                                 Intrinsic::memmove_element_unordered_atomic)

HANDLE_INTRINSIC_DESC(AtomicMemCpyInst, memcpy_element_unordered_atomic)
HANDLE_INTRINSIC_DESC(AtomicMemMoveInst, memmove_element_unordered_atomic)
HANDLE_INTRINSIC_DESC_OPCODE_SET(MemIntrinsic, Intrinsic::memcpy,
                                 Intrinsic::memmove, Intrinsic::memset,
                                 Intrinsic::memset_inline,
                                 Intrinsic::memcpy_inline)

HANDLE_INTRINSIC_DESC_OPCODE_SET(MemSetInst, Intrinsic::memset,
                                 Intrinsic::memset_inline)

HANDLE_INTRINSIC_DESC(MemSetInlineInst, memset_inline)

HANDLE_INTRINSIC_DESC_OPCODE_SET(MemTransferInst, Intrinsic::memcpy,
                                 Intrinsic::memmove, Intrinsic::memcpy_inline)

HANDLE_INTRINSIC_DESC_OPCODE_SET(MemCpyInst, Intrinsic::memcpy,
                                 Intrinsic::memcpy_inline)

HANDLE_INTRINSIC_DESC(MemMoveInst, memmove)
HANDLE_INTRINSIC_DESC(MemCpyInlineInst, memcpy_inline)

HANDLE_INTRINSIC_DESC_OPCODE_SET(AnyMemIntrinsic, Intrinsic::memcpy,
                                 Intrinsic::memcpy_inline, Intrinsic::memmove,
                                 Intrinsic::memset, Intrinsic::memset_inline,
                                 Intrinsic::memcpy_element_unordered_atomic,
                                 Intrinsic::memmove_element_unordered_atomic,
                                 Intrinsic::memset_element_unordered_atomic)

HANDLE_INTRINSIC_DESC_OPCODE_SET(AnyMemSetInst, Intrinsic::memset,
                                 Intrinsic::memset_inline,
                                 Intrinsic::memset_element_unordered_atomic)

HANDLE_INTRINSIC_DESC_OPCODE_SET(AnyMemTransferInst, Intrinsic::memcpy,
                                 Intrinsic::memcpy_inline, Intrinsic::memmove,
                                 Intrinsic::memcpy_element_unordered_atomic,
                                 Intrinsic::memmove_element_unordered_atomic)

HANDLE_INTRINSIC_DESC_OPCODE_SET(AnyMemCpyInst, Intrinsic::memcpy,
                                 Intrinsic::memcpy_inline,
                                 Intrinsic::memcpy_element_unordered_atomic)

HANDLE_INTRINSIC_DESC_OPCODE_SET(AnyMemMoveInst, Intrinsic::memmove,
                                 Intrinsic::memmove_element_unordered_atomic)

// TODO: Is completing this list worth it?

#undef HANDLE_INTRINSIC_DESC_OPCODE_SET
#undef HANDLE_INTRINSIC_DESC
