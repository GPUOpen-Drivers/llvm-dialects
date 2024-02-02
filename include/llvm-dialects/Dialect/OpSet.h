/*
***********************************************************************************************************************
*
*  Copyright (c) 2023 Advanced Micro Devices, Inc. All Rights Reserved.
*
*  Permission is hereby granted, free of charge, to any person obtaining a copy
*  of this software and associated documentation files (the "Software"), to deal
*  in the Software without restriction, including without limitation the rights
*  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*  copies of the Software, and to permit persons to whom the Software is
*  furnished to do so, subject to the following conditions:
*
*  The above copyright notice and this permission notice shall be included in
*all copies or substantial portions of the Software.
*
*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*  SOFTWARE.
*
**********************************************************************************************************************/

#pragma once

#include "llvm-dialects/Dialect/Dialect.h"
#include "llvm-dialects/Dialect/OpDescription.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"

namespace llvm_dialects {

struct DialectOpPair final {
  llvm::StringRef mnemonic;
  bool isOverload;

  // Checks whether the current pair is comparable to an OpDescription
  // object.
  bool operator==(const OpDescription &desc) const {
    auto Kind = desc.getKind();
    bool hasOverload = Kind == OpDescription::Kind::DialectWithOverloads;

    if (Kind != OpDescription::Kind::Dialect && !hasOverload)
      return false;

    return desc.getMnemonic() == mnemonic && hasOverload == isOverload;
  }
};

// An OpSet defines a set of operations. It is used to simplify operating on a
// set of dialect operations, for instance, in the Visitor.
class OpSet final {
public:
  // -------------------------------------------------------------
  // Convenience functions to generate an OpSet from a given range
  // of operations.
  // -------------------------------------------------------------

  // Construct an OpSet from a set of core opcodes.
  static OpSet fromCoreOpcodes(llvm::ArrayRef<unsigned> ops) {
    OpSet set;
    for (const unsigned op : ops)
      set.m_coreOpcodes.insert(op);

    return set;
  }

  // Construct an OpSet from a set of intrinsics.
  static OpSet fromIntrinsicIDs(llvm::ArrayRef<unsigned> intrinsicIDs) {
    OpSet set;
    for (const unsigned intrinsicID : intrinsicIDs)
      set.m_intrinsicIDs.insert(intrinsicID);

    return set;
  }

  // Construct an OpSet from a set of OpDescriptions.
  static OpSet fromOpDescriptions(llvm::ArrayRef<OpDescription> descs) {
    OpSet set;
    for (const OpDescription &desc : descs)
      set.tryInsertOp(desc);

    return set;
  }

  // Construct an OpSet from a set of dialect ops, given as template
  // arguments.
  template <typename... OpTs> static const OpSet &get() {
    static const auto set = ([]() {
      OpSet set;
      (void)(... && ([&set]() {
               set.tryInsertOp(OpDescription::get<OpTs>());
               return true;
             })());
      return set;
    })();
    return set;
  }

  // -------------------------------------------------------------
  // contains check to check if a given operation is stored in the OpSet.
  // -------------------------------------------------------------
  // Checks if a given core opcode is stored in the set.
  bool containsCoreOp(unsigned coreOpcode) const {
    return m_coreOpcodes.contains(coreOpcode);
  }

  // Checks if a given intrinsic ID is stored in the set.
  bool containsIntrinsicID(unsigned intrinsicID) const {
    return m_intrinsicIDs.contains(intrinsicID);
  }

  // Checks if a given dialect operation is stored in the set.
  template <typename OpT> bool contains() const {
    static OpDescription desc = OpDescription::get<OpT>();
    return contains(desc);
  }

  // Checks if a given OpDescription is stored in the set.
  bool contains(const OpDescription &desc) const {
    if (desc.isCoreOp() || desc.isIntrinsic()) {
      assert(desc.getOpcodes().size() == 1 &&
             "OpSet only supports querying of single core opcodes and "
             "intrinsics.");

      const unsigned op = desc.getOpcode();
      return (desc.isCoreOp() && containsCoreOp(op)) ||
             (desc.isIntrinsic() && containsIntrinsicID(op));
    }

    return isMatchingDialectOp(desc.getMnemonic());
  }

  // Checks if `inst` belongs to the OpSet.
  bool contains(const llvm::Instruction &inst) const {
    if (containsCoreOp(inst.getOpcode()))
      return true;

    if (auto *CI = llvm::dyn_cast<llvm::CallInst>(&inst)) {
      const llvm::Function *Callee = CI->getCalledFunction();
      if (!Callee)
        return false;

      return contains(*Callee);
    }

    return false;
  }

  // Checks if `func` belongs to the OpSet.
  bool contains(const llvm::Function &func) const {
    if (func.isIntrinsic() && containsIntrinsicID(func.getIntrinsicID()))
      return true;

    return isMatchingDialectOp(func.getName());
  }

  // -------------------------------------------------------------
  // Convenience getters to access the internal data structures.
  // -------------------------------------------------------------
  const llvm::DenseSet<unsigned> &getCoreOpcodes() const {
    return m_coreOpcodes;
  }

  const llvm::DenseSet<unsigned> &getIntrinsicIDs() const {
    return m_intrinsicIDs;
  }

  const llvm::ArrayRef<DialectOpPair> getDialectOps() const {
    return m_dialectOps;
  }

private:
  // Checks if `mnemonic` can be described by any of the stored dialect
  // operations.
  bool isMatchingDialectOp(llvm::StringRef mnemonic) const {
    for (const auto &dialectOp : m_dialectOps) {
      if (detail::isOperationDecl(mnemonic, dialectOp.isOverload,
                                  dialectOp.mnemonic))
        return true;
    }

    return false;
  }

  // Tries to insert a given description in the internal data structures.
  void tryInsertOp(const OpDescription &desc) {
    if (desc.isCoreOp()) {
      for (const unsigned op : desc.getOpcodes())
        m_coreOpcodes.insert(op);

      return;
    }

    if (desc.isIntrinsic()) {
      for (const unsigned op : desc.getOpcodes())
        m_intrinsicIDs.insert(op);

      return;
    }

    // Store duplicate OpDescriptions once in the set.
    if (!contains(desc))
      m_dialectOps.push_back({desc.getMnemonic(), hasOverloads(desc)});
  }

  static bool hasOverloads(const OpDescription &desc) {
    return desc.getKind() == OpDescription::Kind::DialectWithOverloads;
  }

  llvm::DenseSet<unsigned> m_coreOpcodes;
  llvm::DenseSet<unsigned> m_intrinsicIDs;
  llvm::SmallVector<DialectOpPair, 1> m_dialectOps;
};
} // namespace llvm_dialects
