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

#include "llvm-dialects/Dialect/Dialect.h"
#include "llvm-dialects/Dialect/Utils.h"

#include "llvm/IR/IRBuilder.h"

namespace llvm_dialects {

class Builder : public llvm::IRBuilder<> {
  DialectContext& m_dialects;
public:
  explicit Builder(llvm::LLVMContext& context)
      : IRBuilder(context), m_dialects(DialectContext::get(context)) {}

  explicit Builder(llvm::BasicBlock* block)
      : IRBuilder(block), m_dialects(DialectContext::get(getContext())) {}

  explicit Builder(llvm::Instruction* instruction)
      : IRBuilder(instruction), m_dialects(DialectContext::get(getContext())) {}

  Builder(llvm::BasicBlock *block, llvm::BasicBlock::iterator it)
      : IRBuilder(block, it), m_dialects(DialectContext::get(getContext())) {}

  template <typename DialectT>
  DialectT& getDialect() const {return m_dialects.getDialect<DialectT>();}

  template <typename Op, typename... Args> Op *create(Args &&...args) {
    Op *op = Op::create(*this, std::forward<Args>(args)...);
    assert(runInstructionVerifier([op](llvm::raw_ostream &errs) {
      return op->verifier(errs);
    }, op));
    return op;
  }
};

} // namespace llvm_dialects
