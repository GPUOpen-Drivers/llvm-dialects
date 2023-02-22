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

#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Casting.h"

#include "llvm-dialects/Dialect/OpDescription.h"

namespace llvm {
class Function;
class Instruction;
class Module;
} // namespace llvm

namespace llvm_dialects {

template <typename PayloadT>
class Visitor;

/// The strategy for implementation a visitor of dialect ops, 
enum class VisitorStrategy {
  /// Iterate over all instructions to find the ones that match.
  ByInstruction,

  /// Iterate over the function declarations in a module, filter out those for
  /// relevant dialect ops, and then iterate over their users.
  ///
  /// If a visitor is ever used to also visit core LLVM operations, we fall
  /// back to @ref ByInstruction.
  ByFunctionDeclaration,
};

namespace detail {

class VisitorBase;

using VisitorCallback = void (void *, void *, llvm::Instruction *);
using VisitorCase = std::tuple<const OpDescription *, void *, VisitorCallback *>;

class VisitorBuilderBase {
  friend class VisitorBase;
public:
  void setStrategy(VisitorStrategy strategy) { m_strategy = strategy; }

protected:
  void add(const OpDescription &desc, void *extra, VisitorCallback *fn);

private:
  VisitorStrategy m_strategy = VisitorStrategy::ByFunctionDeclaration;
  llvm::SmallVector<VisitorCase> m_cases;
};

class VisitorBase {
protected:
  VisitorBase(VisitorBuilderBase builder);

  void visit(void *payload, llvm::Instruction &inst) const;
  void visit(void *payload, llvm::Function &fn) const;
  void visit(void *payload, llvm::Module &module) const;

private:
  VisitorStrategy m_strategy;
  llvm::SmallVector<VisitorCase> m_cases;
};

} // namespace detail

/// @brief A class that allows visiting all uses of dialect operations.
///
/// See @ref VisitorBuilder for how to instantiate this class.
///
/// Instructions are not necessarily visited in the order in which they appear
/// in the containing function or even basic block.
///
/// Callbacks must not delete or remove their instruction argument.
template <typename PayloadT>
class Visitor : public detail::VisitorBase {
public:
  Visitor(detail::VisitorBuilderBase builder)
      : VisitorBase(std::move(builder)) {}

  void visit(PayloadT &payload, llvm::Instruction &inst) const {
    VisitorBase::visit(static_cast<void *>(&payload), inst);
  }

  void visit(PayloadT &payload, llvm::Function &fn) const {
    VisitorBase::visit(static_cast<void *>(&payload), fn);
  }

  void visit(PayloadT &payload, llvm::Module &module) const {
    VisitorBase::visit(static_cast<void *>(&payload), module);
  }
};

/// @brief Build a visitor for dialect operations
///
/// @ref Visitor instances are created via this builder class. The intention is
/// that for each code location that wants to iterate over dialect operations,
/// a visitor instance is created statically once and re-used many times.
///
/// This allows more expensive optimizations to be implemented in the visitor
/// whose setup time can be amortized over many uses (e.g., build a hash table
/// for faster operation lookup), but note that the current implementation is
/// naive.
///
/// Example use:
///
/// @code
/// static const auto myVisitor =
///     llvm_dialects::VisitorBuilder<YourPayloadType>()
///         .add<YourOp>([](auto &payload, auto &op) {
///           ...
///         })
///         .build();
/// myVisitor(myPayload, module);
/// @endcode
template <typename PayloadT>
class VisitorBuilder : public detail::VisitorBuilderBase {
public:
  using Payload = PayloadT;

  VisitorBuilder &setStrategy(VisitorStrategy strategy) {
    VisitorBuilderBase::setStrategy(strategy);
    return *this;
  }

  Visitor<PayloadT> build() { return {std::move(*this)}; }

  template <typename OpT> VisitorBuilder &add(void (*fn)(PayloadT &, OpT &)) {
    VisitorBuilderBase::add(
        OpDescription::get<OpT>(),
        (void *)fn,
        [](void *extra, void *payload, llvm::Instruction *op) {
          auto fn = (void (*)(PayloadT &, OpT &))extra;
          fn(*static_cast<PayloadT *>(payload), *llvm::cast<OpT>(op));
        });
    return *this;
  }
};

} // namespace llvm_dialects
