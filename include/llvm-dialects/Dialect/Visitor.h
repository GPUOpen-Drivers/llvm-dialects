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

/// @brief Auxiliary template to support nested visitor clients.
///
/// It can be convenient to write visitor clients in a modular fashion and
/// combine them together to use a single @ref Visitor. Each client module
/// may use a different payload type.
///
/// In this case, each nested client module's payload must be reachable from
/// the top-level payload, and a specialization of this template must be
/// provided that "projects" parent payload references to child payload
/// references.
///
/// In cases where the nested payload is a field of the parent payload, the
/// @ref LLVM_DIALECTS_VISITOR_PAYLOAD_PROJECT_FIELD can be used instead.
template <typename PayloadT, typename NestedPayloadT>
struct VisitorPayloadProjection {
  // Template specializations must implement this static method:
  //
  //   static NestedPayloadT &project(PayloadT &);
};

/// Declare that `PayloadT` can be projected to a nested payload type via
/// `field`.
///
/// This creates a specialization of @ref VisitorPayloadProjection and must
/// therefore typically be outside of any namespace. The nested type is derived
/// automatically.
#define LLVM_DIALECTS_VISITOR_PAYLOAD_PROJECT_FIELD(PayloadT, field)           \
  template <>                                                                  \
  struct llvm_dialects::detail::VisitorPayloadOffsetProjection<                \
      PayloadT,                                                                \
      std::remove_reference_t<decltype(std::declval<PayloadT>().field)>> {     \
    static constexpr bool useOffsetof = true;                                  \
    static constexpr std::size_t offset = offsetof(PayloadT, field);           \
  };

namespace detail {

class VisitorBase;

using VisitorCallback = void (void *, void *, llvm::Instruction *);
using PayloadProjectionCallback = void *(void *);

/// Apply first the byte offset and then the projection function. If projection
/// is null, stop the projection sequence.
struct PayloadProjection {
  std::size_t offset = 0;
  PayloadProjectionCallback *projection = nullptr;
};

struct VisitorCase {
  const OpDescription *description = nullptr;
  VisitorCallback *callback = nullptr;
  void *callbackData = nullptr;

  // If non-negative, a byte offset to apply to the payload. If negative,
  // a shifted index into the projections vector.
  ssize_t projection = 0;
};

template <typename PayloadT, typename NestedPayloadT>
struct VisitorPayloadOffsetProjection {
  static constexpr bool useOffsetof = false;
};

class VisitorBuilderBase {
  friend class VisitorBase;
public:
  VisitorBuilderBase() = default;
  explicit VisitorBuilderBase(VisitorBuilderBase *parent) : m_parent(parent) {}
  ~VisitorBuilderBase();

  void setStrategy(VisitorStrategy strategy) { m_strategy = strategy; }

  void add(const OpDescription &desc, void *extra, VisitorCallback *fn);

public:
  PayloadProjectionCallback *m_projection = nullptr;
  size_t m_offsetProjection = 0;

private:
  VisitorBuilderBase *m_parent = nullptr;
  VisitorStrategy m_strategy = VisitorStrategy::ByFunctionDeclaration;
  llvm::SmallVector<VisitorCase> m_cases;
  llvm::SmallVector<PayloadProjection> m_projections;
};

class VisitorBase {
protected:
  VisitorBase(VisitorBuilderBase &&builder);

  void visit(void *payload, llvm::Instruction &inst) const;
  void visit(void *payload, llvm::Function &fn) const;
  void visit(void *payload, llvm::Module &module) const;

private:
  void call(const VisitorCase &theCase, void *payload,
            llvm::Instruction &inst) const;

  VisitorStrategy m_strategy;
  llvm::SmallVector<VisitorCase> m_cases;
  llvm::SmallVector<PayloadProjection> m_projections;
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
  Visitor(detail::VisitorBuilderBase &&builder)
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
class VisitorBuilder : private detail::VisitorBuilderBase {
  template <typename OtherT> friend class VisitorBuilder;

public:
  using Payload = PayloadT;

  VisitorBuilder() = default;

  VisitorBuilder &setStrategy(VisitorStrategy strategy) {
    VisitorBuilderBase::setStrategy(strategy);
    return *this;
  }

  Visitor<PayloadT> build() { return std::move(*this); }

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

  template <typename NestedPayloadT>
  VisitorBuilder &nest(void (*registration)(VisitorBuilder<NestedPayloadT> &)) {
    VisitorBuilder<NestedPayloadT> nested{this};

    if constexpr (detail::VisitorPayloadOffsetProjection<
                      PayloadT, NestedPayloadT>::useOffsetof) {
      nested.m_offsetProjection =
          detail::VisitorPayloadOffsetProjection<PayloadT,
                                                 NestedPayloadT>::offset;
    } else {
      nested.m_projection = [](void *payload) -> void * {
        return static_cast<void *>(
            &VisitorPayloadProjection<PayloadT, NestedPayloadT>::project(
                *static_cast<PayloadT *>(payload)));
      };
    }

    (*registration)(nested);

    return *this;
  }

private:
  explicit VisitorBuilder(VisitorBuilderBase *parent)
      : VisitorBuilderBase(parent) {}
};

} // namespace llvm_dialects
