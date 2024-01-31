/*
 * Copyright (c) 2022-2024 Advanced Micro Devices, Inc. All Rights Reserved.
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

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Casting.h"

#include "llvm-dialects/Dialect/OpDescription.h"
#include "llvm-dialects/Dialect/OpMap.h"
#include "llvm-dialects/Dialect/OpSet.h"

namespace llvm {
class Function;
class Instruction;
class IntrinsicInst;
class Module;
} // namespace llvm

namespace llvm_dialects {

template <typename PayloadT> class Visitor;

/// The iteration strategy of Visitor.
enum class VisitorStrategy {
  /// Pick a reasonable default.
  Default,

  /// Iterate over all instructions to find the ones that match.
  ByInstruction,

  /// Iterate over all instructions in reverse post-order.
  ReversePostOrder,

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

/// @brief Possible result states of visitor callbacks
///
/// A visitor may have multiple callbacks registered that match on the same
/// instruction. By default, all matching callbacks are invoked in the order in
/// which they were registered with the visitor. This may not be appropriate.
/// A common issue is when the callback erases and replaces the visited
/// instruction.
///
/// Callbacks may explicitly return a result state to indicate whether further
/// visits are desired.
enum class VisitorResult {
  /// Continue with the next callbacks on the same instruction. This is the
  /// default when the callback does not return a value.
  Continue,

  /// Skip subsequent callbacks
  Stop,
};

namespace detail {

class VisitorBase;
class VisitorTemplate;

/// @brief Key describing the condition of a visitor case
class VisitorKey {
  friend class VisitorTemplate;

public:
  template <typename OpT> static VisitorKey op() {
    VisitorKey key{Kind::OpDescription};
    key.m_description = &OpDescription::get<OpT>();
    return key;
  }

  template <typename... OpTs> static VisitorKey opSet() {
    VisitorKey key{Kind::OpSet};
    static const OpSet set = OpSet::get<OpTs...>();
    key.m_set = &set;
    return key;
  }

  static VisitorKey opSet(const OpSet &set) {
    VisitorKey key{Kind::OpSet};
    key.m_set = &set;
    return key;
  }

  static VisitorKey intrinsic(unsigned id) {
    VisitorKey key{Kind::Intrinsic};
    key.m_intrinsicId = id;
    return key;
  }

private:
  enum class Kind {
    OpDescription,
    Intrinsic,
    OpSet,
  };

  VisitorKey(Kind kind) : m_kind(kind) {}

  Kind m_kind;
  const OpDescription *m_description = nullptr;
  const OpSet *m_set = nullptr;
  unsigned m_intrinsicId = 0;
};

// Try to max the function pointer size in different cases.
// VisitorCallbackData of multiple inheritance has the larger function pointer
// size for microsoft cl compiler.
class Foo0 {};
class Foo1 {};
struct VisitorCallbackData : public Foo0, Foo1 {
private:
  static constexpr size_t SizeofMemberFunctionPointer =
      sizeof(void(VisitorCallbackData::*)());
  static constexpr size_t SizeofFunctionPointer = sizeof(void (*)());

public:
  // Size is 16 bytes in the 64-bit Itanium ABI.
  static constexpr size_t Size =
      std::max(SizeofFunctionPointer, SizeofMemberFunctionPointer);

  char data[Size];
};

using VisitorCallback = VisitorResult(const VisitorCallbackData &, void *,
                                      llvm::Instruction *);
using PayloadProjectionCallback = void *(void *);

struct VisitorHandler {
  // Either a byte offset to apply to the payload or an index into the payload
  // projections vector for more complex projections.
  struct Projection {
    static constexpr std::size_t Msb = (std::size_t)1
                                       << (8 * sizeof(std::size_t) - 1);

    std::size_t data = 0;

    void setOffset(std::size_t offset) {
      assert((offset & Msb) == 0);
      data = offset;
    }

    void setIndex(std::size_t index) {
      assert((index & Msb) == 0);
      data = index | Msb;
    }

    bool isOffset() const { return (data & Msb) == 0; }
    std::size_t getOffset() const {
      assert((data & Msb) == 0);
      return data;
    }
    std::size_t getIndex() const {
      assert((data & Msb) != 0);
      return data & ~Msb;
    }
  };

  Projection projection;

  VisitorCallback *callback = nullptr;
  VisitorCallbackData data;
};

/// Apply first the byte offset and then the projection function. If projection
/// is null, stop the projection sequence.
struct PayloadProjection {
  std::size_t offset = 0;
  PayloadProjectionCallback *projection = nullptr;
};

template <typename PayloadT, typename NestedPayloadT>
struct VisitorPayloadOffsetProjection {
  static constexpr bool useOffsetof = false;
};

/// @brief Visitor description during build
///
/// These structures are built up incrementally when a visitor is setup. They
/// are optimized when the final visitor is built.
class VisitorTemplate {
  friend class VisitorBase;
  friend class VisitorBuilderBase;

public:
  void setStrategy(VisitorStrategy strategy);
  void add(VisitorKey key, VisitorCallback *fn, VisitorCallbackData data,
           VisitorHandler::Projection projection);

private:
  VisitorStrategy m_strategy = VisitorStrategy::Default;
  std::vector<PayloadProjection> m_projections;
  std::vector<VisitorHandler> m_handlers;
  OpMap<llvm::SmallVector<unsigned>> m_opMap;
};

/// @brief Base class for VisitorBuilders
///
/// This class provides common functionality in a payload-type-erased manner.
class VisitorBuilderBase {
  friend class VisitorBase;

public:
  VisitorBuilderBase();
  VisitorBuilderBase(VisitorBuilderBase *parent,
                     PayloadProjectionCallback *projection);
  VisitorBuilderBase(VisitorBuilderBase *parent, size_t offset);

  VisitorBuilderBase(VisitorBuilderBase &&rhs)
      : m_ownedTemplate(std::move(rhs.m_ownedTemplate)),
        m_projection(rhs.m_projection) {
    assert(rhs.m_template == &rhs.m_ownedTemplate);
    m_template = &m_ownedTemplate;
  }
  VisitorBuilderBase &operator=(VisitorBuilderBase &&rhs) {
    assert(rhs.m_template == &rhs.m_ownedTemplate);
    if (this != &rhs) {
      m_ownedTemplate = std::move(rhs.m_ownedTemplate);
      m_template = &m_ownedTemplate;
      m_projection = rhs.m_projection;
    }
    return *this;
  }

  void setStrategy(VisitorStrategy strategy);

  void add(VisitorKey key, VisitorCallback *fn, VisitorCallbackData data);

  VisitorBase build();

private:
  VisitorBuilderBase(const VisitorBuilderBase &rhs) = delete;
  VisitorBuilderBase &operator=(const VisitorBuilderBase &rhs) = delete;

  VisitorTemplate m_ownedTemplate;
  VisitorTemplate *m_template;
  VisitorHandler::Projection m_projection;
};

/// @brief Base class for Visitors
///
/// Provides visitor functionality in a payload-type-erased manner.
class VisitorBase {
public:
  VisitorBase(VisitorTemplate &&templ);

  void visit(void *payload, llvm::Instruction &inst) const;
  void visit(void *payload, llvm::Function &fn) const;
  void visit(void *payload, llvm::Module &module) const;

private:
  class BuildHelper;
  using HandlerRange = std::pair<unsigned, unsigned>;

  void call(HandlerRange handlers, void *payload,
            llvm::Instruction &inst) const;
  VisitorResult call(const VisitorHandler &handler, void *payload,
                     llvm::Instruction &inst) const;

  template <typename FilterT>
  void visitByDeclarations(void *payload, llvm::Module &module,
                           FilterT &&filter) const;

  VisitorStrategy m_strategy;
  std::vector<PayloadProjection> m_projections;
  std::vector<VisitorHandler> m_handlers;
  OpMap<HandlerRange> m_opMap;
};

} // namespace detail

/// @brief A class that allows visiting all uses of dialect operations.
///
/// See @ref VisitorBuilder for how to instantiate this class.
///
/// Instructions are not necessarily visited in the order in which they appear
/// in the containing function or even basic block. If a fixed order is
/// required, select the ReversePostOrder strategy at VisitorBuilder time.
///
/// Callbacks must not delete or remove their instruction argument.
template <typename PayloadT> class Visitor : private detail::VisitorBase {
public:
  Visitor(detail::VisitorBase &&base) : VisitorBase(std::move(base)) {}

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

  Visitor<PayloadT> build() { return VisitorBuilderBase::build(); }

  template <typename OpT>
  VisitorBuilder &add(VisitorResult (*fn)(PayloadT &, OpT &)) {
    addCase<OpT>(detail::VisitorKey::op<OpT>(), fn);
    return *this;
  }

  template <typename OpT> VisitorBuilder &add(void (*fn)(PayloadT &, OpT &)) {
    addCase<OpT>(detail::VisitorKey::op<OpT>(), fn);
    return *this;
  }

  template <typename... OpTs>
  VisitorBuilder &addSet(VisitorResult (*fn)(PayloadT &,
                                             llvm::Instruction &I)) {
    addSetCase(detail::VisitorKey::opSet<OpTs...>(), fn);
    return *this;
  }

  template <typename... OpTs>
  VisitorBuilder &addSet(void (*fn)(PayloadT &, llvm::Instruction &I)) {
    addSetCase(detail::VisitorKey::opSet<OpTs...>(), fn);
    return *this;
  }

  VisitorBuilder &addSet(const OpSet &opSet,
                         VisitorResult (*fn)(PayloadT &,
                                             llvm::Instruction &I)) {
    addSetCase(detail::VisitorKey::opSet(opSet), fn);
    return *this;
  }

  VisitorBuilder &addSet(const OpSet &opSet,
                         void (*fn)(PayloadT &, llvm::Instruction &I)) {
    addSetCase(detail::VisitorKey::opSet(opSet), fn);
    return *this;
  }

  template <typename OpT>
  VisitorBuilder &add(VisitorResult (PayloadT::*fn)(OpT &)) {
    addMemberFnCase<OpT>(detail::VisitorKey::op<OpT>(), fn);
    return *this;
  }

  template <typename OpT> VisitorBuilder &add(void (PayloadT::*fn)(OpT &)) {
    addMemberFnCase<OpT>(detail::VisitorKey::op<OpT>(), fn);
    return *this;
  }

  VisitorBuilder &addIntrinsic(unsigned id,
                               VisitorResult (*fn)(PayloadT &,
                                                   llvm::IntrinsicInst &)) {
    addCase<llvm::IntrinsicInst>(detail::VisitorKey::intrinsic(id), fn);
    return *this;
  }

  VisitorBuilder &addIntrinsic(unsigned id,
                               void (*fn)(PayloadT &, llvm::IntrinsicInst &)) {
    addCase<llvm::IntrinsicInst>(detail::VisitorKey::intrinsic(id), fn);
    return *this;
  }

  VisitorBuilder &
  addIntrinsic(unsigned id,
               VisitorResult (PayloadT::*fn)(llvm::IntrinsicInst &)) {
    addMemberFnCase<llvm::IntrinsicInst>(detail::VisitorKey::intrinsic(id), fn);
    return *this;
  }

  VisitorBuilder &addIntrinsic(unsigned id,
                               void (PayloadT::*fn)(llvm::IntrinsicInst &)) {
    addMemberFnCase<llvm::IntrinsicInst>(detail::VisitorKey::intrinsic(id), fn);
    return *this;
  }

  template <typename NestedPayloadT>
  VisitorBuilder &nest(void (*registration)(VisitorBuilder<NestedPayloadT> &)) {
    if constexpr (detail::VisitorPayloadOffsetProjection<
                      PayloadT, NestedPayloadT>::useOffsetof) {
      size_t offset =
          detail::VisitorPayloadOffsetProjection<PayloadT,
                                                 NestedPayloadT>::offset;
      VisitorBuilder<NestedPayloadT> nested{this, offset};
      (*registration)(nested);
    } else {
      auto projection = [](void *payload) -> void * {
        return static_cast<void *>(
            &VisitorPayloadProjection<PayloadT, NestedPayloadT>::project(
                *static_cast<PayloadT *>(payload)));
      };

      VisitorBuilder<NestedPayloadT> nested{this, projection};
      (*registration)(nested);
    }

    return *this;
  }

private:
  VisitorBuilder(VisitorBuilderBase *parent, size_t offset)
      : VisitorBuilderBase(parent, offset) {}
  VisitorBuilder(VisitorBuilderBase *parent,
                 detail::PayloadProjectionCallback *projection)
      : VisitorBuilderBase(parent, projection) {}

  template <typename OpT, typename ReturnT>
  void addCase(detail::VisitorKey key, ReturnT (*fn)(PayloadT &, OpT &)) {
    detail::VisitorCallbackData data{};
    static_assert(sizeof(fn) <= sizeof(data.data));
    memcpy(&data.data, &fn, sizeof(fn));
    VisitorBuilderBase::add(key, &VisitorBuilder::forwarder<OpT, ReturnT>,
                            data);
  }

  template <typename ReturnT>
  void addSetCase(detail::VisitorKey key,
                  ReturnT (*fn)(PayloadT &, llvm::Instruction &)) {
    detail::VisitorCallbackData data{};
    static_assert(sizeof(fn) <= sizeof(data.data));
    memcpy(&data.data, &fn, sizeof(fn));
    VisitorBuilderBase::add(key, &VisitorBuilder::setForwarder<ReturnT>, data);
  }

  template <typename OpT, typename ReturnT>
  void addMemberFnCase(detail::VisitorKey key, ReturnT (PayloadT::*fn)(OpT &)) {
    detail::VisitorCallbackData data{};
    static_assert(sizeof(fn) <= sizeof(data.data));
    memcpy(&data.data, &fn, sizeof(fn));
    VisitorBuilderBase::add(
        key, &VisitorBuilder::memberFnForwarder<OpT, ReturnT>, data);
  }

  template <typename OpT, typename ReturnT>
  static VisitorResult forwarder(const detail::VisitorCallbackData &data,
                                 void *payload, llvm::Instruction *op) {
    ReturnT (*fn)(PayloadT &, OpT &);
    memcpy(&fn, &data.data, sizeof(fn));
    if constexpr (std::is_same_v<ReturnT, void>) {
      fn(*static_cast<PayloadT *>(payload), *llvm::cast<OpT>(op));
      return VisitorResult::Continue;
    } else {
      return fn(*static_cast<PayloadT *>(payload), *llvm::cast<OpT>(op));
    }
  }

  template <typename ReturnT>
  static VisitorResult setForwarder(const detail::VisitorCallbackData &data,
                                    void *payload, llvm::Instruction *op) {
    ReturnT (*fn)(PayloadT &, llvm::Instruction &);
    memcpy(&fn, &data.data, sizeof(fn));
    if constexpr (std::is_same_v<ReturnT, void>) {
      fn(*static_cast<PayloadT *>(payload), *op);
      return VisitorResult::Continue;
    } else {
      return fn(*static_cast<PayloadT *>(payload), *op);
    }
  }

  template <typename OpT, typename ReturnT>
  static VisitorResult
  memberFnForwarder(const detail::VisitorCallbackData &data, void *payload,
                    llvm::Instruction *op) {
    ReturnT (PayloadT::*fn)(OpT &);
    memcpy(&fn, &data.data, sizeof(fn));
    PayloadT *self = static_cast<PayloadT *>(payload);
    if constexpr (std::is_same_v<ReturnT, void>) {
      (self->*fn)(*llvm::cast<OpT>(op));
      return VisitorResult::Continue;
    } else {
      return (self->*fn)(*llvm::cast<OpT>(op));
    }
  }
};

} // namespace llvm_dialects
