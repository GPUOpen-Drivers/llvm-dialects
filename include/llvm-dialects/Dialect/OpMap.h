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
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/IntrinsicInst.h"

#include <tuple>
#include <type_traits>

namespace llvm_dialects {

// Forward declarations.
template <typename ValueT> class OpMap;

template <typename, bool> class OpMapIteratorBase;

// OpMap implements a map-like container that can store core opcodes,
// intrinsics and dialect operations. It provides some lookup functionality for
// these kinds of operations, OpDescriptions and functions as well as
// instructions to simplify working with dialects in scenarios requiring
// association of dialect operations with certain values.
template <typename ValueT> class OpMap final {
  // We don't care about the value type in the @reserve member function,
  // thus we need to make the inners of OpMaps of arbitrary value type
  // accessible to a given OpMap instance.
  template <typename> friend class OpMap;

  friend class OpMapIteratorBase<ValueT, true>;
  friend class OpMapIteratorBase<ValueT, false>;

  using DialectOpKey = std::pair<llvm::StringRef, bool>;

  struct DialectOpKV final {
    DialectOpKey Key;
    ValueT Value;

    bool operator==(const DialectOpKV &other) const {
      return Key.first == other.Key.first && Key.second == other.Key.second &&
             Value == other.Value;
    }

    bool operator==(const OpDescription &desc) const {
      const bool isOverload =
          desc.getKind() == OpDescription::Kind::DialectWithOverloads;
      return Key.first == desc.getMnemonic() && Key.second == isOverload;
    }
  };

public:
  using iterator = OpMapIteratorBase<ValueT, false>;
  using const_iterator = OpMapIteratorBase<ValueT, true>;

  OpMap() = default;

  // --------------------------------------------------------------------------
  // Convenience constructor to initialize the OpMap from a set of
  // OpDescription/Value pairs.
  // --------------------------------------------------------------------------
  OpMap(std::initializer_list<std::pair<OpDescription, ValueT>> vals) {
    for (const std::pair<OpDescription, ValueT> &val : vals)
      insert(val.first, val.second);
  }

  // --------------------------------------------------------------------------
  // Comparison operator overloads.
  // --------------------------------------------------------------------------
  bool operator==(const OpMap &rhs) const {
    if (m_dialectOps.size() != rhs.m_dialectOps.size())
      return false;

    if (m_coreOpcodes == rhs.m_coreOpcodes &&
        m_intrinsics == rhs.m_intrinsics) {
      // Do a lookup for each vector entry, since both LHS and RHS potentially
      // are in different order.
      for (const auto &dialectOp : rhs.m_dialectOps) {
        if (std::find(m_dialectOps.begin(), m_dialectOps.end(), dialectOp) ==
            m_dialectOps.end())
          return false;
      }

      return true;
    }

    return false;
  }

  bool operator!=(const OpMap &rhs) const { return !(*this == rhs); }

  // --------------------------------------------------------------------------
  // contains checks if a given op is contained in any of the
  // internal map containers.
  // --------------------------------------------------------------------------

  bool containsCoreOp(unsigned op) const { return m_coreOpcodes.contains(op); }

  bool containsIntrinsic(unsigned op) const {
    return m_intrinsics.contains(op);
  }

  // Check if the map contains an OpDescription created for a given dialect
  // operation type.
  template <typename OpT> bool contains() const {
    static OpDescription desc = OpDescription::get<OpT>();
    return contains(desc);
  }

  // Check if the map contains an op described by an OpDescription.
  bool contains(const OpDescription &desc) const {
    if (desc.isCoreOp() || desc.isIntrinsic()) {
      assert(desc.getOpcodes().size() == 1 &&
             "OpMap only supports querying of single core opcodes and "
             "intrinsics.");

      const unsigned op = desc.getOpcode();
      return (desc.isCoreOp() && containsCoreOp(op)) ||
             (desc.isIntrinsic() && containsIntrinsic(op));
    }

    for (const DialectOpKV &dialectOpKV : m_dialectOps) {
      if (dialectOpKV == desc)
        return true;
    }

    return false;
  }

  // --------------------------------------------------------------------------
  // find returns an iterator that contains info about elements from one of the
  // internal map containers.
  // --------------------------------------------------------------------------

  // A simple DSL to simplify generating some of the find() overloads

#define GENERATE_FIND_BODY(iterator_t)                                         \
  {                                                                            \
    if (empty())                                                               \
      return end();                                                            \
    iterator_t it(this, arg);                                                  \
    if (it)                                                                    \
      return it;                                                               \
    return end();                                                              \
  }

#define FIND_OVERLOAD(arg_t)                                                   \
  iterator find(arg_t &arg) GENERATE_FIND_BODY(iterator)

#define FIND_CONST_OVERLOAD(arg_t)                                             \
  const_iterator find(const arg_t &arg) const GENERATE_FIND_BODY(const_iterator)

  FIND_OVERLOAD(OpDescription)
  FIND_CONST_OVERLOAD(OpDescription)
  FIND_OVERLOAD(llvm::Function)
  FIND_CONST_OVERLOAD(llvm::Function)
  FIND_OVERLOAD(llvm::Instruction)
  FIND_CONST_OVERLOAD(llvm::Instruction)

#undef FIND_CONST_OVERLOAD
#undef FIND_OVERLOAD
#undef GENERATE_FIND_BODY

  // --------------------------------------------------------------------------
  // Convenience getter definition.
  // --------------------------------------------------------------------------

  ValueT &operator[](const OpDescription &desc) {
    auto [it, inserted] = insert(desc, {});
    return (*it).second;
  }

  // --------------------------------------------------------------------------
  // lookup tries to find whether a given function or instruction
  // can be mapped to any of the entries in the internal map
  // containers.
  // It returns either a default-constructed object if the key
  // was not found or a copy of the contained value.
  // --------------------------------------------------------------------------

  // Try to lookup a function which is either the callee of an intrinsic call
  // or a dialect operation.
  ValueT lookup(const llvm::Function &func) const {
    auto it = find(func);
    if (auto val = it.val(); val)
      return *val;

    return {};
  }

  // Try to lookup an instruction which is either an intrinsic instruction,
  // a dialect operation or a core instruction.
  ValueT lookup(const llvm::Instruction &inst) const {
    auto it = find(inst);
    if (auto val = it.val(); val)
      return *val;

    return {};
  }

  // --------------------------------------------------------------------------
  // Try to construct a value in-place for a given OpDescription
  // and returns a pair which consists of the internal OpMapIterator and a
  // boolean return value, marking if the value was inserted or not. If the
  // OpDescription was already in the internal data structures, nothing will be
  // changed.
  // --------------------------------------------------------------------------
  template <typename... Ts>
  std::pair<iterator, bool> try_emplace(const OpDescription &desc,
                                        Ts &&...vals) {
    auto found = find(const_cast<OpDescription &>(desc));
    if (found)
      return {found, false};

    if (desc.isCoreOp() || desc.isIntrinsic()) {
      assert(desc.getOpcodes().size() == 1 &&
             "OpMap: Can only emplace a single op at a time.");

      const unsigned op = desc.getOpcode();
      if (desc.isCoreOp()) {
        auto [it, inserted] =
            m_coreOpcodes.try_emplace(op, std::forward<Ts>(vals)...);
        return {makeIterator(it, OpDescription::Kind::Core, false), inserted};
      }

      auto [it, inserted] =
          m_intrinsics.try_emplace(op, std::forward<Ts>(vals)...);
      return {makeIterator(it, OpDescription::Kind::Intrinsic, false),
              inserted};
    }

    // Find the iterator into the dialect ops.
    size_t Idx = 0;
    for (DialectOpKV &dialectOpKV : m_dialectOps) {
      if (dialectOpKV == desc) {
        auto it = m_dialectOps.begin();
        std::advance(it, Idx);

        return {makeIterator(it, OpDescription::Kind::Dialect, false), false};
      }

      ++Idx;
    }

    // If the entry doesn't exist, construct it and return an iterator to the
    // end of dialect ops.
    auto it =
        m_dialectOps.insert(m_dialectOps.end(), {getDialectMapKey(desc),
                                                 std::forward<Ts>(vals)...});
    return {makeIterator(it, OpDescription::Kind::Dialect, false), true};
  }

  template <typename OpT> std::pair<iterator, bool> insert(const ValueT &val) {
    const OpDescription desc = OpDescription::get<OpT>();
    return try_emplace(desc, val);
  }

  template <typename OpT> std::pair<iterator, bool> insert(ValueT &&val) {
    const OpDescription desc = OpDescription::get<OpT>();
    return try_emplace(desc, std::move(val));
  }

  std::pair<iterator, bool> insert(const OpDescription &desc,
                                   const ValueT &val) {
    return try_emplace(desc, val);
  }

  std::pair<iterator, bool> insert(const OpDescription &desc, ValueT &&val) {
    return try_emplace(desc, std::move(val));
  }

  // --------------------------------------------------------------------------
  // Erase a given operation from the correct container.
  // --------------------------------------------------------------------------

  // Erase a given dialect operation.
  template <typename OpT> bool erase() {
    const OpDescription desc = OpDescription::get<OpT>();
    return erase(const_cast<OpDescription &>(desc));
  }

  // Erase all the operations described by a given OpDescription.
  bool erase(OpDescription &desc) {
    iterator it = find(desc);
    if (!it)
      return false;

    return it.erase();
  }

  // --------------------------------------------------------------------------
  // Reserve a given number of elements for the maps.
  // --------------------------------------------------------------------------
  void reserve(size_t numCoreOps, size_t numIntrinsics, size_t numDialectOps) {
    m_coreOpcodes.reserve(numCoreOps);
    m_intrinsics.reserve(numIntrinsics);
    m_dialectOps.reserve(numDialectOps);
  }

  template <typename VT> void reserve(const OpMap<VT> &other) {
    m_coreOpcodes.reserve(other.m_coreOpcodes.size());
    m_intrinsics.reserve(other.m_intrinsics.size());
    m_dialectOps.reserve(other.m_dialectOps.size());
  }

  // --------------------------------------------------------------------------
  // Convenience helpers.
  // --------------------------------------------------------------------------
  size_t size() const {
    return m_coreOpcodes.size() + m_intrinsics.size() + m_dialectOps.size();
  }

  bool empty() const {
    return m_coreOpcodes.empty() && m_intrinsics.empty() &&
           m_dialectOps.empty();
  }

  // --------------------------------------------------------------------------
  // Iterator definitions.
  // --------------------------------------------------------------------------

#define GENERATE_ITERATOR_BODY(iterator_provider, name, isInvalid)             \
  {                                                                            \
    if (empty() && !isInvalid)                                                 \
      return end();                                                            \
    if (!m_coreOpcodes.empty())                                                \
      return iterator_provider(m_coreOpcodes.name(),                           \
                               OpDescription::Kind::Core, isInvalid);          \
    if (!m_intrinsics.empty())                                                 \
      return iterator_provider(m_intrinsics.name(),                            \
                               OpDescription::Kind::Intrinsic, isInvalid);     \
    return iterator_provider(m_dialectOps.name(),                              \
                             OpDescription::Kind::Dialect, isInvalid);         \
  }

#define DEFINE_NONCONST_ITERATOR(name, isInvalid)                              \
  inline iterator name() GENERATE_ITERATOR_BODY(makeIterator, name, isInvalid)

#define DEFINE_CONST_ITERATOR(name, isInvalid)                                 \
  inline const_iterator name()                                                 \
      const GENERATE_ITERATOR_BODY(makeConstIterator, name, isInvalid)

  DEFINE_NONCONST_ITERATOR(begin, false)
  DEFINE_NONCONST_ITERATOR(end, true)
  DEFINE_CONST_ITERATOR(begin, false)
  DEFINE_CONST_ITERATOR(end, true)

#undef DEFINE_NONCONST_ITERATOR
#undef DEFINE_CONST_ITERATOR
#undef GENERATE_ITERATOR_BODY

private:
  llvm::DenseMap<unsigned, ValueT> m_coreOpcodes;
  llvm::DenseMap<unsigned, ValueT> m_intrinsics;
  llvm::SmallVector<DialectOpKV, 1> m_dialectOps;

  template <typename... Args> iterator makeIterator(Args &&...args) {
    return iterator(this, std::forward<Args>(args)...);
  }

  template <typename... Args>
  const_iterator makeConstIterator(Args &&...args) const {
    return const_iterator(this, std::forward<Args>(args)...);
  }

  static DialectOpKey getDialectMapKey(const OpDescription &desc) {
    return {desc.getMnemonic(),
            desc.getKind() == OpDescription::Kind::DialectWithOverloads};
  }
};

/// A simple iterator operating on the internal data structures of the OpMap. It
/// uses separate storage and stores pointers to the elements of the internal
/// data structures.
/// It should be used with caution, since the iterators get invalidated after
/// inserting or erasing an element.
/// Note that iterating over an OpMap instance never guarantees the order of
/// insertion.
template <typename ValueT, bool isConst> class OpMapIteratorBase final {
  using OpMapT =
      std::conditional_t<isConst, const OpMap<ValueT>, OpMap<ValueT>>;
  using BaseIteratorT = std::conditional_t<
      isConst, typename llvm::DenseMap<unsigned, ValueT>::const_iterator,
      typename llvm::DenseMap<unsigned, ValueT>::iterator>;
  using DialectOpIteratorT = std::conditional_t<
      isConst,
      typename llvm::SmallVectorImpl<
          typename OpMapT::DialectOpKV>::const_iterator,
      typename llvm::SmallVectorImpl<typename OpMapT::DialectOpKV>::iterator>;

  using InternalValueT = std::conditional_t<isConst, const ValueT, ValueT>;

  friend class OpMap<ValueT>;
  friend class OpMapIteratorBase<ValueT, true>;
  friend class OpMapIteratorBase<ValueT, false>;

  class OpMapIteratorState final {
    OpMapIteratorBase &m_iterator;

    enum class IteratorState : uint8_t {
      CoreOp,
      Intrinsic,
      DialectOp,
      Invalid
    };

    bool isCoreOp() const { return m_iterator.m_desc.isCoreOp(); }

    bool isIntrinsic() const { return m_iterator.m_desc.isIntrinsic(); }

    bool isDialectOp() const { return m_iterator.m_desc.isDialectOp(); }

    IteratorState computeCurrentState() {
      const auto isValidIterator = [&](auto it, auto endIt) -> bool {
        return it != endIt;
      };

      if (isCoreOp() &&
          isValidIterator(std::get<BaseIteratorT>(m_iterator.m_iterator),
                          m_iterator.m_map->m_coreOpcodes.end())) {
        return IteratorState::CoreOp;
      }

      if (isIntrinsic() &&
          isValidIterator(std::get<BaseIteratorT>(m_iterator.m_iterator),
                          m_iterator.m_map->m_intrinsics.end())) {
        return IteratorState::Intrinsic;
      }

      if (isDialectOp() &&
          isValidIterator(std::get<DialectOpIteratorT>(m_iterator.m_iterator),
                          m_iterator.m_map->m_dialectOps.end())) {
        return IteratorState::DialectOp;
      }

      return IteratorState::Invalid;
    }

    // Compute a possible next state after iteration.
    IteratorState computeNextState(IteratorState currentState) {
      IteratorState nextState = currentState;

      if (nextState == IteratorState::CoreOp ||
          nextState == IteratorState::Intrinsic) {
        auto peek = std::get<BaseIteratorT>(m_iterator.m_iterator);
        std::advance(peek, 1);

        if (nextState == IteratorState::CoreOp) {
          if (peek == m_iterator.m_map->m_coreOpcodes.end()) {
            if (!m_iterator.m_map->m_intrinsics.empty())
              return IteratorState::Intrinsic;

            nextState = IteratorState::DialectOp;
          }
        }

        if (nextState == IteratorState::Intrinsic) {
          if (peek == m_iterator.m_map->m_intrinsics.end()) {
            if (!m_iterator.m_map->m_dialectOps.empty())
              return IteratorState::DialectOp;

            return IteratorState::Invalid;
          }
        }
      }

      if (nextState == IteratorState::DialectOp) {
        // If we are about to transition to a new state, we cannot assume that
        // the call to std::get will succeed.
        if (currentState != nextState)
          return !m_iterator.m_map->m_dialectOps.empty()
                     ? IteratorState::DialectOp
                     : IteratorState::Invalid;

        auto peek = std::get<DialectOpIteratorT>(m_iterator.m_iterator);
        std::advance(peek, 1);
        if (peek != m_iterator.m_map->m_dialectOps.end())
          return IteratorState::DialectOp;

        return IteratorState::Invalid;
      }

      return nextState;
    }

  public:
    OpMapIteratorState(OpMapIteratorBase &iterator) : m_iterator{iterator} {}

    void step() {
      auto currentState = computeCurrentState();
      auto nextState = computeNextState(currentState);

      if (currentState == nextState) {
        switch (currentState) {
        case IteratorState::CoreOp:
        case IteratorState::Intrinsic: {
          auto &it = std::get<BaseIteratorT>(m_iterator.m_iterator);
          ++it;
          if (currentState == IteratorState::CoreOp)
            m_iterator.m_desc = OpDescription::fromCoreOp(it->first);
          else
            m_iterator.m_desc = OpDescription::fromIntrinsic(it->first);

          break;
        }
        case IteratorState::DialectOp: {
          auto &it = std::get<DialectOpIteratorT>(m_iterator.m_iterator);
          ++it;

          m_iterator.m_desc = {it->Key.second, it->Key.first};
          break;
        }

        case IteratorState::Invalid:
          m_iterator.invalidate();
          break;
        }
      } else {
        transitionTo(nextState);
      }
    }

    void transitionTo(IteratorState nextState) {
      if (nextState == IteratorState::Intrinsic) {
        auto newIt = m_iterator.m_map->m_intrinsics.begin();
        m_iterator.m_iterator = newIt;

        m_iterator.m_desc = OpDescription::fromIntrinsic(newIt->first);
      } else if (nextState == IteratorState::DialectOp) {
        auto newIt = m_iterator.m_map->m_dialectOps.begin();
        m_iterator.m_iterator = newIt;

        m_iterator.m_desc = {newIt->Key.second, newIt->Key.first};
      } else {
        m_iterator.invalidate();
      }
    }
  };

  OpMapIteratorBase(OpMapT *map,
                    std::variant<BaseIteratorT, DialectOpIteratorT> it,
                    OpDescription::Kind kind, bool isInvalid = false)
      : m_map{map}, m_iterator{it}, m_isInvalid{isInvalid} {
    if (m_map->empty()) {
      invalidate();
      return;
    }

    refreshOpDescriptor(kind);
  }

  OpMapIteratorBase(OpMapT *map, const OpDescription &desc)
      : m_map{map}, m_desc{desc} {
    if (desc.isCoreOp() || desc.isIntrinsic()) {
      assert(desc.getOpcodes().size() == 1 &&
             "OpMapIterator only supports querying of single core opcodes and "
             "intrinsics.");

      const unsigned op = desc.getOpcode();

      if (desc.isCoreOp()) {
        m_iterator = map->m_coreOpcodes.find(op);
        if (std::get<BaseIteratorT>(m_iterator) == map->m_coreOpcodes.end())
          invalidate();
      } else {
        m_iterator = map->m_intrinsics.find(op);
        if (std::get<BaseIteratorT>(m_iterator) == map->m_intrinsics.end())
          invalidate();
      }
    } else {
      createFromDialectOp(desc.getMnemonic());
    }
  }

  OpMapIteratorBase(OpMapT *map, const llvm::Function &func) : m_map{map} {
    createFromFunc(func);
  }

  // Do a lookup for a given instruction. Mark the iterator as invalid
  // if the instruction is a call-like core instruction.
  OpMapIteratorBase(OpMapT *map, const llvm::Instruction &inst) : m_map{map} {
    if (auto *CI = llvm::dyn_cast<llvm::CallInst>(&inst)) {
      const llvm::Function *callee = CI->getCalledFunction();
      if (callee) {
        createFromFunc(*callee);
        return;
      }
    }

    const unsigned op = inst.getOpcode();

    // Construct an invalid iterator.
    if (op == llvm::Instruction::Call || op == llvm::Instruction::CallBr) {
      invalidate();
      return;
    }

    BaseIteratorT it = m_map->m_coreOpcodes.find(op);
    if (it != m_map->m_coreOpcodes.end()) {
      m_desc = OpDescription::fromCoreOp(op);
      m_iterator = it;
    } else {
      invalidate();
    }
  }

public:
  std::pair<OpDescription, InternalValueT &> operator*() {
    return {m_desc, *val()};
  }

  InternalValueT *val() {
    assert(this->operator bool() &&
           "Trying to call val() on invalid OpMapIterator!");

    if (m_desc.isCoreOp() || m_desc.isIntrinsic())
      return std::addressof(std::get<BaseIteratorT>(m_iterator)->second);

    return std::addressof(std::get<DialectOpIteratorT>(m_iterator)->Value);
  }

  operator bool() const { return !m_isInvalid; }

  OpMapIteratorBase &operator++() {
    OpMapIteratorState stateMachine{*this};
    stateMachine.step();

    return *this;
  }

  OpMapIteratorBase &operator++(int) { return this->operator++(); }

  template <bool Proxy = isConst, typename = std::enable_if_t<!Proxy>>
  bool erase() {
    if (m_desc.isCoreOp() || m_desc.isIntrinsic()) {
      assert(m_desc.getOpcodes().size() == 1 &&
             "OpMapIterator only supports erasing of single core opcodes and "
             "intrinsics.");

      const unsigned op = m_desc.getOpcode();

      if (m_desc.isCoreOp())
        return m_map->m_coreOpcodes.erase(op);

      return m_map->m_intrinsics.erase(op);
    }

    // Try to erase the dialect op at last.
    for (size_t I = 0; I < m_map->m_dialectOps.size(); ++I) {
      if (m_map->m_dialectOps[I] == m_desc) {
        DialectOpIteratorT it = m_map->m_dialectOps.begin();
        std::advance(it, I);

        if (it == m_map->m_dialectOps.end())
          return false;

        m_map->m_dialectOps.erase(it);
        return true;
      }
    }

    return false;
  }

protected:
  OpMapT *m_map = nullptr;
  OpDescription m_desc;
  std::variant<BaseIteratorT, DialectOpIteratorT> m_iterator;
  bool m_isInvalid = false;

private:
  void invalidate() { m_isInvalid = true; }

  void createFromFunc(const llvm::Function &func) {
    if (func.isIntrinsic()) {
      m_iterator = m_map->m_intrinsics.find(func.getIntrinsicID());

      if (std::get<BaseIteratorT>(m_iterator) != m_map->m_intrinsics.end()) {
        m_desc = OpDescription::fromIntrinsic(func.getIntrinsicID());
        return;
      }
    }

    createFromDialectOp(func.getName());
  }

  void createFromDialectOp(llvm::StringRef funcName) {
    size_t idx = 0;
    bool found = false;
    for (auto &dialectOpKV : m_map->m_dialectOps) {
      const auto &key = dialectOpKV.Key;
      if (detail::isOperationDecl(funcName, key.second, key.first)) {
        m_desc = {key.second, key.first};
        auto it = m_map->m_dialectOps.begin();
        std::advance(it, idx);
        m_iterator = it;
        found = true;
        break;
      }

      ++idx;
    }

    if (!found)
      invalidate();
  }

  // Re-construct base OpDescription from the stored iterator.
  // Since this is invoked when passing an existing iterator to the
  // OpMapIterator constructor, we need to check the original kind as well
  // to prevent epoch mismatches when comparing the stored iterator with the
  // internal map data structures.
  void refreshOpDescriptor(OpDescription::Kind kind) {
    if (m_isInvalid)
      return;

    if (auto baseIt = std::get_if<BaseIteratorT>(&m_iterator)) {
      auto &unwrapped = *baseIt;
      if (!m_map->m_coreOpcodes.empty() && kind == OpDescription::Kind::Core &&
          unwrapped != m_map->m_coreOpcodes.end()) {
        m_desc = OpDescription::fromCoreOp(unwrapped->first);
      } else if (!m_map->m_intrinsics.empty() &&
                 kind == OpDescription::Kind::Intrinsic &&
                 unwrapped != m_map->m_intrinsics.end()) {
        m_desc = OpDescription::fromIntrinsic(unwrapped->first);
      } else {
        llvm_unreachable("OpMapIterator: Invalid iterator provided!");
      }
    } else if (auto dialectOpIt =
                   std::get_if<DialectOpIteratorT>(&m_iterator)) {
      auto &unwrapped = *dialectOpIt;
      if (unwrapped != m_map->m_dialectOps.end())
        m_desc = {unwrapped->Key.second, unwrapped->Key.first};
    } else {
      llvm_unreachable("OpMapIterator: Invalid iterator provided!");
    }
  }
};

} // namespace llvm_dialects
