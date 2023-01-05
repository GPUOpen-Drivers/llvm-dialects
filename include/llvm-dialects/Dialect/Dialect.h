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
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/TrailingObjects.h"

namespace llvm {
class CallInst;
class Function;
class LLVMContext;
} // namespace llvm

namespace llvm_dialects {

class Dialect;
class DialectContext;

struct DialectDescriptor {
  unsigned index;
  Dialect* (*make)(llvm::LLVMContext& context);
};

/// Base for dialect classes generated by TableGen. Dialect classes are used to define dialect sets
/// for setting up dialect-augmented LLVMContexts. An instance of the dialect classes is created
/// per LLVMContext for which the dialect is enabled.
class Dialect {
  friend DialectContext;

  llvm::LLVMContext& m_context;

protected:
  virtual void anchor();

  // There is a global instance of Key for every dialect class. Dialects have a globally unique ID
  // that is assigned at runtime.
  class Key {
    unsigned m_index;

  public:
    Key();
    ~Key();

    unsigned getIndex() const {return m_index;}

    static llvm::SmallVectorImpl<Key*>& getRegisteredKeys();
  };

  explicit Dialect(llvm::LLVMContext& context) : m_context(context) {}
  virtual ~Dialect() = default;

public:
  llvm::LLVMContext& getContext() const {return m_context;}
};

/// Augmentation of LLVMContext with zero or more dialects.
///
/// Users of this library must create a DialectContext together with the LLVMContext and then
/// guard all code using the pair of context using @ref withDialects.
///
/// @example
///   LLVMContext context;
///   auto dialectContext = DialectContext::make<Dialect1, Dialect2, ...>(context);
///   auto guard = withDialects(dialectContext);
///
class DialectContext final : private llvm::TrailingObjects<DialectContext, Dialect*> {
  friend llvm::TrailingObjects<DialectContext, Dialect*>;

  llvm::LLVMContext& m_llvmContext;
  unsigned m_dialectArraySize;

  DialectContext(llvm::LLVMContext& context, unsigned dialectArraySize);

  size_t numTrailingObjects(OverloadToken<Dialect*>) const {return m_dialectArraySize;}

public:
  ~DialectContext();

  void operator delete(void *ctx);

  // Placement deletes are called if the constructor throws (shouldn't happen,
  // but let's be thorough).
  void operator delete(void *ctx, unsigned) {
    DialectContext::operator delete(ctx);
  }
  void operator delete(void *ctx, unsigned, unsigned) {
    DialectContext::operator delete(ctx);
  }

  /// Get the DialectContext associated to the given LLVM context. This fails if no dialect context
  /// was created for the LLVM context.
  static DialectContext& get(llvm::LLVMContext& context);

  template <typename... DialectsT>
  static std::unique_ptr<DialectContext> make(llvm::LLVMContext& context) {
    std::array<DialectDescriptor, sizeof...(DialectsT)> descs{{DialectsT::getDescriptor()...}};
    return make(context, descs);
  }
  static std::unique_ptr<DialectContext> make(llvm::LLVMContext& context,
                                              llvm::ArrayRef<DialectDescriptor> dialects);

  llvm::LLVMContext& getContext() const {return m_llvmContext;}

  template <typename DialectT>
  DialectT& getDialect() const {
    Dialect* dialect = getTrailingObjects<Dialect*>()[DialectT::getIndex()];
    assert(dialect != nullptr);
    return *static_cast<DialectT*>(dialect);
  }

  template <typename DialectT>
  bool hasDialect() const {
    return getTrailingObjects<Dialect*>()[DialectT::getIndex()];
  }
};

class DialectContextGuard {
  DialectContext* m_dialectContext;

  DialectContextGuard(const DialectContextGuard&) = delete;
  DialectContextGuard& operator=(const DialectContextGuard&) = delete;
public:
  explicit DialectContextGuard(DialectContext& dialectContext);
  ~DialectContextGuard();

  DialectContextGuard(DialectContextGuard&& rhs) {
    *this = std::move(rhs);
  }
  DialectContextGuard& operator=(DialectContextGuard&& rhs) {
    if (this != &rhs) {
      m_dialectContext = rhs.m_dialectContext;
      rhs.m_dialectContext = nullptr;
    }
    return *this;
  }
};

[[nodiscard]] inline DialectContextGuard withDialects(DialectContext& dialectContext) {
  return DialectContextGuard{dialectContext};
}

/// CRTP helper for the TableGen-generated dialect classes.
template <typename DialectT>
class DialectImpl : public Dialect {
  friend class DialectContext;

  static unsigned getIndex() {return DialectT::getKey().getIndex();}

protected:
  explicit DialectImpl(llvm::LLVMContext& context) : Dialect(context) {}

public:
  static DialectT& get(llvm::LLVMContext& context) {
    return DialectContext::get(context).getDialect<DialectT>();
  }
  static bool hasDialect(llvm::LLVMContext& context) {
    return DialectContext::get(context).hasDialect<DialectT>();
  }
  static DialectDescriptor getDescriptor() {
    DialectDescriptor desc;
    desc.index = getIndex();
    desc.make = &DialectT::make;
    return desc;
  }
};

namespace detail {

bool isSimpleOperationDecl(const llvm::Function *fn, llvm::StringRef name);
bool isOverloadedOperationDecl(const llvm::Function *fn, llvm::StringRef name);

bool isSimpleOperation(const llvm::CallInst *i, llvm::StringRef name);
bool isOverloadedOperation(const llvm::CallInst *i, llvm::StringRef name);

} // namespae detail

} // namespace llvm_dialects
