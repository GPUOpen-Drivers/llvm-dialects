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

#include "llvm-dialects/Dialect/Dialect.h"

#include "llvm-dialects/Dialect/ContextExtension.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/Instructions.h"

#include <atomic>
#include <mutex>

using namespace llvm;
using namespace llvm_dialects;
using namespace llvm_dialects::detail;

namespace {

class CurrentContextCache;

/// Singleton class that maintains a global map of LLVMContexts to
/// DialectContexts
class ContextMap {
  friend class CurrentContextCache;

  std::mutex m_mutex;
  DenseMap<LLVMContext *, DialectContext *> m_map;
  CurrentContextCache *m_caches = nullptr;

public:
  static ContextMap &get() {
    static ContextMap theMap;
    return theMap;
  }

  void insert(LLVMContext *llvmContext, DialectContext *dialectContext);
  void remove(LLVMContext *llvmContext, DialectContext *dialectContext);
};

/// Thread-local cache that caches the LLVMContext to DialectContext mapping.
///
/// Since normal code only uses a single context per thread, at least for a
/// significant amount of time, this allows a quick lookup without mutex
/// locking overhead.
class CurrentContextCache {
  friend class ContextMap;

  std::atomic<LLVMContext *> m_llvmContext = nullptr;
  DialectContext *m_dialectContext = nullptr;
  CurrentContextCache **m_pprev;
  CurrentContextCache *m_next = nullptr;

  CurrentContextCache() {
    auto &map = ContextMap::get();
    std::lock_guard<std::mutex> lock(map.m_mutex);
    m_next = map.m_caches;
    if (m_next)
      m_next->m_pprev = &m_next;
    m_pprev = &map.m_caches;
    map.m_caches = this;
  }

  ~CurrentContextCache() {
    auto &map = ContextMap::get();
    std::lock_guard<std::mutex> lock(map.m_mutex);
    if (m_next)
      m_next->m_pprev = m_pprev;
    *m_pprev = m_next;
  }

public:
  static DialectContext *get(LLVMContext *llvmContext) {
    assert(llvmContext != nullptr);
    static thread_local CurrentContextCache cache;
    if (cache.m_llvmContext.load(std::memory_order_relaxed) != llvmContext) {
      auto &map = ContextMap::get();
      std::lock_guard<std::mutex> lock(map.m_mutex);
      cache.m_llvmContext.store(llvmContext, std::memory_order_relaxed);
      cache.m_dialectContext = map.m_map.lookup(llvmContext);
    }
    assert(cache.m_dialectContext &&
           "No DialectContext. Maybe you forgot to use "
           "llvm_dialects::DialectContext::make.");
    return cache.m_dialectContext;
  }
};

void ContextMap::insert(LLVMContext *llvmContext,
                        DialectContext *dialectContext) {
  std::lock_guard<std::mutex> lock(m_mutex);
  bool inserted = m_map.try_emplace(llvmContext, dialectContext).second;
  (void)inserted;
  assert(inserted);
}

void ContextMap::remove(LLVMContext *llvmContext,
                        DialectContext *dialectContext) {
  std::lock_guard<std::mutex> lock(m_mutex);
  assert(m_map.lookup(llvmContext) == dialectContext);
  m_map.erase(llvmContext);

  // Remove any stale per-thread cache entries.
  //
  // This is called when llvmContext still exists, and our thread destroys it
  // (or at least detaches the dialectContext). No other thread can legitimately
  // attempt to do anything with the same llvmContext at the same time.
  //
  // However, another thread may have previously used llvmContext and still
  // see it in its cache. We need to null out those cache entries in case a
  // new LLVMContext is created at the exact same address.
  //
  // The other thread may race us in an attempt to start using a *different*
  // context. All *writes* to CurrentContextCache::m_llvmContext are guarded by
  // ContextMap::m_mutex. But there is still a race between
  //
  //  1. Our thread writing to m_llvmContext and
  //  2. The other thread checking m_llvmContext from CurrentContextCache::get
  //
  // This race is why we use std::atomic. Using std::atomic guarantees that the
  // other thread sees either nullptr or llvmContext, either of which causes it
  // to fail the cache lookup and use the slow path, where it will lock m_mutex
  // before updating m_llvmContext.
  //
  // The other thread may also eventually attempt to start using the *same*
  // LLVMContext again, or a re-created LLVMContext that happens to be allocated
  // at the same address. However, our thread must currently have exclusive
  // ownership of the LLVMContext (by the usual rules that an LLVMContext can
  // only be used from a single thread at a time), and this ownership can only
  // be transferred via external synchronization:
  //
  //  * either explicitly by an application-level synchronization
  //  * or implicitly, via both the malloc implementation and (in case there
  //    are doubts about what exactly the malloc implementation guarantees) also
  //    the fact that the DialectContext constructor call for the newly created
  //    LLVMContext takes the m_mutex look when it calls ContextMap::insert()
  for (CurrentContextCache *cache = m_caches; cache; cache = cache->m_next) {
    if (cache->m_llvmContext.load(std::memory_order_relaxed) == llvmContext)
      cache->m_llvmContext.store(nullptr, std::memory_order_relaxed);
  }
}

} // anonymous namespace

void Dialect::anchor() {}

SmallVectorImpl<Dialect::Key *> &Dialect::Key::getRegisteredKeys() {
  static SmallVector<Dialect::Key *> keys;
  return keys;
}

Dialect::Key::Key() {
  auto &keys = getRegisteredKeys();

  for (auto enumeratedKey : llvm::enumerate(keys)) {
    if (!enumeratedKey.value()) {
      enumeratedKey.value() = this;
      m_index = enumeratedKey.index();
      goto inserted;
    }
  }
  m_index = keys.size();
  keys.push_back(this);
inserted:;
}

Dialect::Key::~Key() {
  getRegisteredKeys()[m_index] = nullptr;
  m_index = std::numeric_limits<unsigned>::max();
}

DialectContext::DialectContext(LLVMContext &context, unsigned dialectArraySize,
                               unsigned extensionArraySize)
    : m_llvmContext(context), m_dialectArraySize(dialectArraySize),
      m_extensionArraySize(extensionArraySize) {
  ContextMap::get().insert(&context, this);
}

DialectContext::~DialectContext() {
  ContextMap::get().remove(&m_llvmContext, this);

  std::unique_ptr<ContextExtensionBase> *extensionArray =
      getTrailingObjects<std::unique_ptr<ContextExtensionBase>>();
  for (unsigned i = 0; i < m_extensionArraySize; ++i)
    std::destroy_n(extensionArray, m_extensionArraySize);

  Dialect **dialectArray = getTrailingObjects<Dialect *>();
  for (unsigned i = 0; i < m_dialectArraySize; ++i)
    delete dialectArray[i]; // may be nullptr
}

void DialectContext::operator delete(void *ctx) { free(ctx); }

std::unique_ptr<DialectContext>
DialectContext::make(LLVMContext &context,
                     ArrayRef<DialectDescriptor> dialects) {
  unsigned dialectArraySize = 0;
  for (const auto &desc : dialects)
    dialectArraySize = std::max(dialectArraySize, desc.index + 1);

  unsigned extensionArraySize = detail::ContextExtensionKey::getKeys().size();

  size_t totalSize =
      totalSizeToAlloc<Dialect *, std::unique_ptr<ContextExtensionBase>>(
          dialectArraySize, extensionArraySize);
  void *ptr = malloc(totalSize);

  std::unique_ptr<DialectContext> result{
      new (ptr) DialectContext(context, dialectArraySize, extensionArraySize)};
  Dialect **dialectArray = result->getTrailingObjects<Dialect *>();
  std::uninitialized_fill_n(dialectArray, dialectArraySize, nullptr);

  for (const auto &desc : dialects)
    dialectArray[desc.index] = desc.make(context);

  auto *extensionArray =
      result->getTrailingObjects<std::unique_ptr<ContextExtensionBase>>();
  std::uninitialized_default_construct_n(extensionArray, extensionArraySize);

  return result;
}

DialectContext &DialectContext::get(LLVMContext &context) {
  return *CurrentContextCache::get(&context);
}

void DialectExtensionPointBase::add(unsigned index, const void *extension) {
  if (index >= m_extensions.size())
    m_extensions.resize(index + 1, nullptr);
  assert(m_extensions[index] == nullptr);
  m_extensions[index] = extension;
}

void DialectExtensionPointBase::clear(unsigned index) {
  assert(index < m_extensions.size() && m_extensions[index] != nullptr);
  m_extensions[index] = nullptr;
}

bool llvm_dialects::detail::isSimpleOperationDecl(const Function *fn,
                                                  StringRef mnemonic) {
  return isOperationDecl(fn->getName(), false, mnemonic);
}

bool llvm_dialects::detail::isOverloadedOperationDecl(const Function *fn,
                                                      StringRef mnemonic) {
  return isOperationDecl(fn->getName(), true, mnemonic);
}

bool llvm_dialects::detail::isSimpleOperation(const CallInst *i,
                                              StringRef mnemonic) {
  if (auto *fn = i->getCalledFunction())
    return isSimpleOperationDecl(fn, mnemonic);
  return false;
}

bool llvm_dialects::detail::isOverloadedOperation(const CallInst *i,
                                                  StringRef mnemonic) {
  if (auto *fn = i->getCalledFunction())
    return isOverloadedOperationDecl(fn, mnemonic);
  return false;
}

bool llvm_dialects::detail::isOperationDecl(llvm::StringRef fn,
                                            bool isOverloaded,
                                            llvm::StringRef mnemonic) {
  if (isOverloaded) {
    if (mnemonic.size() >= fn.size())
      return false;
    if (!fn.startswith(mnemonic))
      return false;

    return fn[mnemonic.size()] == '.';
  }

  return fn == mnemonic;
}
