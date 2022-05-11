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

#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/Instructions.h"

using namespace llvm_dialects;
using namespace llvm;

namespace {

thread_local DialectContext* s_currentContext = nullptr;

} // anonymous namespace

void Dialect::anchor() {}

SmallVectorImpl<Dialect::Key*>& Dialect::Key::getRegisteredKeys() {
  static SmallVector<Dialect::Key*> keys;
  return keys;
}

Dialect::Key::Key() {
  auto& keys = getRegisteredKeys();

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

DialectContext::DialectContext(llvm::LLVMContext& context, unsigned dialectArraySize)
    : m_llvmContext(context), m_dialectArraySize(dialectArraySize) {
}

DialectContext::~DialectContext() {
  assert(s_currentContext != this);

  Dialect** dialectArray = getTrailingObjects<Dialect*>();
  for (unsigned i = 0; i < m_dialectArraySize; ++i)
    delete dialectArray[i]; // may be nullptr
}

std::unique_ptr<DialectContext> DialectContext::make(LLVMContext& context,
                                                     ArrayRef<DialectDescriptor> dialects) {
  unsigned dialectArraySize = 0;
  for (const auto& desc : dialects)
    dialectArraySize = std::max(dialectArraySize, desc.index + 1);

  size_t totalSize = totalSizeToAlloc<Dialect*>(dialectArraySize);
  void* ptr = malloc(totalSize);

  std::unique_ptr<DialectContext> result{new (ptr) DialectContext(context, dialectArraySize)};
  Dialect** dialectArray = result->getTrailingObjects<Dialect*>();
  std::uninitialized_fill_n(dialectArray, dialectArraySize, nullptr);

  for (const auto& desc : dialects)
    dialectArray[desc.index] = desc.make(context);

  return std::move(result);
}

DialectContext& DialectContext::get(llvm::LLVMContext& context) {
  assert(s_currentContext);
  assert(&s_currentContext->m_llvmContext == &context);
  return *s_currentContext;
}

DialectContextGuard::DialectContextGuard(DialectContext& dialectContext) {
  assert(!s_currentContext);
  m_dialectContext = &dialectContext;
  s_currentContext = m_dialectContext;
}

DialectContextGuard::~DialectContextGuard() {
  if (m_dialectContext) {
    assert(m_dialectContext == s_currentContext);
    s_currentContext = nullptr;
  }
}

bool llvm_dialects::detail::isSimpleOperation(const CallInst *i, StringRef name) {
  if (auto* fn = i->getCalledFunction())
    return fn->getName() == name;
  return false;
}

bool llvm_dialects::detail::isOverloadedOperation(const CallInst *i, StringRef name) {
  if (auto *fn = i->getCalledFunction()) {
    StringRef fnName = fn->getName();
    if (name.size() >= fnName.size())
      return false;
    if (!fnName.startswith(name))
      return false;
    return fnName[name.size()] == '.';
  }
  return false;
}
