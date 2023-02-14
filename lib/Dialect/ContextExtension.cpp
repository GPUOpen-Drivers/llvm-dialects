/*
 * Copyright (c) 2023 Advanced Micro Devices, Inc. All Rights Reserved.
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

#include "llvm-dialects/Dialect/ContextExtension.h"

#include "llvm-dialects/Dialect/Dialect.h"

using namespace llvm;
using namespace llvm_dialects;
using namespace llvm_dialects::detail;

namespace {

SmallVectorImpl<ContextExtensionKey *> &getContextExtensionKeys() {
  static SmallVector<ContextExtensionKey *> keys;
  return keys;
}

} // anonymous namespace

void ContextExtensionKey::anchor() {}

ContextExtensionKey::ContextExtensionKey() {
  auto &keys = getContextExtensionKeys();

  for (m_index = 0; m_index < keys.size(); ++m_index) {
    if (!keys[m_index])
      break;
  }
  if (m_index < keys.size()) {
    keys[m_index] = this;
  } else {
    keys.push_back(this);
  }
}

ContextExtensionKey::~ContextExtensionKey() {
  auto &keys = getContextExtensionKeys();
  keys[m_index] = nullptr;
  m_index = std::numeric_limits<unsigned>::max();
}

ContextExtensionBase &ContextExtensionKey::get(llvm::LLVMContext &context) {
  return get(DialectContext::get(context));
}

ContextExtensionBase &ContextExtensionKey::get(DialectContext &context) {
  auto &slot = context.getExtensionSlot(m_index);
  if (!slot)
    slot = create(context.getContext());
  return *slot;
}

ArrayRef<ContextExtensionKey *> ContextExtensionKey::getKeys() {
  return getContextExtensionKeys();
}

void ContextExtensionBase::anchor() {}
