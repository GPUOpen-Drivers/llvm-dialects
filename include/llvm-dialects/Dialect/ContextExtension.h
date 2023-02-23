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

#pragma once

#include "llvm/ADT/ArrayRef.h"

namespace llvm {
class LLVMContext;
} // namespace llvm

namespace llvm_dialects {

class ContextExtensionBase;
class DialectContext;

namespace detail {

class ContextExtensionKey {
  unsigned m_index;

  virtual void anchor();

public:
  ContextExtensionKey();
  virtual ~ContextExtensionKey();

  unsigned getIndex() const { return m_index; }

  ContextExtensionBase &get(llvm::LLVMContext &context);
  ContextExtensionBase &get(DialectContext &context);

  static llvm::ArrayRef<ContextExtensionKey *> getKeys();

protected:
  virtual std::unique_ptr<ContextExtensionBase>
  create(llvm::LLVMContext &context) = 0;
};

} // namespace detail

class ContextExtensionBase {
  virtual void anchor();

public:
  virtual ~ContextExtensionBase() = default;
};

/// CRTP base class for extended data that can be attached to LLVMContexts.
///
/// In fact, extended data is currently attached to the DialectContext, but
/// conceptually this feature is independent of dialect support.
///
/// Extended data types must derive from this template base using the following
/// pattern:
///
///   class MyExtension : public ContextExtensionImpl<MyExtension> {
///   public:
///     MyExtension(llvm::LLVMContext &context);
///
///     static Key theKey; // instantiate this in a source file
///   };
///
/// Extension data can be retrieved using the static method call
/// MyExtension::get(llvmContext).
///
/// The extension data is allocated and constructed the first time it is
/// requested. The destructor is called when the DialectContext is destroyed.
///
/// Note that all context extension types must be present (their keys having
/// registered themselves) _before_ creating the DialectContext with which they
/// are going to be used (this restriction only really matters when dynamic
/// linking is used: it is not possible to dynamically load an extension after
/// a context is created, and then use the extension with that context).
template <typename ContextExtensionT>
class ContextExtensionImpl : public ContextExtensionBase {
public:
  class Key : public detail::ContextExtensionKey {
  protected:
    std::unique_ptr<ContextExtensionBase>
    create(llvm::LLVMContext &context) override {
      return std::make_unique<ContextExtensionT>(context);
    }
  };

  static ContextExtensionT &get(llvm::LLVMContext &context) {
    // Assign to an explicitly typed variable for type checking at template
    // instantiation time.
    Key &key = ContextExtensionT::theKey;
    return static_cast<ContextExtensionT &>(key.get(context));
  }

  static ContextExtensionT &get(DialectContext &context) {
    // Assign to an explicitly typed variable for type checking at template
    // instantiation time.
    Key &key = ContextExtensionT::theKey;
    return static_cast<ContextExtensionT &>(key.get(context));
  }
};

} // namespace llvm_dialects
