/*
 * Copyright (c) 2022-2023 Advanced Micro Devices, Inc. All Rights Reserved.
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

#include <unordered_set>

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"

namespace llvm_dialects {

/// Helper class for choosing unique variable names.
class SymbolTable {
  const SymbolTable *m_parent = nullptr;
  std::unordered_set<std::string> m_names;

public:
  SymbolTable() {}
  explicit SymbolTable(const SymbolTable *parent) : m_parent(parent) {}
  SymbolTable(SymbolTable &&) = default;
  SymbolTable &operator=(SymbolTable &&) = default;

  /// Add a name based on @p name that does not clash with any already existing
  /// names, and return it.
  std::string chooseName(llvm::StringRef name) {
    return chooseName(llvm::ArrayRef<llvm::StringRef>(name));
  }

  /// Add a name based on @p names that does not clash with any already existing
  /// names, and return it.
  ///
  /// Prefer earlier names in @p names if available, but use the last name for
  /// generating "last resort" names with numeric suffixes.
  std::string chooseName(llvm::ArrayRef<llvm::StringRef> names);

private:
  bool existsInAncestor(const std::string &name) const;

  // Prevent accidental copying.
  SymbolTable(const SymbolTable &) = delete;
  SymbolTable &operator=(const SymbolTable &) = delete;
};

class SymbolScope {
  SymbolTable **m_ptr = nullptr;
  SymbolTable *m_parent = nullptr;
  SymbolTable m_symbols;

public:
  SymbolScope(SymbolTable **ptr)
      : m_ptr(ptr), m_parent(*ptr), m_symbols(m_parent) {
    *m_ptr = &m_symbols;
  }

  ~SymbolScope() { *m_ptr = m_parent; }
};

} // namespace llvm_dialects
