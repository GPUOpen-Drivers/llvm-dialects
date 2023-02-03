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

#include "llvm-dialects/TableGen/SymbolTable.h"

#include "llvm/Support/FormatVariadic.h"

using namespace llvm;
using namespace llvm_dialects;

std::string SymbolTable::chooseName(ArrayRef<StringRef> names) {
  for (StringRef name : names) {
    std::string copy = name.str();

    if (existsInAncestor(copy))
      continue;

    auto insert = m_names.insert(copy);
    if (insert.second)
    return *insert.first;
  }

  for (int i = 0;; ++i) {
    std::string alternate = llvm::formatv("{0}_{1}", names.back(), i).str();
    if (existsInAncestor(alternate))
      continue;
    auto insert = m_names.insert(alternate);
    if (insert.second)
      return *insert.first;
  }
}

bool SymbolTable::existsInAncestor(const std::string &name) const {
  for (const SymbolTable *parent = m_parent; parent; parent = parent->m_parent) {
    if (parent->m_names.find(name) != parent->m_names.end()) {
      return true;
    }
  }
  return false;
}
