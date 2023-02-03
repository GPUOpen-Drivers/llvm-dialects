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

#include "llvm/ADT/DenseMap.h"

namespace llvm {
class raw_ostream;
}

namespace llvm_dialects {

class Constraint;
class FmtContext;
class SymbolTable;

/// Helper class for emitting code that gets or builds llvm::Type*'s from
/// constraints.
class LlvmTypeBuilder {
  SymbolTable& m_symbols;
  llvm::raw_ostream& m_out;
  llvm::DenseMap<Constraint*, std::string> m_constraintTypeVarName;
  FmtContext& m_fmt;

public:
  LlvmTypeBuilder(llvm::raw_ostream& out, SymbolTable& symbols, FmtContext& fmt)
      : m_symbols(symbols), m_out(out), m_fmt(fmt) {
  }

  std::string build(Constraint* constraint);
};

} // namespace llvm_dialects
