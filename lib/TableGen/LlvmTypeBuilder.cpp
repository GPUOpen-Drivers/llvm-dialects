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

#include "llvm-dialects/TableGen/LlvmTypeBuilder.h"

#include "llvm-dialects/TableGen/Constraints.h"
#include "llvm-dialects/TableGen/SymbolTable.h"

#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llvm_dialects;

std::string LlvmTypeBuilder::build(Constraint* constraint) {
  if (auto* attr = dyn_cast<Attr>(constraint))
    constraint = attr->getLlvmType();

  std::string& varName = m_constraintTypeVarName[constraint];
  if (varName.empty()) {
    auto* type = cast<Type>(constraint);
    varName = m_symbols.chooseName(type->getName());

    m_out << "llvm::Type* " << varName << " = " << type->getLlvmType(&m_fmt)
          << ";\n";
  }
  return varName;
}
