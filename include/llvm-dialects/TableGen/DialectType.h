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

#include "llvm-dialects/TableGen/Predicates.h"

#include "llvm-dialects/TableGen/SymbolTable.h"

namespace llvm_dialects {

class GenDialect;

class DialectType : public BaseCppPredicate {
public:
  DialectType(GenDialectsContext &context)
      : BaseCppPredicate(Kind::DialectType), m_system(context, m_scope) {}

  static bool classof(const Predicate *o) {
    return o->getKind() == Kind::DialectType;
  }

  bool init(llvm::raw_ostream &errs, GenDialectsContext &context,
            llvm::Init *theInit) override final;

  llvm::ArrayRef<NamedValue> typeArguments() const {
    return arguments().drop_front(1);
  }

  llvm::Record *getDialectRec() const { return m_dialectRec; }
  llvm::StringRef getName() const { return m_name; }
  llvm::StringRef getMnemonic() const { return m_mnemonic; }
  bool defaultGetterHasExplicitContextArgument() const {
    return m_defaultGetterHasExplicitContextArgument;
  }
  llvm::StringRef getSummary() const { return m_summary; }
  llvm::StringRef getDescription() const { return m_description; }

  void emitDeclaration(llvm::raw_ostream &out, GenDialect *dialect) const;
  void emitDefinition(llvm::raw_ostream &out, GenDialect *dialect) const;

private:
  struct GetterArg {
    std::string cppType;
    std::string name;
  };

  llvm::Record *m_dialectRec = nullptr;
  std::string m_name;
  std::string m_mnemonic;
  bool m_defaultGetterHasExplicitContextArgument = false;
  std::string m_summary;
  std::string m_description;

  Scope m_scope;
  ConstraintSystem m_system;
  SymbolTable m_symbols;
  std::string m_prelude;
  std::string m_context;
  std::vector<GetterArg> m_getterArguments;
  unsigned m_argBegin = 0;
};

} // namespace llvm_dialects
