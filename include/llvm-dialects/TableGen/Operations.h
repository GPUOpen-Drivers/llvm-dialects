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

#include "llvm-dialects/TableGen/Constraints.h"
#include "llvm-dialects/TableGen/Evaluator.h"
#include "llvm-dialects/TableGen/NamedValue.h"
#include "llvm-dialects/TableGen/SymbolTable.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"

namespace llvm {
class raw_ostream;
class Record;
} // namespace llvm

namespace llvm_dialects {

class BuilderMethod;
class FmtContext;
class GenDialectsContext;
class GenDialect;
class Operation;
class Trait;

struct OverloadKey {
  enum Kind {
    Result,
    Argument,
  };

  Kind kind;
  unsigned index;
};

class OpClass {
public:
  GenDialect *dialect = nullptr;
  OpClass *superclass = nullptr;
  std::string name;

  /// List of arguments specific to this class; does not contain superclass
  /// arguments, if any.
  std::vector<NamedValue> arguments;
  std::vector<OpClass *> subclasses;
  std::vector<Operation *> operations;

  static std::unique_ptr<OpClass> parse(llvm::raw_ostream &errs,
                                        GenDialectsContext &context,
                                        llvm::Record *record);

  llvm::SmallVector<NamedValue> getFullArguments() const;
  unsigned getNumFullArguments() const;
};

class Operation {
  friend class BuilderMethod;

public:
  OpClass *superclass = nullptr;
  std::string name;
  std::string mnemonic;
  std::vector<Trait *> traits;

  /// List of arguments specific to this operation; does not contain superclass
  /// arguments, if any.
  std::vector<NamedValue> arguments;
  std::vector<NamedValue> results;

  Operation(GenDialectsContext &context) : m_system(context, m_scope) {}
  ~Operation();

  static bool parse(llvm::raw_ostream &errs, GenDialectsContext *context,
                    GenDialect *dialect, llvm::Record *record);

  bool haveResultOverloads() const { return m_haveResultOverloads; }
  bool haveArgumentOverloads() const { return m_haveArgumentOverloads; }

  llvm::ArrayRef<BuilderMethod> builders() const { return m_builders; }

  llvm::SmallVector<NamedValue> getFullArguments() const;
  unsigned getNumFullArguments() const;

  int getAttributeListIdx() const { return m_attributeListIdx; }

  void emitVerifierMethod(llvm::raw_ostream &out, FmtContext &fmt) const;

private:
  friend class GenDialect;
  friend class GenDialectsContext;

  bool m_defaultBuilderHasExplicitResultType = false;

  bool m_haveResultOverloads = false;
  bool m_haveArgumentOverloads = false;

  Scope m_scope;
  ConstraintSystem m_system;
  std::vector<BuilderMethod> m_builders;

  /// -1 if the operation has no attribute list / has an empty attribute list.
  /// Otherwise, an index into the dialect's attribute list array.
  int m_attributeListIdx = -1;
};

class BuilderMethod {
  friend class Operation;

public:
  struct Arg {
    std::string cppType;
    std::string name;
  };

private:
  Operation &m_operation;
  SymbolTable m_symbolTable;
  std::string m_context;
  std::string m_builder;
  std::vector<Arg> m_arguments;
  unsigned m_beginOpArguments = 0;

  std::string m_prelude;
  std::string m_resultType;
  std::vector<std::string> m_attrTypes;

public:
  BuilderMethod(Operation &op) : m_operation(op) {}

  void emitDeclaration(llvm::raw_ostream &out, FmtContext &fmt) const;
  void emitDefinition(llvm::raw_ostream &out, FmtContext &fmt,
                      GenDialectsContext &genContext) const;
};

} // namespace llvm_dialects
