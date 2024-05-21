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

#include "llvm-dialects/TableGen/NamedValue.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"

namespace llvm {
class Init;
class raw_ostream;
class Record;
} // namespace llvm

namespace llvm_dialects {

class GenDialectsContext;
class Predicate;

class Variable {
public:
  explicit Variable(llvm::StringRef name) : m_name(name) {}
  Variable(llvm::StringRef name, Predicate *predicate)
      : m_name(name), m_predicate(predicate) {}

  bool isNamed() const { return m_predicate == nullptr; }
  llvm::StringRef name() const { return m_name; }
  std::string toString() const;

private:
  /// The name of the variable.
  std::string m_name;

  /// The predicate in which the variable appears as an argument, if any.
  Predicate *m_predicate = nullptr;
};

class Scope {
public:
  Variable *findVariable(llvm::StringRef name) const;
  Variable *getVariable(llvm::StringRef name);

  Variable *createVariable(llvm::StringRef name, Predicate *predicate);

private:
  std::vector<std::unique_ptr<Variable>> m_variables;
  llvm::StringMap<Variable *> m_namedVariables;
};

class ConstraintSystem {
  friend class EvaluationPlanner;
  friend class Evaluator;

public:
  ConstraintSystem(GenDialectsContext &context, Scope &scope)
      : m_context(context), m_scope(scope) {}

  GenDialectsContext &getContext() const { return m_context; }

  llvm::ArrayRef<Variable *> variables() const { return m_variables; }

  bool addConstraint(llvm::raw_ostream &errs, llvm::Init *init, Variable *self);
  void merge(ConstraintSystem &&rhs);

  void print(llvm::raw_ostream &out, llvm::StringRef prefix) const;
  void dump() const;

private:
  bool addConstraintImpl(llvm::raw_ostream &errs, llvm::Init *init,
                         Variable *self);
  void addVariable(Variable *variable);
  void addGlobalVariable(Variable *variable);

  GenDialectsContext &m_context;
  Scope &m_scope;
  std::vector<std::unique_ptr<Constraint>> m_constraints;

  // List of all free variables that appear in the constraint system.
  std::vector<Variable *> m_variables;

  // List of all free variables that appear in the constraint system and that
  // may be referenced from the outside.
  std::vector<Variable *> m_globalVariables;
};

class Constraint {
  friend class ConstraintSystem;

public:
  enum class Kind : uint8_t {
    Apply,
    Logic_First,
    And = Logic_First,
    Or,
    Logic_Last = Or,
  };

  virtual ~Constraint() = default;

  virtual void print(llvm::raw_ostream &out, llvm::StringRef prefix) const = 0;

  Kind getKind() const { return m_kind; }
  llvm::Init *getInit() const { return m_init; }
  Variable *getSelf() const { return m_self; }
  llvm::ArrayRef<Variable *> variables() const { return m_variables; }

  std::string toString() const;

protected:
  Constraint(Kind kind) : m_kind(kind) {}

  void printVariables(llvm::raw_ostream &out) const;
  void addVariable(Variable *variable);

private:
  const Kind m_kind;

  /// Only for error messages: The TableGen init (if any) from which this
  /// constraint was derived.
  llvm::Init *m_init = nullptr;

  /// Only for error messages: The variable in the valued position that this
  /// constraint originally appeared in (if any).
  Variable *m_self = nullptr;

  /// The free variables that appear in the constraint.
  std::vector<Variable *> m_variables;
};

class Apply : public Constraint {
  friend class ConstraintSystem;

public:
  Apply() : Constraint(Kind::Apply) {}

  static bool classof(const Constraint *c) {
    return c->getKind() == Kind::Apply;
  }

  void print(llvm::raw_ostream &out, llvm::StringRef prefix) const override;

  Predicate *getPredicate() const { return m_predicate; }
  llvm::ArrayRef<Variable *> arguments() const { return m_arguments; }

private:
  Predicate *m_predicate;

  /// Arguments to the predicate. May be null if an argument is not constrained
  /// in any way (no name capture and no non-trivial constraint).
  std::vector<Variable *> m_arguments;
};

class LogicOr : public Constraint {
  friend class ConstraintSystem;

public:
  LogicOr() : Constraint(Kind::Or) {}

  static bool classof(const Constraint *c) { return c->getKind() == Kind::Or; }

  void print(llvm::raw_ostream &out, llvm::StringRef prefix) const override;

  llvm::ArrayRef<ConstraintSystem> branches() const { return m_branches; }
  llvm::ArrayRef<llvm::Init *> branchInits() const { return m_branchInits; }

private:
  std::vector<ConstraintSystem> m_branches;

  /// Only for error messages.
  std::vector<llvm::Init *> m_branchInits;
};

class MetaType {
public:
  enum class Kind { Type, Value, Attr, VarArgList };

  static MetaType *type();
  static MetaType *value();
  static MetaType *varargs();

  Kind getKind() const { return m_kind; }
  llvm::StringRef getName() const;
  llvm::StringRef getGetterCppType() const;
  llvm::StringRef getCppType() const;
  llvm::StringRef getBuilderCppType() const;
  static std::string printable(const MetaType *type, llvm::StringRef value);
  bool isTypeArg() const { return m_kind == Kind::Type; }
  bool isValueArg() const { return m_kind == Kind::Value; }
  bool isVarArgList() const { return m_kind == Kind::VarArgList; }
  bool isImmutable() const;

protected:
  MetaType(Kind kind) : m_kind(kind) {}

private:
  const Kind m_kind;
};

class Attr : public MetaType {
public:
  Attr() : MetaType(Kind::Attr) {}

  static bool classof(const MetaType *type) {
    return type->getKind() == Kind::Attr;
  }

  static std::unique_ptr<Attr> parse(llvm::raw_ostream &errs,
                                     GenDialectsContext &context,
                                     llvm::Record *record);

  llvm::StringRef getName() const;
  llvm::StringRef getCppType() const { return m_cppType; }
  llvm::Init *getLlvmType() const { return m_llvmType; }
  llvm::StringRef getToLlvmValue() const { return m_toLlvmValue; }
  llvm::StringRef getFromLlvmValue() const { return m_fromLlvmValue; }
  llvm::StringRef getToUnsigned() const { return m_toUnsigned; }
  llvm::StringRef getFromUnsigned() const { return m_fromUnsigned; }
  llvm::StringRef getCheck() const { return m_check; }
  bool getIsImmutable() const { return m_isImmutable; }

  // Set the LLVMType once -- used during initialization to break a circular
  // dependency in how IntegerType is defined.
  void setLlvmType(llvm::Init *llvmType) {
    assert(!m_llvmType);
    assert(llvmType);
    m_llvmType = llvmType;
  }

private:
  llvm::Record *m_record = nullptr;
  std::string m_cppType;
  llvm::Init *m_llvmType = nullptr;
  std::string m_toLlvmValue;
  std::string m_fromLlvmValue;
  std::string m_toUnsigned;
  std::string m_fromUnsigned;
  std::string m_check;
  bool m_isImmutable;
};

} // namespace llvm_dialects
