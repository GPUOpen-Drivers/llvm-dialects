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

#include "llvm-dialects/TableGen/Constraints.h"

#include "llvm-dialects/TableGen/Dialects.h"
#include "llvm-dialects/TableGen/Evaluator.h"
#include "llvm-dialects/TableGen/Format.h"
#include "llvm-dialects/TableGen/Predicates.h"

#include "llvm/Support/Debug.h"
#include "llvm/TableGen/Record.h"

using namespace llvm;
using namespace llvm_dialects;

std::string Variable::toString() const {
  if (!m_predicate)
    return (Twine("$") + m_name).str();
  return (Twine(m_predicate->getInit()->getAsString()) + ":$" + m_name).str();
}

Variable *Scope::findVariable(llvm::StringRef name) const {
  assert(!name.empty());
  auto it = m_namedVariables.find(name);
  if (it == m_namedVariables.end())
    return nullptr;
  return it->second;
}

Variable *Scope::getVariable(llvm::StringRef name) {
  assert(!name.empty());
  Variable *variable = findVariable(name);
  if (variable)
    return variable;

  auto owner = std::make_unique<Variable>(name);
  variable = owner.get();
  m_variables.push_back(std::move(owner));
  m_namedVariables.try_emplace(variable->name(), variable);
  return variable;
}

Variable *Scope::createVariable(StringRef name, Predicate *predicate) {
  auto owner = std::make_unique<Variable>(name, predicate);
  Variable *variable = owner.get();
  m_variables.push_back(std::move(owner));
  return variable;
}

void ConstraintSystem::print(raw_ostream &out, StringRef prefix) const {
  for (const auto &constraintPtr : m_constraints)
    constraintPtr->print(out, prefix);

  out << prefix << "free variables:";
  for (Variable *variable : m_variables)
    out << ' ' << variable->toString();
  out << '\n';

  out << prefix << "free global variables:";
  for (Variable *variable : m_globalVariables)
    out << ' ' << variable->toString();
  out << '\n';
}

void ConstraintSystem::dump() const {
  dbgs() << "Constraint system:\n";
  print(dbgs(), "  ");
}

bool ConstraintSystem::addConstraint(raw_ostream &errs, Init *init,
                                     Variable *self) {
  if (!addConstraintImpl(errs, init, self)) {
    errs << ".. in " << init->getAsString() << '\n';
    return false;
  }
  return true;
}

bool ConstraintSystem::addConstraintImpl(raw_ostream &errs, Init *init,
                                         Variable *self) {
  if (auto *dag = dyn_cast<DagInit>(init)) {
    Record *op = dag->getOperatorAsDef({});

    auto isValidOperand = [&dag, &errs](size_t index,
                                        StringRef opName) -> bool {
      if (!dag->getArgNameStr(index).empty()) {
        errs << "'" << opName << "' operands cannot be captured\n";
        return false;
      }

      if (!dag->getArg(index)) {
        errs << "'" << opName << "' operand is missing\n";
        return false;
      }

      return true;
    };

    if (op->getName() == "and") {
      for (size_t i = 0; i < dag->getNumArgs(); ++i) {
        if (!isValidOperand(i, op->getName()))
          return false;

        Init *operand = dag->getArg(i);
        if (!addConstraint(errs, operand, self))
          return false;
      }

      return true;
    }

    if (op->getName() == "or") {
      auto result = std::make_unique<LogicOr>();
      result->m_init = init;
      result->m_self = self;

      if (self && !dag->arg_empty()) {
        result->addVariable(self);
        addVariable(self);
        addGlobalVariable(self);
      }

      for (size_t i = 0; i < dag->getNumArgs(); ++i) {
        if (!isValidOperand(i, op->getName()))
          return false;

        Init *operand = dag->getArg(i);
        ConstraintSystem branchSystem(m_context, m_scope);
        if (!branchSystem.addConstraint(errs, operand, self))
          return false;

        for (Variable *variable : branchSystem.m_variables) {
          addVariable(variable);
        }
        for (Variable *variable : branchSystem.m_globalVariables) {
          result->addVariable(variable);
          addGlobalVariable(variable);
        }

        result->m_branches.push_back(std::move(branchSystem));
        result->m_branchInits.push_back(operand);
      }

      m_constraints.push_back(std::move(result));
      return true;
    }
  }

  auto result = std::make_unique<Apply>();
  result->m_init = init;
  result->m_self = self;

  auto *dag = dyn_cast<DagInit>(init);
  Init *predicateInit;
  if (dag) {
    predicateInit = dag->getOperator();
  } else {
    predicateInit = init;
  }

  result->m_predicate = m_context.getPredicate(predicateInit, errs);
  if (!result->m_predicate)
    return false;

  unsigned expectedArgSize = result->m_predicate->arguments().size();
  if (self) {
    expectedArgSize -= 1;
    result->m_arguments.push_back(self);
    result->addVariable(self);
  }

  if (dag) {
    if (dag->arg_size() != expectedArgSize) {
      errs << "  expected number of arguments: " << expectedArgSize << '\n';
      errs << "  actual number of arguments: " << dag->arg_size() << '\n';
      return false;
    }

    auto parsed = NamedValue::parseList(errs, m_context, dag, 0,
                                        NamedValue::Parser::ApplyArguments);
    if (!parsed.has_value())
      return false;

    for (unsigned i = 0; i < parsed->size(); ++i) {
      const auto &argument = (*parsed)[i];
      bool nonTrivialConstraint =
          argument.constraint && argument.constraint != m_context.getAny();
      Variable *variable = nullptr;
      bool isGlobal = false;

      if (!argument.name.empty()) {
        variable = m_scope.getVariable(argument.name);
        isGlobal = true;
      } else if (nonTrivialConstraint) {
        unsigned predArgIdx = i + (self ? 1 : 0);
        variable = m_scope.createVariable(
            result->m_predicate->arguments()[predArgIdx].name,
            result->m_predicate);
      }

      if (nonTrivialConstraint) {
        // We add constraints from deeper nesting levels first. This has the
        // effect that the evaluator can evaluate constants in a single pass
        // by scanning the constraint list from beginning to end.
        if (!addConstraint(errs, argument.constraint, variable))
          return false;
      }

      if (variable) {
        addVariable(variable);
        if (isGlobal)
          addGlobalVariable(variable);
        result->addVariable(variable);
      }
      result->m_arguments.push_back(variable);
    }
  } else {
    if (expectedArgSize != 0) {
      if (!self) {
        errs << "predicate in non-valued position needs explicit arguments\n";
        return false;
      }
      if (!result->getPredicate()->canCheckFromSelf()) {
        errs << "predicate cannot be checked from self and needs explicit "
                "arguments\n";
        return false;
      }
    }

    for (unsigned i = 0; i < expectedArgSize; ++i)
      result->m_arguments.push_back(nullptr);
  }

  m_constraints.push_back(std::move(result));
  return true;
}

void ConstraintSystem::merge(ConstraintSystem &&rhs) {
  assert(&m_context == &rhs.m_context);
  assert(&m_scope == &rhs.m_scope);

  for (auto &constraint : rhs.m_constraints)
    m_constraints.push_back(std::move(constraint));
  for (Variable *variable : rhs.m_variables)
    addVariable(variable);
}

void ConstraintSystem::addVariable(Variable *variable) {
  if (!llvm::is_contained(m_variables, variable))
    m_variables.push_back(variable);
}

void ConstraintSystem::addGlobalVariable(Variable *variable) {
  if (!llvm::is_contained(m_globalVariables, variable))
    m_globalVariables.push_back(variable);
}

std::string Constraint::toString() const {
  if (!m_self)
    return m_init->getAsString();
  return (Twine(m_init->getAsString()) + ":" + m_self->toString()).str();
}

void Constraint::printVariables(raw_ostream &out) const {
  bool isFirst = true;
  for (const Variable *var : m_variables) {
    if (!isFirst)
      out << ", ";
    out << var->toString();
    isFirst = false;
  }
}

void Constraint::addVariable(Variable *variable) {
  if (!llvm::is_contained(m_variables, variable))
    m_variables.push_back(variable);
}

void Apply::print(raw_ostream &out, StringRef prefix) const {
  out << prefix << "apply " << m_predicate->getInit()->getAsString() << ": ";
  bool isFirst = true;
  for (Variable *var : m_arguments) {
    if (!isFirst)
      out << ", ";
    if (var)
      out << var->toString();
    else
      out << "(null)";
    isFirst = false;
  }
  out << '\n';

  out << prefix << "  free variables: ";
  printVariables(out);
  out << '\n';
}

void LogicOr::print(raw_ostream &out, StringRef prefix) const {
  out << prefix << "or, free variables: ";
  printVariables(out);
  out << '\n';
  for (const auto &branch : m_branches)
    branch.print(out, (Twine(prefix) + "  ").str());
}

MetaType *MetaType::type() {
  static MetaType typeMetaType(Kind::Type);
  return &typeMetaType;
}

MetaType *MetaType::value() {
  static MetaType valueMetaType(Kind::Value);
  return &valueMetaType;
}

MetaType *MetaType::varargs() {
  static MetaType varargsMetaType(Kind::VarArgList);
  return &varargsMetaType;
}

StringRef MetaType::getGetterCppType() const {
  if (isValueArg())
    return "::llvm::Value *";

  if (isVarArgList())
    return "::llvm::iterator_range<::llvm::User::value_op_iterator>";

  return getCppType();
}

StringRef MetaType::getCppType() const {
  if (auto *attr = dyn_cast<Attr>(this))
    return attr->getCppType();

  assert(!isVarArgList() && "Not defined for varargs.");

  return "::llvm::Type *";
}

StringRef MetaType::getBuilderCppType() const {
  if (isValueArg())
    return "::llvm::Value *";

  if (isVarArgList())
    return "::llvm::ArrayRef<::llvm::Value *>";

  return getCppType();
}

bool MetaType::isImmutable() const {
  if (auto *attr = dyn_cast<Attr>(this))
    return attr->getIsImmutable();

  return false;
}

/// Return the C++ expression @p value transformed to be suitable for printing
/// using LLVM's raw_ostream.
std::string MetaType::printable(const MetaType *type, llvm::StringRef value) {
  if (!type || isa<Attr>(type))
    return value.str();
  return (Twine('*') + value).str();
}

StringRef MetaType::getName() const {
  if (auto *attr = dyn_cast<Attr>(this))
    return attr->getName();

  if (isTypeArg())
    return "type";
  return "value";
}

std::unique_ptr<Attr> Attr::parse(raw_ostream &errs,
                                  GenDialectsContext &context, Record *record) {
  if (!record->isSubClassOf("Attr")) {
    errs << record->getName() << ": must be a subclass of Attr\n";
    return {};
  }

  auto attr = std::make_unique<Attr>();
  attr->m_record = record;

  attr->m_cppType = record->getValueAsString("cppType");
  attr->m_toLlvmValue = record->getValueAsString("toLlvmValue");
  attr->m_fromLlvmValue = record->getValueAsString("fromLlvmValue");
  attr->m_toUnsigned = record->getValueAsString("toUnsigned");
  attr->m_fromUnsigned = record->getValueAsString("fromUnsigned");
  attr->m_check = record->getValueAsString("check");
  attr->m_isImmutable = record->getValueAsBit("isImmutable");

  return attr;
}

StringRef Attr::getName() const { return m_record->getName(); }
