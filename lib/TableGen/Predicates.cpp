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

#include "llvm-dialects/TableGen/Predicates.h"

#include "llvm-dialects/TableGen/DialectType.h"
#include "llvm-dialects/TableGen/Dialects.h"
#include "llvm-dialects/TableGen/Evaluator.h"
#include "llvm-dialects/TableGen/Format.h"
#include "llvm-dialects/TableGen/Predicates.h"

#include "llvm/TableGen/Record.h"

using namespace llvm;
using namespace llvm_dialects;

std::unique_ptr<Predicate> Predicate::parse(raw_ostream &errs,
                                            GenDialectsContext &context,
                                            Init *theInit) {
  std::unique_ptr<Predicate> result;
  if (isa<IntInit>(theInit)) {
    result = std::make_unique<Constant>();
  } else if (auto *defInit = dyn_cast<DefInit>(theInit)) {
    Record *record = defInit->getDef();
    if (!record->isSubClassOf("Predicate")) {
      errs << record->getName()
           << ": trying to use as predicate, but not a subclass of Predicate\n";
      return {};
    }

    bool tgPredicate = record->isSubClassOf("TgPredicateMixin");
    bool cppPredicate = record->isSubClassOf("CppPredicateMixin");
    bool dialectType = record->isSubClassOf("DialectType");
    if (int(tgPredicate) + int(cppPredicate) + int(dialectType) != 1) {
      errs << record->getName()
           << ": must have exactly one of: TgPredicateMixin, "
              "CppPredicateMixin, DialectType\n";
      return {};
    }

    if (tgPredicate) {
      result = std::make_unique<TgPredicate>(context);
    } else if (cppPredicate) {
      result = std::make_unique<CppPredicate>();
    } else {
      result = std::make_unique<DialectType>(context);
    }
  } else {
    errs << "not an operator: " << theInit->getAsString() << '\n';
    return {};
  }

  if (!result->init(errs, context, theInit))
    return {};
  return result;
}

bool Predicate::init(raw_ostream &errs, GenDialectsContext &genContext,
                     Init *theInit) {
  m_init = theInit;

  if (auto *defInit = dyn_cast<DefInit>(theInit)) {
    Record *record = defInit->getDef();

    auto *arguments = record->getValueAsDag("arguments");
    Record *argOp = arguments->getOperatorAsDef({});
    if (argOp->getName() != "args") {
      errs << "  argument list has unexpected operator: " << argOp->getName()
           << '\n';
      errs << "  expected form is (args $arg..)\n";
      return false;
    }

    auto parser = NamedValue::Parser::PredicateArguments;
    if (isa<DialectType>(this))
      parser = NamedValue::Parser::DialectTypeArguments;
    auto parsed = NamedValue::parseList(errs, genContext, arguments, 0, parser);
    if (!parsed.has_value()) {
      errs << "... in argument list of " << record->getName() << '\n';
      return false;
    }

    m_arguments = *parsed;

    if (m_arguments.empty()) {
      errs << " all operators need at least one (self) argument\n";
      return false;
    }
  }
  return true;
}

bool TgPredicate::init(raw_ostream &errs, GenDialectsContext &genContext,
                       Init *theInit) {
  if (!Predicate::init(errs, genContext, theInit))
    return false;

  Record *record = cast<DefInit>(theInit)->getDef();

  for (const auto &argument : arguments())
    m_variables.push_back(m_scope.getVariable(argument.name));

  DagInit *expression = record->getValueAsDag("expression");
  if (!m_system.addConstraint(errs, expression, nullptr))
    return false;

  for (const auto &variable : m_system.variables()) {
    if (!variable->isNamed())
      continue;

    if (!llvm::is_contained(m_variables, variable)) {
      errs << "  expression contains variable $" << variable->name()
           << " which is not an argument\n";
      return false;
    }
  }

  {
    EvaluationPlanner planner{m_system};
    for (unsigned argIdx = 1; argIdx < m_variables.size(); ++argIdx)
      planner.setKnown(m_variables[argIdx]);
    m_canDerive.push_back(planner.getPlan(m_variables[0]) != nullptr);
  }
  {
    EvaluationPlanner planner{m_system};
    planner.setKnown(m_variables[0]);

    m_canCheckFromSelf = true;
    for (unsigned argIdx = 1; argIdx < m_variables.size(); ++argIdx) {
      bool canCapture = planner.getPlan(m_variables[argIdx]) != nullptr;
      m_canDerive.push_back(canCapture);
      if (!canCapture)
        m_canCheckFromSelf = false;
    }
  }

  return true;
}

bool CppPredicate::init(raw_ostream &errs, GenDialectsContext &genContext,
                        Init *theInit) {
  if (!BaseCppPredicate::init(errs, genContext, theInit))
    return false;

  Record *record = cast<DefInit>(theInit)->getDef();

  m_evaluate = record->getValueAsString("evaluate");
  m_canDerive.push_back(!m_evaluate.empty());
  m_check = record->getValueAsString("check");

  auto capture = record->getValueAsListOfStrings("capture");
  if (!capture.empty()) {
    if (capture.size() != m_arguments.size() - 1) {
      errs << "  capture list must be empty or of size one less than argument "
              "list\n";
      errs << "  argument list size: " << m_arguments.size() << '\n';
      errs << "  capture list size: " << capture.size() << '\n';
      return false;
    }

    m_canCheckFromSelf = true;
    for (unsigned i = 0; i < capture.size(); ++i) {
      m_capture.push_back(capture[i].str());
      m_canDerive.push_back(!capture[i].empty());
      if (capture[i].empty())
        m_canCheckFromSelf = false;
    }
  } else {
    for (unsigned i = 0; i < m_arguments.size(); ++i)
      m_canDerive.push_back(false);
  }

  return true;
}

bool Constant::init(raw_ostream &errs, GenDialectsContext &context,
                    Init *theInit) {
  if (!BaseCppPredicate::init(errs, context, theInit))
    return false;

  NamedValue self;
  self.name = "_self";
  m_arguments.push_back(std::move(self));

  auto *intInit = cast<IntInit>(theInit);
  int64_t value = intInit->getValue();

  FmtContext fmt;
  fmt.addSubst("value", Twine(value));
  m_evaluate = tgfmt("$value", &fmt);
  m_check = tgfmt("$$_self == $value", &fmt);
  m_canDerive.push_back(true);
  m_canCheckFromSelf = true;
  return true;
}
