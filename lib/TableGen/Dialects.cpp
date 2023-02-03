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

#include "llvm-dialects/TableGen/Dialects.h"

#include "llvm-dialects/TableGen/Constraints.h"
#include "llvm-dialects/TableGen/Operations.h"
#include "llvm-dialects/TableGen/Traits.h"

#include "llvm/ADT/Twine.h"
#include "llvm/TableGen/Record.h"

using namespace llvm;
using namespace llvm_dialects;

void GenDialect::finalize() {
  // Build the list of different attribute lists.
  auto traitLess = [](Trait *lhs, Trait *rhs) {
    return lhs->getName() < rhs->getName();
  };
  auto opLess = [traitLess](Operation *lhs, Operation *rhs) {
    return std::lexicographical_compare(lhs->traits.begin(), lhs->traits.end(),
                                        rhs->traits.begin(), rhs->traits.end(),
                                        traitLess);
  };

  std::vector<Operation *> traitOperations;

  for (const auto &op : operations) {
    if (op->traits.empty())
      continue;

    llvm::sort(op->traits, traitLess);

    traitOperations.push_back(op.get());
  }

  llvm::sort(traitOperations, opLess);

  Operation *current = nullptr;
  for (Operation *op : traitOperations) {
    if (!current || opLess(current, op)) {
      m_attributeLists.push_back(op->traits);
      current = op;
    }
    op->m_attributeListIdx = m_attributeLists.size() - 1;
  }
}

Trait *GenDialectsContext::getTrait(Record *traitRec) {
  if (!traitRec->isSubClassOf("Trait"))
    report_fatal_error(
        Twine("Trying to use '") + traitRec->getName() +
        "' as a constraint, but it is not a subclass of 'Constraint'");

  auto &result = m_traits[traitRec];
  if (!result)
    result = Trait::fromRecord(this, traitRec);
  return result.get();
}

Constraint *GenDialectsContext::getConstraint(Record *constraintRec) {
  if (!constraintRec->isSubClassOf("Constraint"))
    report_fatal_error(
        Twine("Trying to use '") + constraintRec->getName() +
        "' as a constraint, but it is not a subclass of 'Constraint'");

  auto &result = m_constraints[constraintRec];
  if (!result) {
    if (constraintRec->isSubClassOf("BuiltinType")) {
      result = std::make_unique<BuiltinType>();
    } else if (constraintRec->isSubClassOf("DialectType")) {
      result = std::make_unique<DialectType>();
    } else if (constraintRec->isSubClassOf("TypeArg")) {
      result = std::make_unique<TypeArg>();
    } else if (constraintRec->isSubClassOf("Attr")) {
      result = std::make_unique<Attr>();
    } else if (constraintRec->isSubClassOf("BaseCPred")) {
      result = std::make_unique<BaseCPred>(constraintRec->getName());
    } else {
      report_fatal_error(Twine("unsupported type constraint: ") +
                         constraintRec->getName());
    }

    result->init(this, constraintRec);
  }
  return result.get();
}

GenDialect *GenDialectsContext::getDialect(Record *dialectRec) {
  if (!dialectRec->isSubClassOf("Dialect"))
    report_fatal_error(Twine("Trying to use '") + dialectRec->getName() +
                       "' as a dialect, but it is not a subclass of 'Dialect'");

  auto it = m_dialects.find(dialectRec);
  if (it == m_dialects.end())
    report_fatal_error(Twine("Trying to use dialect '") +
                       dialectRec->getName() +
                       "', but has not been initialized");

  return it->second.get();
}

OpClass *GenDialectsContext::getOpClass(Record *opClassRec) {
  if (opClassRec->getName() == "NoSuperClass")
    return nullptr;

  if (!opClassRec->isSubClassOf("OpClass"))
    report_fatal_error(
        Twine("Trying to use '") + opClassRec->getName() +
        "' as operation class, but it is not a subclass of 'OpClass'");

  auto it = m_opClasses.find(opClassRec);
  if (it != m_opClasses.end()) {
    if (!it->second)
      report_fatal_error(Twine("Circular superclass relations involving '") +
                         opClassRec->getName() + "'");
    return it->second.get();
  }

  // Leave a marker in the map to detect recursive superclass relations.
  m_opClasses.try_emplace(opClassRec);

  auto opClassOwner = OpClass::parse(this, opClassRec);
  OpClass *opClass = opClassOwner.get();

  // Re-find the class record in case there were recursive lookups.
  m_opClasses.find(opClassRec)->second = std::move(opClassOwner);
  return opClass;
}

void GenDialectsContext::init(RecordKeeper &records,
                              const DenseSet<StringRef> &dialects) {
  m_voidTy = cast<BuiltinType>(getConstraint(records.getDef("VoidTy")));

  for (Record *dialectRec : records.getAllDerivedDefinitions("Dialect")) {
    auto name = dialectRec->getValueAsString("name");
    if (!dialects.contains(name))
      continue;

    auto dialect = std::make_unique<GenDialect>();
    dialect->record = dialectRec;
    dialect->cppName = dialectRec->getName();
    dialect->name = name;
    dialect->cppNamespace = dialectRec->getValueAsString("cppNamespace");
    m_dialects.insert(std::make_pair(dialectRec, std::move(dialect)));
  }

  for (Record *typeRec : records.getAllDerivedDefinitions("DialectType")) {
    auto *dialectType = cast<DialectType>(getConstraint(typeRec));
    auto dialectIt = m_dialects.find(dialectType->getDialectRec());
    if (dialectIt != m_dialects.end())
      dialectIt->second->types.push_back(dialectType);
  }

  for (Record *opRec : records.getAllDerivedDefinitions("Op")) {
    Record *dialectRec = opRec->getValueAsDef("dialect");
    auto dialectIt = m_dialects.find(dialectRec);
    if (dialectIt == m_dialects.end())
      continue;

    Operation::parse(this, dialectIt->second.get(), opRec);
  }

  for (auto &dialectEntry : m_dialects)
    dialectEntry.second->finalize();
}

std::unique_ptr<PredicateExpr>
GenDialectsContext::parsePredicateExpr(DagInit *dag) {
  Record *op = dag->getOperatorAsDef({});

  if (op->getName() == "and" || op->getName() == "or" ||
      op->getName() == "not") {
    PredicateExpr::Kind kind;
    if (op->getName() == "and")
      kind = PredicateExpr::Kind::And;
    else if (op->getName() == "or")
      kind = PredicateExpr::Kind::Or;
    else
      kind = PredicateExpr::Kind::Not;

    SmallVector<std::unique_ptr<PredicateExpr>> arguments;
    for (auto [arg, argName] : llvm::zip(dag->getArgs(), dag->getArgNames())) {
      if (argName) {
        report_fatal_error(Twine("Logical expression has named arguments: ") +
                           dag->getAsString());
      }

      if (!isa<DagInit>(arg)) {
        report_fatal_error(Twine("Logical expression is missing argument or "
                                 "has non-dag argument: ") +
                           dag->getAsString());
      }

      arguments.push_back(parsePredicateExpr(cast<DagInit>(arg)));
    }

    if (kind == PredicateExpr::Kind::Not && arguments.size() != 1) {
      report_fatal_error(Twine("Logical 'not' must have exactly 1 argument: ") +
                         dag->getAsString());
    }

    return std::make_unique<PredicateLogic>(kind, arguments);
  }

  // It's an application of a predicate.
  Constraint *predicate = getConstraint(op);
  SmallVector<std::string> arguments;

  for (auto [arg, argName] : llvm::zip(dag->getArgs(), dag->getArgNames())) {
    if (!isa<UnsetInit>(arg)) {
      report_fatal_error(
          Twine("Predicate application has an argument object: ") +
          dag->getAsString());
    }

    if (!argName) {
      report_fatal_error(
          Twine("Predicate application is missing argument name: ") +
          dag->getAsString());
    }

    arguments.push_back(argName->getValue().str());
  }

  auto [minArgs, maxArgs] = predicate->getMinMaxArgs();
  if (arguments.size() < minArgs || arguments.size() > maxArgs) {
    report_fatal_error(
        Twine("Predicate application has wrong number of arguments: ") +
        dag->getAsString());
  }

  return std::make_unique<PredicateApply>(predicate, arguments);
}
