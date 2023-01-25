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

Trait *GenDialectsContext::getTrait(Record *traitRec) {
  if (!traitRec->isSubClassOf("Trait"))
    report_fatal_error(
        Twine("Trying to use '") + traitRec->getName() +
        "' as a constraint, but it is not a subclass of 'Constraint'");

  auto &result = m_traits[traitRec];
  if (!result) {
    if (traitRec->isSubClassOf("LlvmAttributeTrait")) {
      result = std::make_unique<LlvmAttributeTrait>();
    } else {
      report_fatal_error(Twine("unsupported trait: ") + traitRec->getName());
    }
    result->init(this, traitRec);
  }
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

std::vector<OpNamedValue> GenDialectsContext::parseArguments(Record *rec) {
  Record *superClassRec = rec->getValueAsDef("superclass");
  OpClass *superclass = getOpClass(superClassRec);
  DagInit *argsInit = rec->getValueAsDag("arguments");
  std::vector<OpNamedValue> arguments;

  if (argsInit->getOperatorAsDef({})->getName() != "ins")
    report_fatal_error(Twine(rec->getName()) +
                       " argument operator must be 'ins'");

  for (unsigned i = 0; i < argsInit->getNumArgs(); ++i) {
    if (superclass && i == 0) {
      if (argsInit->getArgName(0) ||
          argsInit->getArg(0) != superClassRec->getDefInit())
        report_fatal_error(Twine(rec->getName()) +
                           ": superclass must be first in arguments list");
      continue;
    }

    OpNamedValue opArg;
    opArg.name = argsInit->getArgNameStr(i);
    if (auto *arg = dyn_cast_or_null<DefInit>(argsInit->getArg(i)))
      opArg.type = getConstraint(arg->getDef());
    if (!opArg.type) {
      report_fatal_error(Twine(rec->getName()) + " argument " + Twine(i) +
                         ": bad type constraint");
    }
    arguments.push_back(std::move(opArg));
  }

  return arguments;
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

  GenDialect *dialect = getDialect(opClassRec->getValueAsDef("dialect"));
  auto opClass = std::make_unique<OpClass>();
  opClass->name = opClassRec->getName();
  opClass->superclass = getOpClass(opClassRec->getValueAsDef("superclass"));
  opClass->arguments = parseArguments(opClassRec);

  OpClass *ptr = opClass.get();
  if (opClass->superclass)
    opClass->superclass->subclasses.push_back(ptr);
  dialect->opClasses.push_back(ptr);
  m_opClasses.find(opClassRec)->second = std::move(opClass);
  return ptr;
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

    // Extract information from the TableGen record.
    auto op = std::make_unique<Operation>();
    op->superclass = getOpClass(opRec->getValueAsDef("superclass"));
    if (op->superclass)
      op->superclass->operations.push_back(op.get());
    op->name = opRec->getName();
    op->mnemonic = opRec->getValueAsString("mnemonic");
    for (Record *traitRec : opRec->getValueAsListOfDefs("traits"))
      op->traits.push_back(getTrait(traitRec));

    op->arguments = parseArguments(opRec);

    DagInit *results = opRec->getValueAsDag("results");
    assert(results->getOperatorAsDef({})->getName() == "outs");
    assert(results->getNumArgs() <= 1 &&
           "multiple result values not supported");
    for (unsigned i = 0; i < results->getNumArgs(); ++i) {
      OpNamedValue opResult;
      opResult.name = results->getArgNameStr(i);
      if (auto *result = dyn_cast_or_null<DefInit>(results->getArg(i)))
        opResult.type = getConstraint(result->getDef());
      if (!opResult.type) {
        report_fatal_error(Twine("Operation '") + op->mnemonic + "' result " +
                           Twine(i) + ": bad type constraint");
      }
      if (!isa<Attr>(opResult.type) && !isa<Type>(opResult.type))
        op->builderHasExplicitResultTypes = true;
      op->results.push_back(std::move(opResult));
    }

    ListInit *verifier = opRec->getValueAsListInit("verifier");
    for (Init *ruleInit : *verifier) {
      auto *rule = dyn_cast<DagInit>(ruleInit);
      if (!rule) {
        report_fatal_error(Twine("Operation '") + op->mnemonic +
                           "': verifier rules must be dags");
      }

      op->verifier.push_back(parsePredicateExpr(rule));
    }

    // Derive the overload keys: Scan through results and arguments. Whenever
    // we encounter one whose type isn't fully specified, add it to the overload
    // keys unless an equal type has already been added.
    auto needsOverloadKey = [&](const OpNamedValue &namedValue) -> bool {
      if (isa<Type>(namedValue.type) || isa<Attr>(namedValue.type))
        return false;

      for (const auto &expr : op->verifier) {
        if (const auto *apply = dyn_cast<PredicateApply>(expr.get())) {
          if (apply->getPredicate()->getKind() == Constraint::Kind::SameTypes) {
            auto arguments = apply->arguments();
            if (llvm::is_contained(arguments, namedValue.name)) {
              for (const auto &overloadKey : op->overloadKeys) {
                StringRef name = overloadKey.kind == OverloadKey::Result
                                     ? op->results[overloadKey.index].name
                                     : op->arguments[overloadKey.index].name;
                if (llvm::is_contained(arguments, name))
                  return false;
              }
            }
          }
        }
      }

      return true;
    };

    for (const auto &result : llvm::enumerate(op->results)) {
      if (needsOverloadKey(result.value())) {
        OverloadKey key;
        key.kind = OverloadKey::Result;
        key.index = result.index();
        op->overloadKeys.push_back(key);
      }
    }
    for (const auto &arg : llvm::enumerate(op->getFullArguments())) {
      if (needsOverloadKey(arg.value())) {
        OverloadKey key;
        key.kind = OverloadKey::Argument;
        key.index = arg.index();
        op->overloadKeys.push_back(key);
      }
    }

    dialectIt->second->operations.push_back(std::move(op));
  }
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
