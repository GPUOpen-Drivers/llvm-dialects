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

#include "llvm-dialects/TableGen/Operations.h"

#include "llvm-dialects/TableGen/Constraints.h"
#include "llvm-dialects/TableGen/Dialects.h"

#include "llvm/TableGen/Record.h"

using namespace llvm;
using namespace llvm_dialects;

static std::vector<OpNamedValue> parseArguments(GenDialectsContext *context,
                                                Record *rec) {
  Record *superClassRec = rec->getValueAsDef("superclass");
  OpClass *superclass = context->getOpClass(superClassRec);
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
      opArg.type = context->getConstraint(arg->getDef());
    if (!opArg.type) {
      report_fatal_error(Twine(rec->getName()) + " argument " + Twine(i) +
                         ": bad type constraint");
    }
    arguments.push_back(std::move(opArg));
  }

  return arguments;
}

std::unique_ptr<OpClass> OpClass::parse(GenDialectsContext *context,
                                        Record *record) {
  auto opClass = std::make_unique<OpClass>();
  opClass->dialect = context->getDialect(record->getValueAsDef("dialect"));
  opClass->name = record->getName();
  opClass->superclass = context->getOpClass(record->getValueAsDef("superclass"));
  opClass->arguments = parseArguments(context, record);

  OpClass *ptr = opClass.get();
  opClass->dialect->opClasses.push_back(opClass.get());
  if (opClass->superclass)
    opClass->superclass->subclasses.push_back(ptr);

  return opClass;
}

SmallVector<OpNamedValue> OpClass::getFullArguments() const {
  SmallVector<OpNamedValue> args;
  if (superclass)
    args = superclass->getFullArguments();
  args.insert(args.end(), arguments.begin(), arguments.end());
  return args;
}

unsigned OpClass::getNumFullArguments() const {
  if (superclass)
    return superclass->getNumFullArguments() + arguments.size();
  return arguments.size();
}

void Operation::parse(GenDialectsContext *context, GenDialect *dialect,
                      Record *record) {
  auto op = std::make_unique<Operation>();
  op->superclass = context->getOpClass(record->getValueAsDef("superclass"));
  if (op->superclass)
    op->superclass->operations.push_back(op.get());
  op->name = record->getName();
  op->mnemonic = record->getValueAsString("mnemonic");
  for (Record *traitRec : record->getValueAsListOfDefs("traits"))
    op->traits.push_back(context->getTrait(traitRec));

  op->arguments = parseArguments(context, record);

  DagInit *results = record->getValueAsDag("results");
  assert(results->getOperatorAsDef({})->getName() == "outs");
  assert(results->getNumArgs() <= 1 &&
          "multiple result values not supported");
  for (unsigned i = 0; i < results->getNumArgs(); ++i) {
    OpNamedValue opResult;
    opResult.name = results->getArgNameStr(i);
    if (auto *result = dyn_cast_or_null<DefInit>(results->getArg(i)))
      opResult.type = context->getConstraint(result->getDef());
    if (!opResult.type) {
      report_fatal_error(Twine("Operation '") + op->mnemonic + "' result " +
                          Twine(i) + ": bad type constraint");
    }
    if (!isa<Attr>(opResult.type) && !isa<Type>(opResult.type))
      op->builderHasExplicitResultTypes = true;
    op->results.push_back(std::move(opResult));
  }

  ListInit *verifier = record->getValueAsListInit("verifier");
  for (Init *ruleInit : *verifier) {
    auto *rule = dyn_cast<DagInit>(ruleInit);
    if (!rule) {
      report_fatal_error(Twine("Operation '") + op->mnemonic +
                          "': verifier rules must be dags");
    }

    op->verifier.push_back(context->parsePredicateExpr(rule));
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
            for (const auto &overloadKey : op->overload_keys()) {
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
      op->m_overloadKeys.push_back(key);
      op->m_haveResultOverloadKey = true;
    }
  }
  for (const auto &arg : llvm::enumerate(op->getFullArguments())) {
    if (needsOverloadKey(arg.value())) {
      OverloadKey key;
      key.kind = OverloadKey::Argument;
      key.index = arg.index();
      op->m_overloadKeys.push_back(key);
      op->m_haveArgumentOverloadKey = true;
    }
  }

  dialect->operations.push_back(std::move(op));
}

SmallVector<OpNamedValue> Operation::getFullArguments() const {
  SmallVector<OpNamedValue> args;
  if (superclass)
    args = superclass->getFullArguments();
  args.insert(args.end(), arguments.begin(), arguments.end());
  return args;
}

unsigned Operation::getNumFullArguments() const {
  if (superclass)
    return superclass->getNumFullArguments() + arguments.size();
  return arguments.size();
}
