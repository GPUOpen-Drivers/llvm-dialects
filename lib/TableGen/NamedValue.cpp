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

#include "llvm-dialects/TableGen/NamedValue.h"

#include "llvm-dialects/TableGen/Constraints.h"
#include "llvm-dialects/TableGen/Dialects.h"

#include "llvm/TableGen/Record.h"

using namespace llvm;
using namespace llvm_dialects;

std::optional<std::vector<NamedValue>>
NamedValue::parseList(raw_ostream &errs, GenDialectsContext &context,
                      DagInit *init, unsigned begin, Parser mode) {
  std::vector<NamedValue> values;
  bool isOperation =
      mode == Parser::OperationArguments || mode == Parser::OperationResults;

  bool didSeeVarArgs = false;

  for (unsigned i = begin; i < init->getNumArgs(); ++i) {
    if (didSeeVarArgs) {
      errs << "No other argument can follow a definition of 'varargs'\n";
      return {};
    }

    NamedValue value;
    value.name = init->getArgNameStr(i);
    if (value.name.empty() && mode != Parser::ApplyArguments) {
      errs << "Name missing: " << init->getAsString() << '\n';
      return {};
    }

    Init *valueInit = init->getArg(i);
    if (mode != Parser::ApplyArguments && isa<UnsetInit>(valueInit)) {
      errs << "Type/constraint missing for $" << value.name
           << " in: " << init->getAsString() << '\n';
      return {};
    }

    bool recognized = false;
    bool accepted = false;

    if (auto *defInit = dyn_cast<DefInit>(valueInit)) {
      Record *def = defInit->getDef();

      if (def->getName() == "type") {
        if (mode == Parser::OperationResults ||
            mode == Parser::ApplyArguments) {
          if (mode == Parser::OperationResults)
            errs << "Operation result";
          else
            errs << "Predicate argument";
          errs << " cannot be 'type'\n";
          errs << "... in: " << init->getAsString() << '\n';
          return {};
        }

        value.type = MetaType::type();
        recognized = true;
        accepted = true;
      } else if (def->getName() == "value") {
        recognized = true;
        if (isOperation) {
          value.type = MetaType::value();
          accepted = true;
        }
      } else if (def->getName() == "any") {
        recognized = true;
        if (mode == Parser::PredicateArguments)
          accepted = true;
      } else if (def->isSubClassOf("Attr")) {
        recognized = true;

        if (mode != Parser::OperationResults &&
            mode != Parser::ApplyArguments) {
          value.type = context.getAttr(def, errs);
          if (!value.type) {
            errs << "... in: " << init->getAsString() << '\n';
            return {};
          }
          accepted = true;
        }
      } else if (def->getName() == "varargs") {
        recognized = true;

        if (mode == Parser::OperationArguments) {
          value.type = MetaType::varargs();
          didSeeVarArgs = true;
          accepted = true;
        }
      }
    }

    if (isOperation || mode == Parser::ApplyArguments) {
      if (!recognized && !isa<UnsetInit>(valueInit)) {
        if (isOperation)
          value.type = MetaType::value();
        value.constraint = valueInit;
      }

      if (!value.constraint)
        value.constraint = context.getAny();
      accepted = true;
    }

    if (!accepted) {
      errs << "type of $" << value.name
           << " not accepted: " << valueInit->getAsString() << '\n';
      errs << "... in: " << init->getAsString() << '\n';
      return {};
    }

    values.push_back(std::move(value));
  }

  return values;
}

std::string NamedValue::toString() const {
  std::string result;
  if (constraint)
    result = constraint->getAsString();
  if (!name.empty()) {
    if (constraint)
      result += ":$";
    else
      result = '$';
    result += name;
  }
  return result;
}
