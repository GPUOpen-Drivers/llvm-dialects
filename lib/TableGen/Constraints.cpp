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

#include "llvm-dialects/TableGen/Constraints.h"

#include "llvm-dialects/TableGen/Dialects.h"
#include "llvm-dialects/TableGen/Format.h"

#include "llvm/TableGen/Record.h"

using namespace llvm;
using namespace llvm_dialects;

void Constraint::init(GenDialectsContext *context, Record *record) {
  m_record = record;
}

StringRef Constraint::getName() const { return m_record->getName(); }

StringRef Constraint::getCppType() const {
  if (auto* attr = dyn_cast<Attr>(this))
    return attr->getCppType();
  return "::llvm::Value *";
}

std::string Type::apply(FmtContext *fmt, ArrayRef<StringRef> arguments) const {
  assert(arguments.size() == 1);
  return tgfmt("$0 == $1", fmt, arguments[0], getLlvmType(fmt));
}

void BuiltinType::init(GenDialectsContext *context, llvm::Record *record) {
  Type::init(context, record);
  m_getter = record->getValueAsString("getter");
}

std::string BuiltinType::getLlvmType(FmtContext *fmt) const {
  return tgfmt(getGetter(), fmt);
}

void DialectType::init(GenDialectsContext *context, llvm::Record *record) {
  Type::init(context, record);

  m_dialectRec = record->getValueAsDef("dialect");
  if (!m_dialectRec->isSubClassOf("Dialect")) {
    report_fatal_error(Twine("'dialect' field of type constraint '") +
                       record->getName() + "' is not a subclass of Dialect");
  }
  m_mnemonic = record->getValueAsString("mnemonic");
}

std::string DialectType::getLlvmType(FmtContext *fmt) const {
  return tgfmt("$0::get($_builder)", fmt, getName());
}

void Attr::init(GenDialectsContext *context, llvm::Record *record) {
  Constraint::init(context, record);

  m_cppType = record->getValueAsString("cppType");
  llvm::Record *llvmTypeRec = record->getValueAsDef("llvmType");
  Constraint *llvmType = context->getConstraint(llvmTypeRec);
  if (!isa<Type>(llvmType)) {
    report_fatal_error(Twine("Attr '") + record->getName() +
                       "' has llvmType '" + llvmType->getName() +
                       "' which is not a Type");
  }

  m_llvmType = cast<Type>(llvmType);
  m_toLlvmValue = record->getValueAsString("toLlvmValue");
  m_fromLlvmValue = record->getValueAsString("fromLlvmValue");
}

void BaseCPred::init(GenDialectsContext *context, llvm::Record *record) {
  Constraint::init(context, record);

  m_predExpr = record->getValueAsString("predExpr");

  DagInit *arguments = record->getValueAsDag("arguments");
  if (arguments->getOperatorAsDef({})->getName() != "ins") {
    report_fatal_error(Twine("BaseCPred '") + record->getName() +
                       "': arguments dag operator must be 'ins'");
  }

  for (auto [argInit, argName] :
       llvm::zip(arguments->getArgs(), arguments->getArgNames())) {
    if (m_variadic) {
      report_fatal_error(Twine("BaseCPred '") + record->getName() +
                         "': seq must be last");
    }

    Argument arg;
    arg.name = argName->getValue();

    if (arg.name.empty()) {
      report_fatal_error(Twine("BaseCPred '") + record->getName() +
                         "': missing argument name");
    }

    if (isa<UnsetInit>(argInit)) {
      m_arguments.push_back(arg);
    } else {
      if (!isa<DefInit>(argInit) ||
          cast<DefInit>(argInit)->getAsString() != "seq") {
        report_fatal_error(Twine("BaseCPred '") + record->getName() +
                           "': bad def used in arguments");
      }

      m_variadic = arg;
    }
  }
}

std::string BaseCPred::apply(FmtContext *fmt,
                             ArrayRef<StringRef> arguments) const {
  assert(arguments.size() >= m_arguments.size());
  assert(m_variadic || arguments.size() == m_arguments.size());

  FmtContextScope scope(*fmt);
  for (const auto &[formal, actual] : llvm::zip(m_arguments, arguments)) {
    if (formal.name == "_self")
      fmt->withSelf(actual);
    else
      fmt->addSubst(formal.name, actual);
  }

  if (m_variadic) {
    std::string actuals;
    actuals += '{';

    for (const auto &actual : arguments.drop_front(m_arguments.size())) {
      if (actuals.size() > 1)
        actuals += ", ";
      actuals += actual;
    }

    actuals += '}';
    fmt->addSubst(m_variadic->name, actuals);
  }

  return tgfmt(m_predExpr, fmt);
}
