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

#include "llvm-dialects/TableGen/Predicates.h"

#include "llvm-dialects/TableGen/Constraints.h"

#include "llvm/ADT/Twine.h"
#include "llvm/TableGen/Record.h"

using namespace llvm;
using namespace llvm_dialects;

PredicateLogic::PredicateLogic(
    Kind kind, MutableArrayRef<std::unique_ptr<PredicateExpr>> arguments)
    : PredicateExpr(kind) {
  assert(classof(this));

  for (auto &arg : arguments)
    m_arguments.push_back(std::move(arg));
}

std::string
PredicateLogic::evaluate(FmtContext *fmt,
                         const DenseMap<StringRef, std::string> &names) const {
  if (getKind() == Kind::Not)
    return "!" + m_arguments[0]->evaluate(fmt, names);

  std::string out;
  out += '(';

  for (const auto &argument : m_arguments) {
    if (out.size() > 1) {
      if (getKind() == Kind::And)
        out += " && ";
      else
        out += " || ";
    }

    out += argument->evaluate(fmt, names);
  }

  out += ')';
  return out;
}

PredicateApply::PredicateApply(Constraint *constraint,
                               ArrayRef<std::string> arguments)
    : PredicateExpr(Kind::Apply), m_constraint(constraint),
      m_arguments(arguments) {
  assert(m_arguments.size() >= m_constraint->getMinMaxArgs().first);
  assert(m_arguments.size() <= m_constraint->getMinMaxArgs().second);
}

std::string
PredicateApply::evaluate(FmtContext *fmt,
                         const DenseMap<StringRef, std::string> &names) const {
  SmallVector<StringRef> arguments;
  for (StringRef arg : m_arguments) {
    auto it = names.find(arg);
    if (it == names.end()) {
      report_fatal_error(Twine("Evaluating ") + getAsTableGenString() +
                         ": argument '" + arg + "' not found");
    }
    arguments.push_back(it->second);
  }
  return '(' + m_constraint->apply(fmt, arguments) + ')';
}

std::string PredicateApply::getAsTableGenString() const {
  std::string result;
  result += '(';
  result += m_constraint->getRecord()->getName();

  for (const auto &arg : m_arguments) {
    result += ' ';
    result += arg;
  }

  result += ')';
  return result;
}
