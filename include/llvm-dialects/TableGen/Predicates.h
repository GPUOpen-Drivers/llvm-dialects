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

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"

namespace llvm_dialects {

class Constraint;
class FmtContext;

class PredicateExpr {
public:
  enum class Kind {
    Logic_First,
    And = Logic_First,
    Or,
    Not,
    Logic_Last = Not,
    Apply,
  };

  virtual ~PredicateExpr() = default;

  Kind getKind() const { return m_kind; }

  /// Return a string containing a C++ expression that evaluates this
  /// predicate expression.
  ///
  /// @p names maps TableGen argument names (such as `$lhs`) to C++ names.
  virtual std::string
  evaluate(FmtContext *fmt,
           const llvm::DenseMap<llvm::StringRef, std::string> &names) const = 0;

protected:
  PredicateExpr(Kind kind) : m_kind(kind) {}

  const Kind m_kind;
};

class PredicateLogic : public PredicateExpr {
public:
  PredicateLogic(
      Kind kind,
      llvm::MutableArrayRef<std::unique_ptr<PredicateExpr>> arguments);

  static bool classof(const PredicateExpr *e) {
    return e->getKind() >= Kind::Logic_First &&
           e->getKind() <= Kind::Logic_Last;
  }

  std::string evaluate(
      FmtContext *fmt,
      const llvm::DenseMap<llvm::StringRef, std::string> &names) const final;

private:
  std::vector<std::unique_ptr<PredicateExpr>> m_arguments;
};

class PredicateApply : public PredicateExpr {
public:
  PredicateApply(Constraint *constraint, llvm::ArrayRef<std::string> arguments);

  static bool classof(const PredicateExpr *e) {
    return e->getKind() == Kind::Apply;
  }

  Constraint *getPredicate() const { return m_constraint; }
  llvm::ArrayRef<std::string> arguments() const { return m_arguments; }

  std::string evaluate(
      FmtContext *fmt,
      const llvm::DenseMap<llvm::StringRef, std::string> &names) const final;

  std::string getAsTableGenString() const;

private:
  Constraint *m_constraint;
  std::vector<std::string> m_arguments;
};

} // namespace llvm_dialects
