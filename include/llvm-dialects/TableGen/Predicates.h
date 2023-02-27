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

#include "llvm-dialects/TableGen/Constraints.h"
#include "llvm-dialects/TableGen/NamedValue.h"

#include "llvm/ADT/ArrayRef.h"

namespace llvm_dialects {

class FmtContext;
class GenDialectsContext;

class Predicate {
public:
  enum class Kind {
    TgPredicate,
    CppPredicate_First,
    CppPredicate = CppPredicate_First,
    Constant,
    DialectType,
    CppPredicate_Last = DialectType,
  };

  virtual ~Predicate() = default;

  static std::unique_ptr<Predicate> parse(llvm::raw_ostream &errs,
                                          GenDialectsContext &context,
                                          llvm::Init *theInit);

  virtual bool init(llvm::raw_ostream &errs, GenDialectsContext &genContext,
                    llvm::Init *theInit);

  Kind getKind() const { return m_kind; }
  llvm::Init *getInit() const { return m_init; }
  llvm::ArrayRef<NamedValue> arguments() const { return m_arguments; }
  bool canDerive(unsigned argumentIndex) const {
    assert(argumentIndex < m_canDerive.size());
    return m_canDerive[argumentIndex];
  }
  bool canCheckFromSelf() const { return m_canCheckFromSelf; }

protected:
  Predicate(Kind kind) : m_kind(kind) {}

  const Kind m_kind;
  llvm::Init *m_init = nullptr;
  std::vector<NamedValue> m_arguments;

  /// Whether the constraint can be fully checked given only the "self"
  /// argument.
  bool m_canCheckFromSelf = false;

  /// Whether the given argument can be derived. For the self argument
  /// (argument 0), this means deriving given all other arguments. For non-self
  /// arguments, this means deriving given the self argument.
  std::vector<bool> m_canDerive;
};

class TgPredicate : public Predicate {
public:
  TgPredicate(GenDialectsContext &context)
      : Predicate(Kind::TgPredicate), m_system(context, m_scope) {}

  static bool classof(const Predicate *o) {
    return o->getKind() == Kind::TgPredicate;
  }

  bool init(llvm::raw_ostream &errs, GenDialectsContext &genContext,
            llvm::Init *theInit) override final;

  const ConstraintSystem &getSystem() const { return m_system; }
  llvm::ArrayRef<Variable *> variables() const { return m_variables; }

private:
  Scope m_scope;
  ConstraintSystem m_system;
  std::vector<Variable *> m_variables;
};

class BaseCppPredicate : public Predicate {
public:
  static bool classof(const Predicate *o) {
    return o->getKind() >= Kind::CppPredicate_First &&
           o->getKind() <= Kind::CppPredicate_Last;
  }

  llvm::StringRef getEvaluate() const { return m_evaluate; }
  llvm::StringRef getCheck() const { return m_check; }
  llvm::StringRef getCapture(unsigned i) const {
    if (m_capture.empty() || i == 0)
      return {};
    assert(i <= m_capture.size());
    return m_capture[i - 1];
  }

protected:
  BaseCppPredicate(Kind kind) : Predicate(kind) { assert(classof(this)); }

  std::string m_evaluate;
  std::string m_check;
  std::vector<std::string> m_capture;
};

class CppPredicate : public BaseCppPredicate {
public:
  CppPredicate() : BaseCppPredicate(Kind::CppPredicate) {}

  bool init(llvm::raw_ostream &errs, GenDialectsContext &genContext,
            llvm::Init *theInit) override final;
};

class Constant : public BaseCppPredicate {
public:
  Constant() : BaseCppPredicate(Kind::Constant) {}

  bool init(llvm::raw_ostream &errs, GenDialectsContext &genContext,
            llvm::Init *theInit) override final;
};

} // namespace llvm_dialects
