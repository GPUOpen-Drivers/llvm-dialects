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

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/raw_ostream.h"

namespace llvm {
class Init;
} // namespace llvm

namespace llvm_dialects {

class Apply;
class Constraint;
class ConstraintSystem;
class FmtContext;
class LogicOr;
class SymbolTable;
class Variable;

class Assignment {
public:
  using WatcherCallback = std::function<void(Variable *)>;
  class Watcher {
    friend class Assignment;

  public:
    Watcher(Assignment &assignment, WatcherCallback callback);
    ~Watcher();

  private:
    Assignment &m_assignment;
    WatcherCallback m_callback;
  };

private:
  friend class Evaluator;

  const Assignment *const m_parent = nullptr;
  llvm::DenseMap<Variable *, std::string> m_values;
  std::vector<Watcher *> m_watchers;

public:
  Assignment() = default;
  Assignment(const Assignment *parent) : m_parent(parent) {}

  llvm::StringRef lookup(Variable *variable) const;
  void assign(Variable *variable, llvm::StringRef value);
};

class EvaluationPlanner {
public:
  struct Plan {
    const Apply *apply = nullptr;
    unsigned argumentIndex;
    unsigned cost = std::numeric_limits<unsigned>::max();
  };

private:
  const ConstraintSystem &m_system;
  llvm::DenseMap<Variable *, Plan> m_plans;
  bool m_dirty = true;

public:
  EvaluationPlanner(const ConstraintSystem &system) : m_system(system) {}

  void setKnown(Variable *variable);
  const Plan *getPlan(Variable *variable);
  void explain(llvm::raw_ostream &errs, Variable *goal);

private:
  void update();
  void propagateApplyEvaluate(Apply *apply);
  void propagateApplyCapture(Apply *apply);
  void explainImpl(llvm::raw_ostream &errs, unsigned depth, Variable *goal,
                   llvm::DenseSet<Variable *> &visited);
};

class Evaluator {
  SymbolTable *m_symbols = nullptr;
  Assignment &m_assignment;
  const ConstraintSystem &m_system;
  EvaluationPlanner m_planner;
  Assignment::Watcher m_assignmentWatcher;
  llvm::raw_ostream &m_out;
  FmtContext &m_fmt;
  std::string m_errors;
  llvm::raw_string_ostream m_errs;
  bool m_comments = false;

  struct ErrorScope;

public:
  Evaluator(SymbolTable &symbols, Assignment &assignment,
            const ConstraintSystem &system, llvm::raw_ostream &out,
            FmtContext &fmt);
  ~Evaluator();

  /// Return the error messages that were accumulated in the last
  /// evaluate/capture call.
  std::string takeErrorMessages();

  /// Try to obtain a C++ expression producing the value of @p variable, using
  /// any and all information available in the constraint system and the cache.
  ///
  /// On failure, return an empty string and produce error messages. The caller
  /// _must_ use @ref takeErrorMessages in this case.
  std::string evaluate(Variable *variable);

  /// Emit C++ code that checks all known constraints. Errors are written to
  /// $_errs if writeErrs is true, and false is returned by the generated code
  /// if an error is detected.
  bool check(bool writeErrs);

private:
  void checkErrors();
  std::string evaluateImpl(Variable *variable);
  bool checkApplyEvaluate(bool writeErrs, const Apply *apply);
  bool checkApplyCapture(bool writeErrs, const Apply *apply);
  bool checkLogicOr(bool writeErrs, const LogicOr *logicOr);
  void checkAssignment(bool writeErrs, Variable *variable, std::string value,
                       llvm::Init *constraint);
};

} // namespace llvm_dialects
