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

#include "llvm-dialects/TableGen/Evaluator.h"

#include "llvm-dialects/TableGen/Common.h"
#include "llvm-dialects/TableGen/Constraints.h"
#include "llvm-dialects/TableGen/Format.h"
#include "llvm-dialects/TableGen/Predicates.h"
#include "llvm-dialects/TableGen/SymbolTable.h"

#include "llvm/ADT/DenseSet.h"
#include "llvm/Support/Casting.h"
#include "llvm/TableGen/Record.h"

using namespace llvm;
using namespace llvm_dialects;

Assignment::Watcher::Watcher(Assignment &assignment, WatcherCallback callback)
    : m_assignment(assignment), m_callback(callback) {
  m_assignment.m_watchers.push_back(this);
}

Assignment::Watcher::~Watcher() {
  m_assignment.m_watchers.erase(llvm::find(m_assignment.m_watchers, this));
}

StringRef Assignment::lookup(Variable *variable) const {
  assert(variable);
  auto it = m_values.find(variable);
  if (it != m_values.end())
    return it->second;
  if (m_parent)
    return m_parent->lookup(variable);
  return {};
}

void Assignment::assign(Variable *variable, StringRef value) {
  assert(variable);
  assert(!value.empty());
  assert(lookup(variable).empty());
  m_values.try_emplace(variable, value.str());

  for (const auto &watcher : m_watchers)
    watcher->m_callback(variable);
}

void EvaluationPlanner::setKnown(Variable *variable) {
  Plan &plan = m_plans[variable];
  if (plan.cost != 0) {
    plan.apply = nullptr;
    plan.cost = 0;
    m_dirty = true;
  }
}

const EvaluationPlanner::Plan *EvaluationPlanner::getPlan(Variable *variable) {
  update();

  auto it = m_plans.find(variable);
  if (it != m_plans.end())
    return &it->second;

  return {};
}

void EvaluationPlanner::explain(raw_ostream &errs, Variable *goal) {
  DenseSet<Variable *> visited;
  explainImpl(errs, 1, goal, visited);
}

void EvaluationPlanner::explainImpl(raw_ostream &errs, unsigned depth,
                                    Variable *goal,
                                    DenseSet<Variable *> &visited) {
  if (!visited.insert(goal).second)
    return;

  std::string d;
  d.resize(2 * depth, ' ');

  for (const auto &constraintPtr : m_system.m_constraints) {
    Constraint *constraint = constraintPtr.get();

    if (!llvm::is_contained(constraint->variables(), goal))
      continue;

    if (auto *apply = dyn_cast<Apply>(constraint)) {
      unsigned argIdx = std::distance(apply->arguments().begin(),
                                      llvm::find(apply->arguments(), goal));
      if (!apply->getPredicate()->canDerive(argIdx)) {
        errs << d << goal->toString() << " appears as argument #" << argIdx
             << " of " << apply->toString() << '\n';
        errs << d << "but cannot be derived\n";
        continue;
      }

      errs << d << goal->toString() << " can be derived as argument #" << argIdx
           << " of " << apply->toString() << '\n';

      auto dependencies = apply->arguments();
      if (argIdx == 0)
        dependencies = dependencies.drop_front(1);
      else
        dependencies = dependencies.take_front(1);
      for (Variable *dependency : dependencies) {
        if (m_plans.find(dependency) != m_plans.end())
          continue;

        errs << d << "but requires unknown " << dependency->toString() << '\n';
        explainImpl(errs, depth + 1, dependency, visited);
        break;
      }
    } else {
      errs << d << goal->toString() << " appears in constraint "
           << constraint->toString()
           << " which cannot be used for derivations\n";
    }
  }
}

void EvaluationPlanner::update() {
  // Plan and cost update is an inefficient flood fill algorithm at the moment.
  // Good enough for now, for the size of constraint systems we expect.
  while (m_dirty) {
    m_dirty = false;

    for (const auto &constraintPtr : m_system.m_constraints) {
      if (auto *apply = dyn_cast<Apply>(constraintPtr.get())) {
        propagateApplyEvaluate(apply);
        propagateApplyCapture(apply);
      }
    }
  }
}

void EvaluationPlanner::propagateApplyEvaluate(Apply *apply) {
  if (!apply->getPredicate()->canDerive(0))
    return;

  unsigned cost = 0;
  for (unsigned argIdx = 1; argIdx < apply->arguments().size(); ++argIdx) {
    Variable *argument = apply->arguments()[argIdx];
    if (!argument)
      return;

    auto it = m_plans.find(argument);
    if (it == m_plans.end())
      return;
    cost += it->second.cost;
  }

  cost += 16;

  Plan &plan = m_plans[apply->arguments()[0]];
  if (cost < plan.cost) {
    plan.apply = apply;
    plan.argumentIndex = 0;
    plan.cost = cost;
    m_dirty = true;
  }
}

void EvaluationPlanner::propagateApplyCapture(Apply *apply) {
  auto it = m_plans.find(apply->arguments()[0]);
  if (it == m_plans.end())
    return;

  unsigned cost = it->second.cost + 1;
  for (unsigned argIdx = 1; argIdx < apply->arguments().size(); ++argIdx) {
    if (apply->getPredicate()->canDerive(argIdx)) {
      Plan &plan = m_plans[apply->arguments()[argIdx]];
      if (cost < plan.cost) {
        plan.apply = apply;
        plan.argumentIndex = argIdx;
        plan.cost = cost;
        m_dirty = true;
      }
    }
  }
}

Evaluator::Evaluator(SymbolTable &symbols, Assignment &assignment,
                     const ConstraintSystem &system, raw_ostream &out,
                     FmtContext &fmt)
    : m_symbols(&symbols), m_assignment(assignment), m_system(system),
      m_planner(m_system), m_assignmentWatcher(assignment,
                                               [this](Variable *variable) {
                                                 m_planner.setKnown(variable);
                                               }),
      m_out(out), m_fmt(fmt), m_errs(m_errors) {
  for (Variable *variable : system.variables()) {
    if (!assignment.lookup(variable).empty())
      m_planner.setKnown(variable);
  }

  m_comments = shouldEmitComments();
}

Evaluator::~Evaluator() { checkErrors(); }

std::string Evaluator::takeErrorMessages() {
  std::string errors = std::move(m_errors);
  m_errors.clear();
  return errors;
}

void Evaluator::checkErrors() {
  if (!m_errors.empty()) {
    report_fatal_error(
        Twine("Evaluator user failed to pick up error messages:\n") + m_errors);
  }
}

struct Evaluator::ErrorScope {
  Evaluator &evaluator;
  size_t oldSize;

  ErrorScope(Evaluator &evaluator);
  ~ErrorScope();

  void commit();
};

Evaluator::ErrorScope::ErrorScope(Evaluator &evaluator)
    : evaluator(evaluator), oldSize(evaluator.m_errors.size()) {}

Evaluator::ErrorScope::~ErrorScope() {
  if (oldSize != std::string::npos)
    evaluator.m_errors.resize(oldSize);
}

void Evaluator::ErrorScope::commit() { oldSize = std::string::npos; }

std::string Evaluator::evaluate(Variable *variable) {
  StringRef cachedValue = m_assignment.lookup(variable);
  if (!cachedValue.empty())
    return cachedValue.str();

  std::string value = evaluateImpl(variable);
  m_assignment.m_values.try_emplace(variable, value);
  return value;
}

std::string Evaluator::evaluateImpl(Variable *goal) {
  const EvaluationPlanner::Plan *plan = m_planner.getPlan(goal);
  if (!plan) {
    m_errs << "cannot derive " << goal->toString() << '\n';
    m_planner.explain(m_errs, goal);
    return {};
  }

  assert(plan->apply->arguments()[plan->argumentIndex] == goal);

  if (m_comments) {
    m_out << "// Evaluate argument " << plan->argumentIndex << " of "
          << plan->apply->toString() << '\n';
  }

  if (auto *tg = dyn_cast<TgPredicate>(plan->apply->getPredicate())) {
    Assignment nestedAssignment;
    ArrayRef<Variable *> args = plan->apply->arguments();
    if (plan->argumentIndex == 0)
      args = args.drop_front(1);
    else
      args = args.take_front(1);
    for (Variable *arg : args)
      nestedAssignment.assign(arg, evaluate(arg));

    SymbolScope symbolScope{&m_symbols};
    Evaluator nested(*m_symbols, nestedAssignment, tg->getSystem(), m_out,
                     m_fmt);
    std::string value = nested.evaluate(tg->variables()[plan->argumentIndex]);
    if (value.empty()) {
      m_errs << "TgPredicate expression evaluation failed\n";
      return {};
    }
    return value;
  }

  auto *cpp = cast<BaseCppPredicate>(plan->apply->getPredicate());
  FmtContextScope scope{m_fmt};
  if (plan->argumentIndex != 0) {
    m_fmt.addSubst(cpp->arguments()[0].name,
                   evaluate(plan->apply->arguments()[0]));
    return tgfmt(cpp->getCapture(plan->argumentIndex), &m_fmt).str();
  }

  for (unsigned i = 1; i < plan->apply->arguments().size(); ++i) {
    m_fmt.addSubst(cpp->arguments()[i].name,
                   evaluate(plan->apply->arguments()[i]));
  }
  return tgfmt(cpp->getEvaluate(), &m_fmt).str();
}

bool Evaluator::check(bool writeErrs) {
  if (m_comments) {
    m_out << "// Checking the constraint system:\n";
    m_system.print(m_out, "//   ");
  }

  SmallVector<bool> checkedConstraints;
  checkedConstraints.resize(m_system.m_constraints.size(), false);

  for (;;) {
    bool change = false;
    ErrorScope errorScope{*this};

    // Prefer to check constraints by evaluating them, as this leads to
    // higher-level comparisons in the verifier.
    //
    // For example, consider: (IntegerType $x, 32). If we check the constraint
    // by check & capture, we get potential error messages:
    //  - x is not an integer type
    //  - number of bits in x is not 32
    // If we instead check by evaluation, we get the potential error message
    //  - x is not i32
    // The latter is hopefully easier to understand in the usual cases.
    for (size_t i = 0; i < checkedConstraints.size(); ++i) {
      if (checkedConstraints[i])
        continue;

      auto *apply = dyn_cast<Apply>(m_system.m_constraints[i].get());
      if (!apply)
        continue;

      if (m_comments) {
        m_out << "// Try apply-evaluate of " << apply->toString() << '\n';
      }

      if (checkApplyEvaluate(writeErrs, apply)) {
        if (m_comments) {
          m_out << "// Done: " << apply->toString() << '\n';
        }
        checkedConstraints[i] = true;
        change = true;
      }
    }
    if (change)
      continue;

    for (size_t i = 0; i < checkedConstraints.size(); ++i) {
      if (checkedConstraints[i])
        continue;

      Constraint *constraint = m_system.m_constraints[i].get();

      if (m_comments) {
        m_out << "// Try check of " << constraint->toString() << '\n';
      }

      bool ok = false;
      if (auto *apply = dyn_cast<Apply>(constraint)) {
        ok = checkApplyCapture(writeErrs, apply);
      } else {
        auto *logicOr = dyn_cast<LogicOr>(constraint);
        ok = checkLogicOr(writeErrs, logicOr);
      }

      if (ok) {
        if (m_comments) {
          m_out << "// Done: " << constraint->toString() << '\n';
        }
        checkedConstraints[i] = true;
        change = true;
      }
    }

    if (llvm::all_of(checkedConstraints, [](bool checked) { return checked; }))
      break;

    if (!change) {
      errorScope.commit();
      return false;
    }
  }

  return true;
}

/// Check apply constraints by evaluating them where possible.
bool Evaluator::checkApplyEvaluate(bool writeErrs, const Apply *apply) {
  if (!apply->getPredicate()->canDerive(0))
    return false;

  for (unsigned argIdx = 1; argIdx < apply->arguments().size(); ++argIdx) {
    Variable *argument = apply->arguments()[argIdx];

    if (!argument) {
      m_errs << "cannot evaluate " << apply->toString() << '\n';
      m_errs << "because argument "
             << apply->getPredicate()->arguments()[argIdx].name
             << " is missing\n";
      return false;
    }

    if (m_assignment.lookup(argument).empty()) {
      m_errs << "cannot evaluate " << apply->toString() << ":\n";
      m_errs << "argument " << apply->arguments()[argIdx]->toString()
             << " is not known\n";
      return false;
    }
  }

  std::string value;
  if (auto *tg = dyn_cast<TgPredicate>(apply->getPredicate())) {
    SymbolScope symbolScope{&m_symbols};
    Assignment nestedAssignment;
    Evaluator nestedEvaluator(*m_symbols, nestedAssignment, tg->getSystem(),
                              m_out, m_fmt);
    for (unsigned argIdx = 1; argIdx < apply->arguments().size(); ++argIdx) {
      nestedAssignment.assign(tg->variables()[argIdx],
                              m_assignment.lookup(apply->arguments()[argIdx]));
    }
    nestedEvaluator.check(writeErrs);

    value = nestedAssignment.lookup(tg->variables()[0]).str();
  } else {
    auto *cpp = dyn_cast<BaseCppPredicate>(apply->getPredicate());
    FmtContextScope scope{m_fmt};
    for (unsigned argIdx = 1; argIdx < apply->arguments().size(); ++argIdx) {
      m_fmt.addSubst(cpp->arguments()[argIdx].name,
                     m_assignment.lookup(apply->arguments()[argIdx]));
    }

    value = tgfmt(cpp->getEvaluate(), &m_fmt).str();
  }

  Variable *self = apply->arguments()[0];
  checkAssignment(writeErrs, self, std::move(value), apply->getInit());
  return true;
}

/// Check apply constraints by checking and capturing them from the self
/// argument if possible.
bool Evaluator::checkApplyCapture(bool writeErrs, const Apply *apply) {
  Variable *self = apply->arguments()[0];
  StringRef selfValue = m_assignment.lookup(self);
  if (selfValue.empty()) {
    m_errs << "cannot check and capture " << apply->toString();
    m_errs << "\nbecause " << self->toString() << " is unknown\n";
    return false;
  }

  if (!apply->getPredicate()->canCheckFromSelf()) {
    for (unsigned argIdx = 1; argIdx < apply->arguments().size(); ++argIdx) {
      Variable *argument = apply->arguments()[argIdx];
      if (m_assignment.lookup(argument).empty()) {
        m_errs << "cannot check and capture " << apply->toString();
        m_errs << "\nbecause " << argument->toString() << " is unknown\n";
        return false;
      }
    }
  }

  std::string with;
  if (writeErrs) {
    raw_string_ostream withstream(with);
    withstream << tgfmt(R"(
      $_errs << "  with $0 = " << printable($1) << '\n';
    )",
                        &m_fmt, self->toString(), selfValue);
    if (!apply->getPredicate()->canCheckFromSelf()) {
      for (unsigned argIdx = 1; argIdx < apply->arguments().size(); ++argIdx) {
        Variable *argument = apply->arguments()[argIdx];
        withstream << tgfmt(R"(
          $_errs << "  with $0 = " << printable($1) << '\n';
        )",
                            &m_fmt, argument->toString(),
                            m_assignment.lookup(argument));
      }
    }
  }

  if (auto *tg = dyn_cast<TgPredicate>(apply->getPredicate())) {
    SymbolScope symbolScope{&m_symbols};
    Assignment nestedAssignment;
    Evaluator nestedEvaluator(*m_symbols, nestedAssignment, tg->getSystem(),
                              m_out, m_fmt);
    nestedAssignment.assign(tg->variables()[0], selfValue);

    if (!apply->getPredicate()->canCheckFromSelf()) {
      for (unsigned argIdx = 1; argIdx < apply->arguments().size(); ++argIdx) {
        Variable *argument = apply->arguments()[argIdx];
        nestedAssignment.assign(tg->variables()[argIdx],
                                m_assignment.lookup(argument));
      }
    }

    m_out << "if (![&]() {\n";

    nestedEvaluator.check(writeErrs);

    {
      FmtContextScope scope{m_fmt};
      m_fmt.addSubst("constraint", apply->toString());
      m_fmt.addSubst("with", with);

      m_out << tgfmt(R"(
          return true;
        }()) {)",
                     &m_fmt);

      if (writeErrs) {
        m_out << tgfmt(R"(
          $_errs << "  while checking $constraint\n";
          $with
        )",
                       &m_fmt);
      }

      m_out << R"(
          return false;
        }
      )";
    }

    for (unsigned argIdx = 1; argIdx < apply->arguments().size(); ++argIdx) {
      if (!tg->canDerive(argIdx))
        continue;

      StringRef value = nestedAssignment.lookup(tg->variables()[argIdx]);
      checkAssignment(writeErrs, apply->arguments()[argIdx], value.str(),
                      apply->getInit());
    }
  } else {
    auto *cpp = dyn_cast<BaseCppPredicate>(apply->getPredicate());
    FmtContextScope scope{m_fmt};
    m_fmt.addSubst(cpp->arguments()[0].name, selfValue);

    if (!apply->getPredicate()->canCheckFromSelf()) {
      for (unsigned argIdx = 1; argIdx < apply->arguments().size(); ++argIdx) {
        Variable *argument = apply->arguments()[argIdx];
        m_fmt.addSubst(cpp->arguments()[argIdx].name,
                       m_assignment.lookup(argument));
      }
    }

    if (cpp->getCheck() != "true") {
      StringRef check = cpp->getCheck();
      std::string checkValue = tgfmt(check, &m_fmt);

      FmtContextScope scope{m_fmt};
      m_fmt.addSubst("checkValue", checkValue);
      m_fmt.addSubst("constraint", apply->toString());
      m_fmt.addSubst("with", with);

      m_out << tgfmt(R"(
        if (!($checkValue)) {)",
                     &m_fmt);

      if (writeErrs) {
        m_out << tgfmt(R"(
          $_errs << "  failed check for $constraint\n";
          $with
        )",
                       &m_fmt);
      }

      m_out << R"(
          return false;
        }
      )";
    }

    for (unsigned argIdx = 1; argIdx < apply->arguments().size(); ++argIdx) {
      Variable *variable = apply->arguments()[argIdx];
      if (!variable || !cpp->canDerive(argIdx))
        continue;

      std::string value = tgfmt(cpp->getCapture(argIdx), &m_fmt).str();
      checkAssignment(writeErrs, variable, std::move(value), apply->getInit());
    }
  }

  return true;
}

bool Evaluator::checkLogicOr(bool writeErrs, const LogicOr *logicOr) {
  for (Variable *variable : logicOr->variables()) {
    if (m_assignment.lookup(variable).empty()) {
      m_errs << "cannot check " << logicOr->toString() << '\n';
      m_errs << "because " << variable->toString() << " is not known\n";
      return false;
    }
  }

  SymbolScope symbolScope(&m_symbols);
  {
    m_out << tgfmt(R"(
      {
        if (true
    )",
                   &m_fmt, logicOr->branches().size());
  }

  for (auto enumeratedBranch : llvm::enumerate(logicOr->branches())) {
    unsigned index = enumeratedBranch.index();
    const ConstraintSystem &childSystem = enumeratedBranch.value();

    m_out << tgfmt(R"(
      && !([&]() {
    )",
                   &m_fmt, index);

    Assignment scopedAssignment{&m_assignment};
    Evaluator childEvaluator(*m_symbols, scopedAssignment, childSystem, m_out,
                             m_fmt);
    if (!childEvaluator.check(false)) {
      m_errs << "failed to check branch #" << index << " ("
             << logicOr->branchInits()[index]->getAsString() << "):\n";
      m_errs << childEvaluator.takeErrorMessages();
      return false;
    }

    m_out << "return true;\n}())\n";
  }

  m_out << ") {\n";

  if (writeErrs) {
    if (Variable *self = logicOr->getSelf()) {
      FmtContextScope scope{m_fmt};
      m_fmt.addSubst("name", self->toString());
      m_fmt.addSubst("value", m_assignment.lookup(self));

      m_out << tgfmt(R"(
        $_errs << "  $name (" << printable($value)
               << ") does not match any available option\n";
      )",
                     &m_fmt);
    }

    for (unsigned i = 0; i < logicOr->branches().size(); ++i) {
      m_out << tgfmt(R"(
        $_errs << "  failed option $0 ($1):\n";
        ([&]() {
      )",
                     &m_fmt, i, logicOr->branchInits()[i]->getAsString());

      const ConstraintSystem &childSystem = logicOr->branches()[i];
      Assignment scopedAssignment{&m_assignment};
      Evaluator childEvaluator(*m_symbols, scopedAssignment, childSystem, m_out,
                               m_fmt);
      childEvaluator.check(true);

      m_out << R"(
          return true;
        })();
      )";
    }
  }

  m_out << "return false;\n}\n}\n";
  return true;
}

void Evaluator::checkAssignment(bool writeErrs, Variable *variable,
                                std::string value, Init *constraint) {
  assert(variable);
  assert(!value.empty());

  if (m_comments) {
    m_out << "// Check assignment " << variable->toString() << " <- " << value
          << '\n';
  }

  StringRef old = m_assignment.lookup(variable);
  if (old.empty()) {
    m_assignment.assign(variable, value);
  } else {
    m_fmt.addSubst("old", old);
    m_fmt.addSubst("new", value);
    m_fmt.addSubst("variable", variable->toString());

    if (variable->isNamed()) {
      m_out << tgfmt(R"(
        if ($new != $old) {)",
                     &m_fmt);

      if (writeErrs) {
        m_out << tgfmt(R"(
          $_errs << "  unexpected value of $variable:\n";
          $_errs << "    expected:  " << printable($new) << '\n';
          $_errs << "    actual:    " << printable($old) << '\n';
        )",
                       &m_fmt);
      }

      m_out << R"(
          return false;
        }
      )";
    } else {
      m_fmt.addSubst("constraint", constraint->getAsString());

      m_out << tgfmt(R"(
        if ($new != $old) {)",
                     &m_fmt);

      if (writeErrs) {
        m_out << tgfmt(R"(
          $_errs << "  inconsistent value of $variable found\n";
          $_errs << "  while checking $constraint:\n";
          $_errs << "    here:       " << printable($new) << '\n';
          $_errs << "    previously: " << printable($old) << '\n';
        )",
                       &m_fmt);
      }

      m_out << R"(
          return false;
        }
      )";
    }
  }
}
