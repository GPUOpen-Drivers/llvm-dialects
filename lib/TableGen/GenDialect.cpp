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

#include "llvm-dialects/TableGen/Common.h"
#include "llvm-dialects/TableGen/Format.h"
#include "llvm-dialects/TableGen/GenDialect.h"

#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/TableGen/Record.h"

#include <unordered_set>

using namespace llvm_dialects;
using namespace llvm;

namespace {

cl::opt<std::string> g_dialect("dialect", cl::desc("the dialect to generate"), cl::init(""));

class BuiltinType;
class Constraint;
class Dialect;
class PredicateExpr;
class Trait;

class DialectsContext {
public:
  void init(RecordKeeper& records, const DenseSet<StringRef>& dialects);

  Trait* getTrait(Record* traitRec);
  Constraint* getConstraint(Record* constraintRec);
  Dialect* getDialect(Record* dialectRec);

  BuiltinType* getVoidTy() const {return m_voidTy;}

  std::unique_ptr<PredicateExpr> parsePredicateExpr(DagInit *dag);

private:
  BuiltinType* m_voidTy = nullptr;
  DenseMap<Record*, std::unique_ptr<Trait>> m_traits;
  DenseMap<Record*, std::unique_ptr<Constraint>> m_constraints;
  DenseMap<Record*, std::unique_ptr<Dialect>> m_dialects;
};

class Constraint {
public:
  enum class Kind : uint8_t {
    Type_First,
    BuiltinType = Type_First,
    DialectType,
    Type_Last = DialectType,

    Attr,
    BaseCPred_First,
    BaseCPred = BaseCPred_First,
    SameTypes,
    BaseCPred_Last = SameTypes,
  };

  virtual void init(DialectsContext* context, Record* record) {
    m_record = record;
    m_builderArgumentFilter = record->getValueAsString("builderArgumentFilter");
  }

  Kind getKind() const {return m_kind;}
  Record* getRecord() const {return m_record;}
  StringRef getName() const {return m_record->getName();}
  StringRef getCppType() const;

  StringRef getBuilderArgumentFilter() const {return m_builderArgumentFilter;}

  virtual std::pair<unsigned, unsigned> getMinMaxArgs() const = 0;

  /// Return a string containing a C++ expression that applies this predicate
  /// to the given arguments.
  virtual std::string
  apply(FmtContext *fmt, ArrayRef<StringRef> arguments) const = 0;

protected:
  Constraint(Kind kind) : m_kind(kind) {}

private:
  const Kind m_kind;
  Record* m_record = nullptr;
  std::string m_builderArgumentFilter;
};

class Type : public Constraint {
public:
  static bool classof(const Constraint* c) {
    return c->getKind() >= Kind::Type_First && c->getKind() <= Kind::Type_Last;
  }

  void init(DialectsContext* context, Record* record) override {
    Constraint::init(context, record);
  }

  std::pair<unsigned, unsigned> getMinMaxArgs() const final {return {1, 1};}

  std::string
  apply(FmtContext *fmt, ArrayRef<StringRef> arguments) const final {
    assert(arguments.size() == 1);
    return tgfmt("$0 == $1", fmt, arguments[0], getLlvmType(fmt));
  }

  /// Return a string containing a C++ expression that returns the `llvm::Type*`
  /// for this type.
  virtual std::string getLlvmType(FmtContext *fmt) const = 0;

protected:
  Type(Kind kind) : Constraint(kind) {}
};

class BuiltinType : public Type {
public:
  BuiltinType() : Type(Kind::BuiltinType) {}

  void init(DialectsContext* context, Record* record) override {
    Type::init(context, record);
    m_getter = record->getValueAsString("getter");
  }

  StringRef getGetter() const {return m_getter;}

  static bool classof(const Constraint* c) {return c->getKind() == Kind::BuiltinType;}

  std::string getLlvmType(FmtContext *fmt) const final {
    return tgfmt(getGetter(), fmt);
  }

private:
  std::string m_getter;
};

class DialectType : public Type {
public:
  DialectType() : Type(Kind::DialectType) {}

  void init(DialectsContext* context, Record* record) override {
    Type::init(context, record);

    m_dialectRec = record->getValueAsDef("dialect");
    if (!m_dialectRec->isSubClassOf("Dialect")) {
      report_fatal_error(Twine("'dialect' field of type constraint '")
                              + record->getName() + "' is not a subclass of Dialect");
    }
    m_mnemonic = record->getValueAsString("mnemonic");
  }

  Record* getDialectRec() const {return m_dialectRec;}
  StringRef getMnemonic() const {return m_mnemonic;}

  static bool classof(const Constraint* c) {return c->getKind() == Kind::DialectType;}

  std::string getLlvmType(FmtContext *fmt) const final {
    return tgfmt("$0::get($_builder)", fmt, getName());
  }

private:
  Record* m_dialectRec = nullptr;
  std::string m_mnemonic;
};

class Attr : public Constraint {
public:
  Attr() : Constraint(Kind::Attr) {}

  void init(DialectsContext* context, Record* record) override {
    Constraint::init(context, record);

    m_cppType = record->getValueAsString("cppType");
    Record* llvmTypeRec = record->getValueAsDef("llvmType");
    Constraint* llvmType = context->getConstraint(llvmTypeRec);
    if (!isa<Type>(llvmType)) {
      report_fatal_error(Twine("Attr '") + record->getName()
                              + "' has llvmType '" + llvmType->getName()
                              + "' which is not a Type");
    }

    m_toLlvmValue = record->getValueAsString("toLlvmValue");
    m_fromLlvmValue = record->getValueAsString("fromLlvmValue");
  }

  std::pair<unsigned, unsigned> getMinMaxArgs() const final {return {1, 1};}

  std::string
  apply(FmtContext *fmt, ArrayRef<StringRef> arguments) const final {
    llvm_unreachable("cannot apply an Attr predicate");
  }

  StringRef getCppType() const {return m_cppType;}
  Type* getLlvmType() const {return m_llvmType;}
  StringRef getToLlvmValue() const {return m_toLlvmValue;}
  StringRef getFromLlvmValue() const {return m_fromLlvmValue;}

  static bool classof(const Constraint* c) {return c->getKind() == Kind::Attr;}

private:
  std::string m_cppType;
  Type* const m_llvmType = nullptr;
  std::string m_toLlvmValue;
  std::string m_fromLlvmValue;
};

class BaseCPred : public Constraint {
public:
  BaseCPred(StringRef name) : Constraint(getBaseCPredKind(name)) {}

  static bool classof(const Constraint* c) {
    return c->getKind() >= Kind::BaseCPred_First &&
           c->getKind() <= Kind::BaseCPred_Last;
  }

  void init(DialectsContext* context, Record* record) override {
    Constraint::init(context, record);

    m_predExpr = record->getValueAsString("predExpr");

    DagInit *arguments = record->getValueAsDag("arguments");
    if (arguments->getOperatorAsDef({})->getName() != "ins") {
      report_fatal_error(Twine("BaseCPred '") + record->getName()
                             + "': arguments dag operator must be 'ins'");
    }

    for (auto [argInit, argName]
             : llvm::zip(arguments->getArgs(), arguments->getArgNames())) {
      if (m_variadic) {
        report_fatal_error(Twine("BaseCPred '") + record->getName()
                               + "': seq must be last");
      }

      Argument arg;
      arg.name = argName->getValue();

      if (arg.name.empty()) {
        report_fatal_error(Twine("BaseCPred '") + record->getName()
                               + "': missing argument name");
      }

      if (isa<UnsetInit>(argInit)) {
        m_arguments.push_back(arg);
      } else {
        if (!isa<DefInit>(argInit) ||
            cast<DefInit>(argInit)->getAsString() != "seq") {
          report_fatal_error(Twine("BaseCPred '") + record->getName()
                                 + "': bad def used in arguments");
        }

        m_variadic = arg;
      }
    }
  }

  std::pair<unsigned, unsigned> getMinMaxArgs() const final {
    unsigned min = m_arguments.size();
    return {min, m_variadic ? std::numeric_limits<unsigned>::max() : min};
  }

  std::string
  apply(FmtContext *fmt, ArrayRef<StringRef> arguments) const final {
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

private:
  static Kind getBaseCPredKind(StringRef name) {
    if (name == "SameTypes")
      return Kind::SameTypes;
    return Kind::BaseCPred;
  }

  struct Argument {
    std::string name;
  };

  SmallVector<Argument> m_arguments;
  Optional<Argument> m_variadic;
  std::string m_predExpr;
};

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

  Kind getKind() const {return m_kind;}

  /// Return a string containing a C++ expression that evaluates this
  /// predicate expression.
  ///
  /// @p names maps TableGen argument names (such as `$lhs`) to C++ names.
  virtual std::string
  evaluate(FmtContext *fmt,
           const DenseMap<StringRef, std::string> &names) const = 0;

protected:
  PredicateExpr(Kind kind) : m_kind(kind) {}

  const Kind m_kind;
};

class PredicateLogic : public PredicateExpr {
public:
  PredicateLogic(Kind kind,
                 MutableArrayRef<std::unique_ptr<PredicateExpr>> arguments)
      : PredicateExpr(kind) {
    assert(classof(this));

    for (auto &arg : arguments)
      m_arguments.push_back(std::move(arg));
  }

  static bool classof(const PredicateExpr *e) {
    return e->getKind() >= Kind::Logic_First &&
           e->getKind() <= Kind::Logic_Last;
  }

  std::string
  evaluate(FmtContext *fmt,
           const DenseMap<StringRef, std::string> &names) const final {
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

private:
  std::vector<std::unique_ptr<PredicateExpr>> m_arguments;
};

class PredicateApply : public PredicateExpr {
public:
  PredicateApply(Constraint *constraint, ArrayRef<std::string> arguments)
      : PredicateExpr(Kind::Apply), m_constraint(constraint)
      , m_arguments(arguments) {
    assert(m_arguments.size() >= m_constraint->getMinMaxArgs().first);
    assert(m_arguments.size() <= m_constraint->getMinMaxArgs().second);
  }

  static bool classof(const PredicateExpr *e) {
    return e->getKind() == Kind::Apply;
  }

  Constraint *getPredicate() const {return m_constraint;}
  ArrayRef<std::string> arguments() const {return m_arguments;}

  std::string
  evaluate(FmtContext *fmt,
           const DenseMap<StringRef, std::string> &names) const final {
    SmallVector<StringRef> arguments;
    for (StringRef arg : m_arguments) {
      auto it = names.find(arg);
      if (it == names.end()) {
        report_fatal_error(Twine("Evaluating ") + getAsTableGenString()
                               + ": argument '" + arg + "' not found");
      }
      arguments.push_back(it->second);
    }
    return '(' + m_constraint->apply(fmt, arguments) + ')';
  }

  std::string getAsTableGenString() const {
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

private:
  Constraint *m_constraint;
  std::vector<std::string> m_arguments;
};

class Trait {
public:
  enum class Kind : uint8_t {
    LlvmAttributeTrait,
  };

  virtual void init(DialectsContext* context, Record* record) {
    m_record = record;
  }

  Kind getKind() const {return m_kind;}
  Record* getRecord() const {return m_record;}
  StringRef getName() const {return m_record->getName();}

protected:
  Trait(Kind kind) : m_kind(kind) {}

private:
  const Kind m_kind;
  Record* m_record = nullptr;
};

class LlvmAttributeTrait : public Trait {
public:
  LlvmAttributeTrait() : Trait(Kind::LlvmAttributeTrait) {}

  void init(DialectsContext* context, Record* record) override {
    Trait::init(context, record);
    m_llvmEnum = record->getValueAsString("llvmEnum");
  }

  StringRef getLlvmEnum() const {return m_llvmEnum;}

  static bool classof(const Trait* t) {return t->getKind() == Kind::LlvmAttributeTrait;}

private:
  std::string m_llvmEnum;
};

struct OpNamedValue {
  std::string name;
  Constraint* type = nullptr;
};

struct OverloadKey {
  enum Kind {
    Result,
    Argument,
  };

  Kind kind;
  unsigned index;
};

class Operation {
public:
  std::string name;
  std::string mnemonic;
  std::vector<Trait*> traits;
  std::vector<OpNamedValue> arguments;
  std::vector<OpNamedValue> results;
  std::vector<std::unique_ptr<PredicateExpr>> verifier;
  std::vector<OverloadKey> overloadKeys;
};

class Dialect {
public:
  Record* record;
  std::string cppName;
  std::string name;
  std::string cppNamespace;
  std::vector<DialectType*> types;
  std::vector<Operation> operations;
};

/// Helper class for choosing unique variable names.
class SymbolTable {
  std::unordered_set<std::string> m_names;

public:
  std::string chooseName(StringRef name) {
    return chooseName(makeArrayRef(name));
  }

  std::string chooseName(ArrayRef<StringRef> names) {
    for (StringRef name : names) {
      auto insert = m_names.insert(name.str());
      if (insert.second)
        return *insert.first;
    }

    for (int i = 0;; ++i) {
      std::string alternate = llvm::formatv("{0}_{1}", names.back(), i).str();
      auto insert = m_names.insert(alternate);
      if (insert.second)
        return *insert.first;
    }
  }
};

/// Helper class for emitting code that gets or builds llvm::Type*'s from constraints.
class LlvmTypeBuilder {
  SymbolTable& m_symbols;
  raw_ostream& m_out;
  DenseMap<Constraint*, std::string> m_constraintTypeVarName;
  FmtContext& m_fmt;

public:
  LlvmTypeBuilder(raw_ostream& out, SymbolTable& symbols, FmtContext& fmt)
      : m_symbols(symbols), m_out(out), m_fmt(fmt) {
  }

  std::string build(Constraint* constraint) {
    std::string& varName = m_constraintTypeVarName[constraint];
    if (varName.empty()) {
      if (auto* attr = dyn_cast<Attr>(constraint))
        constraint = attr->getLlvmType();

      auto* type = cast<Type>(constraint);
      varName = m_symbols.chooseName(type->getName());

      m_out << "llvm::Type* " << varName << " = " << type->getLlvmType(&m_fmt)
            << ";\n";
    }
    return varName;
  }
};

} // anonymous namespace

StringRef Constraint::getCppType() const {
  if (auto* attr = dyn_cast<Attr>(this))
    return attr->getCppType();
  return "::llvm::Value *";
}

Trait* DialectsContext::getTrait(Record* traitRec) {
  if (!traitRec->isSubClassOf("Trait"))
    report_fatal_error(Twine("Trying to use '") + traitRec->getName()
                           + "' as a constraint, but it is not a subclass of 'Constraint'");

  auto& result = m_traits[traitRec];
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

Constraint* DialectsContext::getConstraint(Record* constraintRec) {
  if (!constraintRec->isSubClassOf("Constraint"))
    report_fatal_error(Twine("Trying to use '") + constraintRec->getName()
                           + "' as a constraint, but it is not a subclass of 'Constraint'");

  auto& result = m_constraints[constraintRec];
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
      report_fatal_error(Twine("unsupported type constraint: ") + constraintRec->getName());
    }

    result->init(this, constraintRec);
  }
  return result.get();
}

Dialect* DialectsContext::getDialect(Record* dialectRec) {
  if (!dialectRec->isSubClassOf("Dialect"))
    report_fatal_error(Twine("Trying to use '") + dialectRec->getName()
                           + "' as a dialect, but it is not a subclass of 'Dialect'");

  auto it = m_dialects.find(dialectRec);
  if (it == m_dialects.end())
    report_fatal_error(Twine("Trying to use dialect '") + dialectRec->getName()
                           + "', but has not been initialized");

  return it->second.get();
}

void DialectsContext::init(RecordKeeper& records,
                           const DenseSet<StringRef>& dialects) {
  m_voidTy = cast<BuiltinType>(getConstraint(records.getDef("VoidTy")));

  for (Record* dialectRec : records.getAllDerivedDefinitions("Dialect")) {
    auto name = dialectRec->getValueAsString("name");
    if (!dialects.contains(name))
      continue;

    auto dialect = std::make_unique<Dialect>();
    dialect->record = dialectRec;
    dialect->cppName = dialectRec->getName();
    dialect->name = name;
    dialect->cppNamespace = dialectRec->getValueAsString("cppNamespace");
    m_dialects.insert(std::make_pair(dialectRec, std::move(dialect)));
  }

  for (Record* typeRec : records.getAllDerivedDefinitions("DialectType")) {
    auto* dialectType = cast<DialectType>(getConstraint(typeRec));
    auto dialectIt = m_dialects.find(dialectType->getDialectRec());
    if (dialectIt != m_dialects.end())
      dialectIt->second->types.push_back(dialectType);
  }

  for (Record* opRec : records.getAllDerivedDefinitions("Op")) {
    Record* dialectRec = opRec->getValueAsDef("dialect");
    auto dialectIt = m_dialects.find(dialectRec);
    if (dialectIt == m_dialects.end())
      continue;

    // Extract information from the TableGen record.
    Operation op;
    op.name = opRec->getName();
    op.mnemonic = opRec->getValueAsString("mnemonic");
    for (Record* traitRec : opRec->getValueAsListOfDefs("traits"))
      op.traits.push_back(getTrait(traitRec));

    DagInit* arguments = opRec->getValueAsDag("arguments");
    assert(arguments->getOperatorAsDef({})->getName() == "ins");
    for (unsigned i = 0; i < arguments->getNumArgs(); ++i) {
      OpNamedValue opArg;
      opArg.name = arguments->getArgNameStr(i);
      if (auto* arg = dyn_cast_or_null<DefInit>(arguments->getArg(i)))
        opArg.type = getConstraint(arg->getDef());
      if (!opArg.type) {
        report_fatal_error(Twine("Operation '") + op.mnemonic + "' argument " + Twine(i) +
                               ": bad type constraint");
      }
      op.arguments.push_back(std::move(opArg));
    }

    DagInit* results = opRec->getValueAsDag("results");
    assert(results->getOperatorAsDef({})->getName() == "outs");
    assert(results->getNumArgs() <= 1 && "multiple result values not supported");
    for (unsigned i = 0; i < results->getNumArgs(); ++i) {
      OpNamedValue opResult;
      opResult.name = results->getArgNameStr(i);
      if (auto* result = dyn_cast_or_null<DefInit>(results->getArg(i)))
        opResult.type = getConstraint(result->getDef());
      if (!opResult.type) {
        report_fatal_error(Twine("Operation '") + op.mnemonic + "' result " + Twine(i) +
                               ": bad type constraint");
      }
      op.results.push_back(std::move(opResult));
    }

    ListInit *verifier = opRec->getValueAsListInit("verifier");
    for (Init *ruleInit : *verifier) {
      auto *rule = dyn_cast<DagInit>(ruleInit);
      if (!rule) {
        report_fatal_error(Twine("Operation '") + op.mnemonic
                               + "': verifier rules must be dags");
      }

      op.verifier.push_back(parsePredicateExpr(rule));
    }

    // Derive the overload keys: Scan through results and arguments. Whenever
    // we encounter one whose type isn't fully specified, add it to the overload
    // keys unless an equal type has already been added.
    auto needsOverloadKey = [&](const OpNamedValue &namedValue) -> bool {
      if (isa<Type>(namedValue.type))
        return false;

      for (const auto &expr : op.verifier) {
        if (const auto *apply = dyn_cast<PredicateApply>(expr.get())) {
          if (apply->getPredicate()->getKind() == Constraint::Kind::SameTypes) {
            auto arguments = apply->arguments();
            if (llvm::is_contained(arguments, namedValue.name)) {
              for (const auto &overloadKey : op.overloadKeys) {
                StringRef name =
                    overloadKey.kind == OverloadKey::Result
                        ? op.results[overloadKey.index].name
                        : op.arguments[overloadKey.index].name;
                if (llvm::is_contained(arguments, name))
                  return true;
              }
            }
          }
        }
      }

      return false;
    };

    for (const auto result : llvm::enumerate(op.results)) {
      if (needsOverloadKey(result.value())) {
        OverloadKey key;
        key.kind = OverloadKey::Result;
        key.index = result.index();
        op.overloadKeys.push_back(key);
      }
    }
    for (const auto arg : llvm::enumerate(op.arguments)) {
      if (needsOverloadKey(arg.value())) {
        OverloadKey key;
        key.kind = OverloadKey::Argument;
        key.index = arg.index();
        op.overloadKeys.push_back(key);
      }
    }

    dialectIt->second->operations.push_back(std::move(op));
  }
}

std::unique_ptr<PredicateExpr> DialectsContext::parsePredicateExpr(DagInit *dag) {
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
        report_fatal_error(
            Twine("Logical expression has named arguments: ") 
                + dag->getAsString());
      }

      if (!isa<DagInit>(arg)) {
        report_fatal_error(
            Twine("Logical expression has missing or non-dag argument: ")
                + dag->getAsString());
      }

      arguments.push_back(parsePredicateExpr(cast<DagInit>(arg)));
    }

    if (kind == PredicateExpr::Kind::Not && arguments.size() != 1) {
      report_fatal_error(Twine("Logical 'not' must have exactly 1 argument: ")
                             + dag->getAsString());
    }

    return std::make_unique<PredicateLogic>(kind, arguments);
  }

  // It's an application of a predicate.
  Constraint *predicate = getConstraint(op);
  SmallVector<std::string> arguments;

  for (auto [arg, argName] : llvm::zip(dag->getArgs(), dag->getArgNames())) {
    if (!isa<UnsetInit>(arg)) {
      report_fatal_error(
          Twine("Predicate application has an argument object: ")
              + dag->getAsString());
    }

    if (!argName) {
      report_fatal_error(
          Twine("Predicate appliation has missing argument name: ")
              + dag->getAsString());
    }

    arguments.push_back(argName->getValue().str());
  }

  auto [minArgs, maxArgs] = predicate->getMinMaxArgs();
  if (arguments.size() < minArgs || arguments.size() > maxArgs) {
    report_fatal_error(
        Twine("Predicate application has wrong number of arguments: ")
            + dag->getAsString());
  }

  return std::make_unique<PredicateApply>(predicate, arguments);
}

static std::pair<DialectsContext, Dialect*>
getSelectedDialect(RecordKeeper& records) {
  if (g_dialect.empty())
    report_fatal_error(Twine("Must select a dialect using the --dialect option"));

  DialectsContext context;
  DenseSet<StringRef> dialects;
  dialects.insert(g_dialect);

  context.init(records, dialects);

  for (Record* dialectRec : records.getAllDerivedDefinitions("Dialect")) {
    if (dialectRec->getValueAsString("name") == g_dialect) {
      Dialect* selectedDialect = context.getDialect(dialectRec);
      return {std::move(context), selectedDialect};
    }
  }

  report_fatal_error(Twine("Could not find dialect. Check the '--dialect' option."));
}

void llvm_dialects::genDialectDecls(raw_ostream& out, RecordKeeper& records) {
  auto [context, dialect] = getSelectedDialect(records);

  emitHeader(out);

  out << R"(
#ifdef GET_INCLUDES
#undef GET_INCLUDES
#include "llvm/IR/Instructions.h"
#include "llvm-dialects/Dialect/Dialect.h"
#endif // GET_INCLUDES

#ifdef GET_DIALECT_DECLS
#undef GET_DIALECT_DECLS

namespace llvm_dialects {
class Builder;
} // namespace llvm_dialects

)";

  FmtContext fmt;
  fmt.addSubst("Dialect", dialect->cppName);
  fmt.addSubst("dialect", dialect->name);
  fmt.addSubst("namespace", dialect->cppNamespace);

  if (!dialect->cppNamespace.empty())
    out << tgfmt("namespace $namespace {\n\n", &fmt);

  // Dialect class declaration
  out << tgfmt(R"(
    class $Dialect : public ::llvm_dialects::DialectImpl<$Dialect> {
      friend DialectImpl;
      friend ::llvm_dialects::DialectContext;

      void anchor() override;

      static Key& getKey();

      $Dialect(::llvm::LLVMContext& context);

      static ::llvm_dialects::Dialect* make(::llvm::LLVMContext& context);
    };
  )", &fmt);

  // Type class declaration
  for (DialectType* type : dialect->types) {
    FmtContextScope scope{fmt};
    fmt.addSubst("Type", type->getName());
    fmt.addSubst("type", type->getMnemonic());

    out << tgfmt(R"(
      class $Type : public ::llvm::StructType {
        static constexpr ::llvm::StringLiteral s_name{"$dialect.$type"};

      public:
        static $Type* get(::llvm::LLVMContext& context);
        static $Type* get($Dialect& dialect);
        static $Type* get(::llvm_dialects::Builder& builder);

        static bool classof(::llvm::StructType *t) {
          return t->hasName() && t->getName() == s_name;
        }
        static bool classof(::llvm::Type* t) {
          return llvm::isa<::llvm::StructType>(t) &&
                 classof(llvm::cast<::llvm::StructType>(t));
        }
      };
    )", &fmt);
  }

  // Operation class declarations
  for (Operation& op : dialect->operations) {
    FmtContextScope scope{fmt};
    fmt.withOp(op.name);
    fmt.addSubst("mnemonic", op.mnemonic);

    out << tgfmt(R"(
      class $_op : public llvm::CallInst {
        static const ::llvm::StringLiteral s_name; //{"$dialect.$mnemonic"};

      public:
        static bool classof(const llvm::CallInst* i) {
          return llvm_dialects::detail::$0(i, s_name);
        }
        static bool classof(const llvm::Value* v) {
          return llvm::isa<llvm::CallInst>(v) &&
                 classof(llvm::cast<llvm::CallInst>(v));
        }
    )", &fmt, op.overloadKeys.empty() ? "isSimpleOperation"
                                      : "isOverloadedOperation");

    SymbolTable symbols;
    SmallVector<std::string> argNames;

    for (const auto& arg : op.arguments)
      argNames.push_back(symbols.chooseName(convertToCamelFromSnakeCase(arg.name, false)));

    fmt.withBuilder(symbols.chooseName({"b", "builder"}));

    out << tgfmt("static ::llvm::Value* create(llvm_dialects::Builder& $_builder", &fmt);
    for (const auto& [argName, arg] : llvm::zip_first(argNames, op.arguments))
      out << tgfmt(", $0 $1", &fmt, arg.type->getCppType(), argName);
    out << ");\n\n";

    for (const auto& arg : op.arguments) {
      out << tgfmt("$0 get$1();\n", &fmt, arg.type->getCppType(),
                   convertToCamelFromSnakeCase(arg.name, true));
    }

    out << '\n';

    for (const auto& result : op.results) {
      out << tgfmt("$0 get$1();\n", &fmt, result.type->getCppType(),
                   convertToCamelFromSnakeCase(result.name, true));
    }

    out << '\n';

    out << R"(
      };
    )";

  }

  if (!dialect->cppNamespace.empty())
    out << tgfmt("} // namespace $namespace\n", &fmt);

  out << R"(
#endif // GET_DIALECT_DECLS
)";
}

void llvm_dialects::genDialectDefs(raw_ostream& out, RecordKeeper& records) {
  auto [dialectsContext, dialect] = getSelectedDialect(records);

  emitHeader(out);

  out << R"(
#ifdef GET_INCLUDES
#undef GET_INCLUDES
#include "llvm-dialects/Dialect/Builder.h"
#include "llvm-dialects/Dialect/Utils.h"
#include "llvm/IR/InstrTypes.h"
#endif // GET_INCLUDES

#ifdef GET_DIALECT_DEFS
#undef GET_DIALECT_DEFS
)";

  FmtContext fmt;
  fmt.addSubst("Dialect", dialect->cppName);
  fmt.addSubst("dialect", dialect->name);
  fmt.addSubst("namespace", dialect->cppNamespace);

  if (!dialect->cppNamespace.empty())
    out << tgfmt("namespace $namespace {\n", &fmt);

  // Dialect class definitions.
  out << tgfmt(R"(
    void $Dialect::anchor() {}

    ::llvm_dialects::Dialect::Key& $Dialect::getKey() {
      static Key s_key;
      return s_key;
    }

    $Dialect::$Dialect(::llvm::LLVMContext& context) : DialectImpl(context) {}

    ::llvm_dialects::Dialect* $Dialect::make(::llvm::LLVMContext& context) {
      return new $Dialect(context);
    }
  )", &fmt);

  // Type class definitions.
  for (DialectType* type : dialect->types) {
    FmtContextScope scope{fmt};
    fmt.addSubst("Type", type->getName());
    fmt.addSubst("type", type->getMnemonic());

    out << tgfmt(R"(
      $Type* $Type::get(::llvm::LLVMContext& context) {
        ::llvm::Type* t = ::llvm::StructType::getTypeByName(context, s_name);
        if (t)
          return static_cast<$Type*>(t);

        return static_cast<$Type*>(::llvm::StructType::create(context, s_name));
      }
      $Type* $Type::get($Dialect& dialect) {return get(dialect.getContext());}
      $Type* $Type::get(::llvm_dialects::Builder& builder) {return get(builder.getContext());}

    )", &fmt);
  }

  // Operation class definitions.
  for (Operation& op : dialect->operations) {
    // Emit create() method definition.
    SmallVector<std::string> argNames;
    SymbolTable symbols;

    for (const auto& arg : op.arguments) {
      argNames.push_back(symbols.chooseName(convertToCamelFromSnakeCase(arg.name, false)));
    }

    std::string mod = symbols.chooseName("mod");
    std::string fn = symbols.chooseName("fn");
    std::string attrs = symbols.chooseName("attrs");
    std::string args = symbols.chooseName("args");
    std::string mangledName = symbols.chooseName("mangledName");

    FmtContextScope scope{fmt};
    fmt.withContext(symbols.chooseName("context"));
    fmt.withBuilder(symbols.chooseName({"b", "builder"}));
    fmt.withOp(op.name);
    fmt.addSubst("_module", mod);
    fmt.addSubst("mnemonic", op.mnemonic);

    out << tgfmt(R"(
      const ::llvm::StringLiteral $_op::s_name{"$dialect.$mnemonic"};
    
    )", &fmt);

    out << tgfmt("::llvm::Value* $_op::create(llvm_dialects::Builder& $_builder", &fmt);
    for (const auto& [argName, arg] : llvm::zip_first(argNames, op.arguments)) {
      out << tgfmt(", $0 $1", &fmt, arg.type->getCppType(), argName);
    }

    out << tgfmt(R"() {
      ::llvm::LLVMContext& $_context = $_builder.getContext();
      ::llvm::Module& $_module = *$_builder.GetInsertBlock()->getModule();
    
    )", &fmt);

    // Map TableGen names of arguments to C++ expressions to be used by
    // predicates.
    DenseMap<StringRef, std::string> argToCppExprMap;
    for (const auto &[arg, argName] : llvm::zip(op.arguments, argNames)) {
      std::string cppExpr;
      if (isa<Attr>(arg.type))
        cppExpr = argName;
      else
        cppExpr = argName + "->getType()";
      argToCppExprMap[arg.name] = cppExpr;

      if (!isa<Attr>(arg.type))
        out << tgfmt("assert($0);\n", &fmt, arg.type->apply(&fmt, {cppExpr}));
    }

    for (const auto &expr : op.verifier) {
      out << tgfmt("assert($0);\n", &fmt,
                   expr->evaluate(&fmt, argToCppExprMap));
    }
    out << '\n';

    if (op.traits.empty()) {
      out << "const ::llvm::AttributeList " << attrs << ";\n";
    } else {
      out << tgfmt("const auto $0 = ::llvm::AttributeList::get($_context, "
                   "::llvm::AttributeList::FunctionIndex, {\n", &fmt, attrs);

      for (const Trait* trait : op.traits) {
        if (auto* llvmAttribute = dyn_cast<LlvmAttributeTrait>(trait)) {
          out << "::llvm::Attribute::" << llvmAttribute->getLlvmEnum() << ",\n";
        } else {
          llvm_unreachable("unsupported trait kind");
        }
      }

      out << "});\n\n";
    }

    LlvmTypeBuilder typeBuilder{out, symbols, fmt};
    SmallVector<std::string> argTypes;
    for (const auto& [arg, argName] : llvm::zip(op.arguments, argNames)) {
      if (isa<Attr>(arg.type))
        argTypes.push_back(typeBuilder.build(arg.type));
      else
        argTypes.push_back(argName + "->getType()");
    }

    assert(op.results.size() <= 1);
    std::string resultType =
        typeBuilder.build(op.results.empty() ? dialectsContext.getVoidTy()
                                             : op.results[0].type);
    StringRef fnName;

    if (!op.overloadKeys.empty()) {
      out << tgfmt("std::string $0 = ::llvm_dialects::getMangledName(s_name, {\n", &fmt,
                   mangledName);
      for (const auto &key : op.overloadKeys) {
        assert(key.kind == OverloadKey::Argument);
        out << argNames[key.index] << "->getType(),\n";
      }
      out << "});\n";

      fnName = mangledName;
    } else {
      fnName = "s_name";
    }

    out << tgfmt("\nauto $0 = $_module.getOrInsertFunction($1, $2, $3",
                 &fmt, fn, fnName, attrs, resultType);
    for (const auto& argType : argTypes) {
      out << ", " << argType;
    }
    out << ");\n\n";

    for (const auto& [name, arg] : llvm::zip_first(argNames, op.arguments)) {
      if (auto* type = dyn_cast<Type>(arg.type)) {
        StringRef filter = type->getBuilderArgumentFilter();
        if (!filter.empty()) {
          FmtContextScope scope{fmt};
          fmt.withSelf(name);
          out << tgfmt(filter, &fmt);
        }
      }
    }

    out << tgfmt("::llvm::Value* const $0[] = {\n", &fmt, args);
    for (const auto& [name, type, arg]
             : llvm::zip_first(argNames, argTypes, op.arguments)) {
      if (auto* attr = dyn_cast<Attr>(arg.type)) {
        out << tgfmt(attr->getToLlvmValue(), &fmt, name, type);
      } else {
        out << name;
      }
      out << ",\n";
    }
    out << "};\n\n";

    out << tgfmt("return $_builder.CreateCall($0, $1);\n", &fmt, fn, args);
    out << "}\n\n";

    // Emit argument getters.
    for (auto indexedArg : llvm::enumerate(op.arguments)) {
      const OpNamedValue& arg = indexedArg.value();
      std::string value = llvm::formatv("getArgOperand({0})", indexedArg.index());
      if (auto* attr = dyn_cast<Attr>(arg.type))
        value = tgfmt(attr->getFromLlvmValue(), &fmt, value);
      out << tgfmt(R"(
        $0 $_op::get$1() {
          return $2;
        }
      )", &fmt, arg.type->getCppType(), convertToCamelFromSnakeCase(arg.name, true), value);
    }

    out << '\n';

    // Emit result getter
    for (const auto& result : op.results) {
      out << tgfmt("::llvm::Value* $_op::get$0() {return this;}\n", &fmt,
                   convertToCamelFromSnakeCase(result.name, true));
    }

    out << "\n\n";
  }

  if (!dialect->cppNamespace.empty())
    out << tgfmt("} // namespace $namespace\n", &fmt);

  out << R"(
#endif // GET_DIALECT_DEFS
)";
}
