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
#include "llvm-dialects/TableGen/Format.h"

#include "llvm/TableGen/Record.h"

using namespace llvm;
using namespace llvm_dialects;

static std::string evaluateAttrLlvmType(raw_ostream &errs, raw_ostream &out,
                                        FmtContext &fmt, Attr *attr,
                                        StringRef name,
                                        GenDialectsContext &context,
                                        SymbolTable &symbols);

static std::optional<std::vector<NamedValue>>
parseArguments(raw_ostream &errs, GenDialectsContext &context, Record *rec) {
  Record *superClassRec = rec->getValueAsDef("superclass");
  OpClass *superclass = context.getOpClass(superClassRec);
  DagInit *argsInit = rec->getValueAsDag("arguments");

  if (argsInit->getOperatorAsDef({})->getName() != "ins") {
    errs << "argument list operator must be 'ins'\n";
    errs << "... in: " << argsInit->getAsString() << '\n';
    return {};
  }

  if (superclass) {
    if (argsInit->arg_size() < 1 || argsInit->getArgName(0) ||
        argsInit->getArg(0) != superClassRec->getDefInit()) {
      errs << "'superclass' must be first in arguments list\n";
      errs << "... in: " << argsInit->getAsString() << '\n';
      return {};
    }
  }

  return NamedValue::parseList(errs, context, argsInit, superclass ? 1 : 0,
                               NamedValue::Parser::OperationArguments);
}

namespace {
class AccessorBuilder final {
public:
  AccessorBuilder(FmtContext &fmt, llvm::raw_ostream &out,
                  const NamedValue &arg, std::string_view argTypeString)
      : m_fmt{fmt}, m_os{out}, m_arg{arg}, m_argTypeString{argTypeString} {}

  void emitAccessorDefinitions() const;

private:
  FmtContext &m_fmt;
  llvm::raw_ostream &m_os;
  const NamedValue &m_arg;
  std::string_view m_argTypeString;

  void emitGetterDefinition() const;
  void emitSetterDefinition() const;
};
} // namespace

bool OperationBase::init(raw_ostream &errs, GenDialectsContext &context,
                         Record *record) {
  m_dialect = context.getDialect(record->getValueAsDef("dialect"));
  m_superclass = context.getOpClass(record->getValueAsDef("superclass"));

  auto arguments = parseArguments(errs, context, record);
  if (!arguments.has_value())
    return false;

  m_arguments = std::move(*arguments);

  // Don't allow any other arguments if the superclass already uses
  // variadic arguments, as the arguments will be appended to the arguments of
  // the superclass.
  if (m_superclass && m_superclass->hasVariadicArgument()) {
    if (!m_arguments.empty()) {
      errs << "Superclass " << m_superclass->name
           << " already has variadic arguments, cannot have any other "
              "arguments.\n";
      return false;
    }

    m_hasVariadicArgument = true;
  }

  for (const auto &arg : m_arguments) {
    std::string attrType;

    if (!m_hasVariadicArgument && arg.type->isVarArgList()) {
      m_hasVariadicArgument = true;
    }

    if (auto *attr = dyn_cast<Attr>(arg.type)) {
      SymbolTable symbols;
      std::string prelude;
      raw_string_ostream preludes(prelude);
      FmtContext fmt;
      fmt.withContext("getContext()");

      attrType = evaluateAttrLlvmType(errs, preludes, fmt, attr, arg.name,
                                      context, symbols);
      if (attrType.empty())
        return false;

      if (!prelude.empty()) {
        errs << "got a non-empty prelude when determining the LLVM type of "
             << arg.name << '\n';
        errs << prelude;
        errs << "this is currently unsupported\n";
        return false;
      }
    }

    m_attrTypes.push_back(attrType);
  }

  return true;
}

SmallVector<NamedValue> OperationBase::getFullArguments() const {
  SmallVector<NamedValue> args;
  if (m_superclass)
    args = m_superclass->getFullArguments();
  args.insert(args.end(), m_arguments.begin(), m_arguments.end());
  return args;
}

unsigned OperationBase::getNumFullArguments() const {
  size_t argSize = m_arguments.size();

  // Variadic argument lists can be of size zero,
  // so do not add them to the total number of arguments.
  if (m_hasVariadicArgument)
    --argSize;

  if (m_superclass)
    return m_superclass->getNumFullArguments() + argSize;
  return argSize;
}

void OperationBase::emitArgumentAccessorDeclarations(llvm::raw_ostream &out,
                                                     FmtContext &fmt) const {
  for (const auto &arg : m_arguments) {
    std::string defaultDeclaration = "$0 get$1();";
    if (!arg.type->isVarArgList() && !arg.type->isImmutable()) {
      defaultDeclaration += R"(
        void set$1($0 $2);
      )";
    }

    out << tgfmt(defaultDeclaration, &fmt, arg.type->getGetterCppType(),
                 convertToCamelFromSnakeCase(arg.name, true), arg.name);
  }
}

void AccessorBuilder::emitAccessorDefinitions() const {
  // We do not generate a setter for variadic arguments for now.
  emitGetterDefinition();
  if (!m_arg.type->isVarArgList())
    emitSetterDefinition();
}

void AccessorBuilder::emitGetterDefinition() const {
  std::string fromLlvm;

  if (!m_arg.type->isVarArgList()) {
    fromLlvm = tgfmt("getArgOperand($index)", &m_fmt);
    if (auto *attr = dyn_cast<Attr>(m_arg.type))
      fromLlvm = tgfmt(attr->getFromLlvmValue(), &m_fmt, fromLlvm);
    else if (m_arg.type->isTypeArg())
      fromLlvm += "->getType()";
  } else {
    fromLlvm = tgfmt(
        R"(::llvm::make_range(
            value_op_iterator(arg_begin() + $index),
            value_op_iterator(arg_end())))",
        &m_fmt);
  }

  m_fmt.addSubst("fromLlvm", fromLlvm);

  m_os << tgfmt(R"(
      $cppType $_op::get$Name() {
        return $fromLlvm;
      })",
                &m_fmt);
}

void AccessorBuilder::emitSetterDefinition() const {
  if (m_arg.type->isImmutable())
    return;

  std::string toLlvm = m_arg.name;

  if (auto *attr = dyn_cast<Attr>(m_arg.type)) {
    toLlvm = tgfmt(attr->getToLlvmValue(), &m_fmt, toLlvm, m_argTypeString);
  } else if (m_arg.type->isTypeArg()) {
    toLlvm = llvm::formatv("llvm::PoisonValue::get({0})", toLlvm);
  }
  m_fmt.addSubst("toLlvm", toLlvm);

  m_os << tgfmt(R"(

      void $_op::set$Name($cppType $name) {
        setArgOperand($index, $toLlvm);
      })",
                &m_fmt);
}

void OperationBase::emitArgumentAccessorDefinitions(llvm::raw_ostream &out,
                                                    FmtContext &fmt) const {
  unsigned numSuperclassArgs = 0;
  if (m_superclass)
    numSuperclassArgs = m_superclass->getNumFullArguments();

  for (const auto &indexedArg : llvm::enumerate(m_arguments)) {
    FmtContextScope scope(fmt);

    const NamedValue &arg = indexedArg.value();
    AccessorBuilder builder{fmt, out, arg, m_attrTypes[indexedArg.index()]};

    fmt.withContext("getContext()");
    fmt.addSubst("index", Twine(numSuperclassArgs + indexedArg.index()));
    fmt.addSubst("cppType", arg.type->getGetterCppType());
    fmt.addSubst("name", arg.name);
    fmt.addSubst("Name", convertToCamelFromSnakeCase(arg.name, true));

    builder.emitAccessorDefinitions();
  }
}

std::unique_ptr<OpClass>
OpClass::parse(raw_ostream &errs, GenDialectsContext &context, Record *record) {
  auto opClass = std::make_unique<OpClass>();
  opClass->name = record->getName();

  if (!opClass->init(errs, context, record))
    return {};

  OpClass *ptr = opClass.get();
  opClass->dialect()->opClasses.push_back(opClass.get());
  if (opClass->superclass())
    opClass->superclass()->subclasses.push_back(ptr);

  return opClass;
}

static std::string evaluateAttrLlvmType(raw_ostream &errs, raw_ostream &out,
                                        FmtContext &fmt, Attr *attr,
                                        StringRef name,
                                        GenDialectsContext &context,
                                        SymbolTable &symbols) {
  Scope attrTypeScope;
  ConstraintSystem attrTypeSystem(context, attrTypeScope);
  Variable *typeVariable =
      attrTypeScope.getVariable((Twine(name) + "Type").str());
  if (!attrTypeSystem.addConstraint(errs, attr->getLlvmType(), typeVariable)) {
    errs << "... while trying to obtain llvm::Type of " << name << '\n';
    return {};
  }

  Assignment assignment;
  Evaluator attrTypeEval(symbols, assignment, attrTypeSystem, out, fmt);
  std::string attrType = attrTypeEval.evaluate(typeVariable);
  if (attrType.empty()) {
    errs << "Failed to obtain llvm::Type of " << name << ":\n";
    errs << attrTypeEval.takeErrorMessages();
    return {};
  }
  return attrType;
}

// Implement constructor here, where BuilderMethod is fully defined.
Operation::Operation(GenDialectsContext &context) : m_system(context, m_scope) {}

// Default destructor instantiated explicitly to avoid having to add more
// includes in the header.
Operation::~Operation() = default;

bool Operation::parse(raw_ostream &errs, GenDialectsContext *context,
                      GenDialect *dialect, Record *record) {
  auto op = std::make_unique<Operation>(*context);

  if (!op->init(errs, *context, record))
    return false;

  assert(op->dialect() == dialect);

  if (op->superclass())
    op->superclass()->operations.push_back(op.get());

  op->name = record->getName();
  op->mnemonic = record->getValueAsString("mnemonic");
  for (Record *traitRec : record->getValueAsListOfDefs("traits"))
    op->traits.push_back(context->getTrait(traitRec));

  EvaluationPlanner evaluation(op->m_system);

  for (const auto &arg : op->getFullArguments()) {
    auto variable = op->m_scope.getVariable(arg.name);
    ConstraintSystem singletonSystem{*context, op->m_scope};
    if (!singletonSystem.addConstraint(errs, arg.constraint, variable))
      return false;

    if (!arg.type) {
      errs << "No type provided for " << arg.name << '\n';
      return false;
    }

    if (!isa<Attr>(arg.type)) {
      EvaluationPlanner planner{singletonSystem};
      if (!planner.getPlan(variable))
        op->m_haveArgumentOverloads = true;
    }

    op->m_system.merge(std::move(singletonSystem));
  }

  DagInit *results = record->getValueAsDag("results");
  if (results->getOperatorAsDef({})->getName() != "outs") {
    errs << "result list operator must be 'outs'\n";
    return false;
  }
  if (results->getNumArgs() > 1) {
    errs << "multiple result values are not supported\n";
    return false;
  }

  auto parsedResults = NamedValue::parseList(
      errs, *context, results, 0, NamedValue::Parser::OperationResults);
  if (!parsedResults.has_value())
    return false;
  op->results = std::move(*parsedResults);

  for (const auto &result : op->results) {
    auto variable = op->m_scope.getVariable(result.name);
    ConstraintSystem singletonSystem{*context, op->m_scope};
    if (!singletonSystem.addConstraint(errs, result.constraint, variable))
      return false;

    EvaluationPlanner planner{singletonSystem};
    if (!planner.getPlan(variable))
      op->m_haveResultOverloads = true;

    op->m_system.merge(std::move(singletonSystem));
  }

  op->m_defaultBuilderHasExplicitResultType =
      record->getValueAsBit("defaultBuilderHasExplicitResultType");

  ListInit *verifier = record->getValueAsListInit("verifier");
  for (Init *constraint : *verifier) {
    if (!op->m_system.addConstraint(errs, constraint, nullptr))
      return false;
  }

  // Derive the default builder method.
  BuilderMethod builder{*op};
  const auto fullArguments = op->getFullArguments();
  SmallVector<std::string> opArgumentBuilderNames;

  for (const auto &opArg : fullArguments) {
    opArgumentBuilderNames.push_back(builder.m_symbolTable.chooseName(
        convertToCamelFromSnakeCase(opArg.name)));
  }

  builder.m_instName = builder.m_symbolTable.chooseName("instName");
  builder.m_context = builder.m_symbolTable.chooseName("context");
  builder.m_builder = builder.m_symbolTable.chooseName({"b", "builder"});

  raw_string_ostream prelude(builder.m_prelude);
  FmtContext fmt;
  fmt.withContext(builder.m_context);
  fmt.withBuilder(builder.m_builder);

  Assignment assignments;
  Evaluator eval(builder.m_symbolTable, assignments, op->m_system, prelude,
                 fmt);

  for (const auto &[opArg, builderName] :
       llvm::zip(fullArguments, opArgumentBuilderNames)) {
    auto variable = op->m_scope.findVariable(opArg.name);

    std::string constraintValue = builderName;
    if (opArg.type->isValueArg())
      constraintValue += "->getType()";

    assignments.assign(variable, constraintValue);
  }

  if (!op->results.empty()) {
    assert(op->results.size() == 1);
    StringRef resultName = op->results[0].name;

    if (op->m_defaultBuilderHasExplicitResultType) {
      builder.m_resultType = builder.m_symbolTable.chooseName(
          convertToCamelFromSnakeCase(resultName, false) + "Type");

      BuilderMethod::Arg builderArg;
      builderArg.name = builder.m_resultType;
      builderArg.cppType = "::llvm::Type*";
      builder.m_arguments.push_back(std::move(builderArg));
    } else {
      auto variable = op->m_scope.findVariable(op->results[0].name);
      builder.m_resultType = eval.evaluate(variable);
      if (builder.m_resultType.empty()) {
        errs << "Failed to deduce result type for default builder:\n";
        errs << eval.takeErrorMessages();
        return false;
      }
    }
  }

  builder.m_beginOpArguments = builder.m_arguments.size();
  for (const auto &[opArg, builderName] :
       llvm::zip(fullArguments, opArgumentBuilderNames)) {
    BuilderMethod::Arg builderArg;
    builderArg.name = builderName;
    builderArg.cppType = opArg.type->getBuilderCppType();

    builder.m_arguments.push_back(std::move(builderArg));

    std::string attrType;
    if (auto *attr = dyn_cast<Attr>(opArg.type)) {
      attrType = evaluateAttrLlvmType(errs, prelude, fmt, attr, builderName,
                                      *context, builder.m_symbolTable);
      if (attrType.empty())
        return false;
    }
    builder.m_attrTypes.push_back(std::move(attrType));
  }

  op->m_builders.push_back(std::move(builder));

  ++dialect->operationCounts[op->mnemonic];
  dialect->operations.push_back(std::move(op));

  return true;
}

void Operation::emitVerifierMethod(llvm::raw_ostream &out,
                                   FmtContext &fmt) const {
  SymbolTable symbols;
  FmtContextScope scope{fmt};
  fmt.withContext(symbols.chooseName("context"));
  fmt.addSubst("_errs", symbols.chooseName("errs"));

  bool emitArgCountVerifier = true;
  if (m_hasVariadicArgument) {
    if (getNumFullArguments() == 0) {
      // If the only argument in an operation is a variadic argument list,
      // getNumFullArguments() will return zero. Thus, prevent us from emitting
      // arg_size() < 0 verifier checks.
      emitArgCountVerifier = false;
    } else {
      fmt.addSubst("_comparator", "<");
      fmt.addSubst("_at_least", "at least ");
    }
  } else {
    fmt.addSubst("_comparator", "!=");
    fmt.addSubst("_at_least", "");
  }

  out << tgfmt(R"(
    bool $_op::verifier(::llvm::raw_ostream &$_errs) {
      ::llvm::LLVMContext &$_context = getModule()->getContext();
      (void)$_context;

      using ::llvm_dialects::printable;
  )",
               &fmt);

  if (emitArgCountVerifier) {
    out << tgfmt(R"(
      if (arg_size() $_comparator $0) {
        $_errs << "  wrong number of arguments: " << arg_size()
               << ", expected $_at_least$0\n";
        return false;
      }
  )",
                 &fmt, getNumFullArguments());
  }

  Assignment assignment;
  Evaluator eval(symbols, assignment, m_system, out, fmt);

  SmallVector<NamedValue> variables = getFullArguments();
  for (const auto &enumeratedArgument : llvm::enumerate(variables)) {
    auto index = enumeratedArgument.index();
    const NamedValue &argument = enumeratedArgument.value();

    if (auto *attr = dyn_cast<Attr>(argument.type)) {
      FmtContextScope scope{fmt};
      fmt.addSubst("type", evaluateAttrLlvmType(
                               llvm::errs(), out, fmt, attr, argument.name,
                               m_system.getContext(), symbols));
      fmt.addSubst("name", argument.name);
      fmt.addSubst("index", Twine(index));

      out << tgfmt(R"(
        if (getArgOperand($index)->getType() != $type) {
          $_errs << "  argument $index ($name) has type: "
                 << *getArgOperand($index)->getType() << '\n';
          $_errs << "  expected: " << *$type << '\n';
          return false;
        }
      )",
                   &fmt);
    }
  }

  variables.append(results.begin(), results.end());
  for (const auto &variable : variables) {
    FmtContextScope scope{fmt};

    std::string name = convertToCamelFromSnakeCase(variable.name, false);
    if (variable.type->isValueArg())
      name += "Type";
    name = symbols.chooseName(name);

    if (!variable.type->isVarArgList()) {
      fmt.addSubst("type", variable.type->getCppType());
      fmt.addSubst("name", name);
      fmt.addSubst("getter", convertToCamelFromSnakeCase(variable.name, true));

      if (variable.type->isValueArg()) {
        fmt.addSubst("value", tgfmt("get$getter()->getType()", &fmt).str());
      } else {
        fmt.addSubst("value", tgfmt("get$getter()", &fmt).str());
      }

      out << tgfmt("$type const $name = $value;\n(void)$name;\n", &fmt);
    }

    auto systemVariable = m_scope.findVariable(variable.name);
    assignment.assign(systemVariable, name);
  }

  if (!eval.check(true)) {
    llvm::errs() << "failed to generate verifier method for " << name << ":\n";
    llvm::errs() << eval.takeErrorMessages();
    report_fatal_error("failed to generate verifier");
  }

  out << "  return true;\n}\n\n";
}

void BuilderMethod::emitDeclaration(raw_ostream &out, FmtContext &fmt) const {
  FmtContextScope scope{fmt};
  fmt.withBuilder(m_builder);
  assert(m_instName.size() > 0);
  fmt.addSubst("_instname", m_instName);

  out << tgfmt("static $_op* create(::llvm_dialects::Builder& $_builder", &fmt);
  for (const auto &builderArg : m_arguments) {
    out << ", " << builderArg.cppType << " " << builderArg.name;
  }
  out << tgfmt(", const llvm::Twine &$_instname = \"\");\n", &fmt);
}

void BuilderMethod::emitDefinition(raw_ostream &out, FmtContext &fmt,
                                   GenDialectsContext &genContext) const {
  const auto fullArguments = m_operation.getFullArguments();
  const auto opArgBuilderArgs =
      ArrayRef(m_arguments).drop_front(m_beginOpArguments);

  SymbolTable symbols{&m_symbolTable};
  FmtContextScope scope{fmt};
  fmt.withBuilder(m_builder);
  fmt.withContext(m_context);
  fmt.addSubst("_module", symbols.chooseName("module"));
  assert(m_instName.size() > 0);
  fmt.addSubst("_instname", m_instName);

  out << tgfmt("$_op* $_op::create(llvm_dialects::Builder& $_builder", &fmt);
  for (const auto &builderArg : m_arguments)
    out << tgfmt(", $0 $1", &fmt, builderArg.cppType, builderArg.name);

  out << tgfmt(R"(, const llvm::Twine &$_instname) {
    ::llvm::LLVMContext& $_context = $_builder.getContext();
    (void)$_context;
    ::llvm::Module& $_module = *$_builder.GetInsertBlock()->getModule();
  
  )",
               &fmt);

  out << m_prelude;

  fmt.addSubst("attrs", symbols.chooseName({"attrs", "attributes"}));
  if (m_operation.getAttributeListIdx() < 0) {
    out << tgfmt("const ::llvm::AttributeList $attrs;\n", &fmt);
  } else {
    out << tgfmt(R"(
      const ::llvm::AttributeList $attrs
          = $Dialect::get($_context).getAttributeList($0);
    )",
                 &fmt, m_operation.getAttributeListIdx());
  }

  if (!m_resultType.empty()) {
    assert(m_operation.results.size() == 1);
    fmt.addSubst("resultType", m_resultType);
  } else {
    assert(m_operation.results.size() == 0);

    Assignment assignment;
    Scope scope;
    ConstraintSystem system{genContext, scope};
    Evaluator eval(symbols, assignment, system, out, fmt);
    Variable *variable = scope.getVariable("void");
    bool success =
        system.addConstraint(llvm::errs(), genContext.getVoidTy(), variable);
    (void)success;
    assert(success);
    fmt.addSubst("resultType", eval.evaluate(variable));
  }

  if (m_operation.haveResultOverloads()) {
    fmt.addSubst("fnName", symbols.chooseName("mangledName"));

    out << tgfmt(R"(
      std::string $fnName =
          ::llvm_dialects::getMangledName(s_name, {$resultType});
    )",
                 &fmt);
  } else {
    fmt.addSubst("fnName", "s_name");
  }

  fmt.addSubst("fnType", symbols.chooseName({"fnType", "functionType"}));
  if (m_operation.haveArgumentOverloads()) {
    out << tgfmt(
        "auto $fnType = ::llvm::FunctionType::get($resultType, true);\n", &fmt);
  } else {
    SmallVector<std::string> argTypes;
    for (const auto &[opArg, builderArg, attrType] :
         llvm::zip(fullArguments, opArgBuilderArgs, m_attrTypes)) {
      if (!attrType.empty())
        argTypes.push_back(attrType);
      else
        argTypes.push_back(builderArg.name + "->getType()");
    }
    out << tgfmt("auto $fnType = ::llvm::FunctionType::get($resultType, {\n",
                 &fmt);
    for (const auto &argType : argTypes)
      out << argType << ",\n";
    out << "}, false);\n";
  }

  fmt.addSubst("fn", symbols.chooseName({"fn", "function"}));
  out << tgfmt(R"(
    auto $fn = $_module.getOrInsertFunction($fnName, $fnType, $attrs);
    ::llvm::SmallString<32> newName;
    for (unsigned i = 0; !::llvm::isa<::llvm::Function>($fn.getCallee()) ||
                         ::llvm::cast<::llvm::Function>($fn.getCallee())->getFunctionType() != $fn.getFunctionType(); i++) {
      // If a function with the same name but a different types already exists,
      // we get a bitcast of a function or a function with the wrong type.
      // Try new names until we get one with the correct type.
      newName = "";
      ::llvm::raw_svector_ostream newNameStream(newName);
      newNameStream << $fnName << "_" << i;
      $fn = $_module.getOrInsertFunction(newNameStream.str(), $fnType, $attrs);
    }
    assert(::llvm::isa<::llvm::Function>($fn.getCallee()));
    assert($fn.getFunctionType() == $fnType);
    assert(::llvm::cast<::llvm::Function>($fn.getCallee())->getFunctionType() == $fn.getFunctionType());

  )",
               &fmt);

  if (!fullArguments.empty()) {
    for (const auto &[opArg, builderArg, attrType] :
         llvm::zip(fullArguments, opArgBuilderArgs, m_attrTypes)) {
      if (auto *attr = dyn_cast<Attr>(opArg.type)) {
        out << tgfmt(attr->getCheck(), &fmt, builderArg.name) << '\n';
      }
    }

    fmt.addSubst("args", symbols.chooseName({"args", "arguments"}));
    out << tgfmt("::llvm::SmallVector<::llvm::Value*, $0> $args = {\n", &fmt,
                 m_operation.getNumFullArguments());
    const NamedValue *varArg = nullptr;

    size_t ArgIdx = 0;
    for (const auto &[opArg, builderArg, attrType] :
         llvm::zip_first(fullArguments, opArgBuilderArgs, m_attrTypes)) {
      if (ArgIdx > 0 && !opArg.type->isVarArgList())
        out << ",\n";

      if (auto *attr = dyn_cast<Attr>(opArg.type)) {
        out << tgfmt(attr->getToLlvmValue(), &fmt, builderArg.name, attrType);
      } else if (opArg.type->isTypeArg()) {
        out << tgfmt("::llvm::PoisonValue::get($0)", &fmt, builderArg.name);
      } else if (opArg.type->isVarArgList()) {
        varArg = &opArg;
        // Assume no other arguments follow the variadic argument.
        --ArgIdx;
        break;
      } else {
        out << builderArg.name;
      }

      ++ArgIdx;
    }

    std::string varArgInitializer = "";
    if (varArg) {
      varArgInitializer = tgfmt(R"(
        $args.append($0.begin(), $0.end());
      )",
                                &fmt, varArg->name);
    }

    fmt.addSubst("varArgInitializer", varArgInitializer);
    out << tgfmt(R"(
      };
      $varArgInitializer
      return ::llvm::cast<$_op>($_builder.CreateCall($fn, $args, $_instname));
    )",
                 &fmt);
  } else {
    out << tgfmt("return ::llvm::cast<$_op>($_builder.CreateCall($fn, std::nullopt, $_instname));\n",
                 &fmt);
  }

  out << "}\n\n";
}
