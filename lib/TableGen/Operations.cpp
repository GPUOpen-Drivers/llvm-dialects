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

// Default destructor instantiated explicitly to avoid having to add more
// includes in the header.
Operation::~Operation() = default;

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

std::unique_ptr<OpClass>
OpClass::parse(raw_ostream &errs, GenDialectsContext &context, Record *record) {
  auto opClass = std::make_unique<OpClass>();
  opClass->dialect = context.getDialect(record->getValueAsDef("dialect"));
  opClass->name = record->getName();
  opClass->superclass = context.getOpClass(record->getValueAsDef("superclass"));

  auto arguments = parseArguments(errs, context, record);
  if (!arguments.has_value())
    return {};
  opClass->arguments = std::move(*arguments);

  OpClass *ptr = opClass.get();
  opClass->dialect->opClasses.push_back(opClass.get());
  if (opClass->superclass)
    opClass->superclass->subclasses.push_back(ptr);

  return opClass;
}

SmallVector<NamedValue> OpClass::getFullArguments() const {
  SmallVector<NamedValue> args;
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

bool Operation::parse(raw_ostream &errs, GenDialectsContext *context,
                      GenDialect *dialect, Record *record) {
  auto op = std::make_unique<Operation>(*context);
  op->superclass = context->getOpClass(record->getValueAsDef("superclass"));
  if (op->superclass)
    op->superclass->operations.push_back(op.get());
  op->name = record->getName();
  op->mnemonic = record->getValueAsString("mnemonic");
  for (Record *traitRec : record->getValueAsListOfDefs("traits"))
    op->traits.push_back(context->getTrait(traitRec));

  auto arguments = parseArguments(errs, *context, record);
  if (!arguments.has_value())
    return false;
  op->arguments = std::move(*arguments);

  EvaluationPlanner evaluation(op->m_system);

  for (const auto &arg : op->getFullArguments()) {
    auto variable = op->m_scope.getVariable(arg.name);
    ConstraintSystem singletonSystem{*context, op->m_scope};
    if (!singletonSystem.addConstraint(errs, arg.constraint, variable))
      return false;

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
  for (const auto &[opArg, builderName]
      : llvm::zip(fullArguments, opArgumentBuilderNames)) {
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

  dialect->operations.push_back(std::move(op));

  return true;
}

SmallVector<NamedValue> Operation::getFullArguments() const {
  SmallVector<NamedValue> args;
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

void Operation::emitVerifierMethod(llvm::raw_ostream &out,
                                   FmtContext &fmt) const {
  SymbolTable symbols;
  FmtContextScope scope{fmt};
  fmt.withContext(symbols.chooseName("context"));
  fmt.addSubst("_errs", symbols.chooseName("errs"));

  out << tgfmt(R"(
    bool $_op::verifier(::llvm::raw_ostream &$_errs) {
      ::llvm::LLVMContext &$_context = getModule()->getContext();
      (void)$_context;

      using ::llvm_dialects::printable;

      if (arg_size() != $0) {
        $_errs << "  wrong number of arguments: " << arg_size()
               << ", expected $0\n";
        return false;
      }
  )",
               &fmt, getNumFullArguments());

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
    fmt.addSubst("type", variable.type->getCppType());
    fmt.addSubst("getter", convertToCamelFromSnakeCase(variable.name, true));

    std::string name = convertToCamelFromSnakeCase(variable.name, false);
    if (variable.type->isValueArg())
      name += "Type";
    name = symbols.chooseName(name);
    fmt.addSubst("name", name);

    if (variable.type->isValueArg()) {
      fmt.addSubst("value", tgfmt("get$getter()->getType()", &fmt).str());
    } else {
      fmt.addSubst("value", tgfmt("get$getter()", &fmt).str());
    }

    out << tgfmt("$type const $name = $value;\n(void)$name;\n", &fmt);

    auto systemVariable = m_scope.findVariable(variable.name);
    assignment.assign(systemVariable, name);
  }

  if (!eval.check()) {
    llvm::errs() << "failed to generate verifier method for " << name << ":\n";
    llvm::errs() << eval.takeErrorMessages();
    report_fatal_error("failed to generate verifier");
  }

  out << "  return true;\n}\n\n";
}

void BuilderMethod::emitDeclaration(raw_ostream &out, FmtContext &fmt) const {
  FmtContextScope scope{fmt};
  fmt.withBuilder(m_builder);

  out << tgfmt("static $_op* create(::llvm_dialects::Builder& $_builder", &fmt);
  for (const auto& builderArg : m_arguments)
    out << tgfmt(", $0 $1", &fmt, builderArg.cppType, builderArg.name);
  out << ");\n";
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

  out << tgfmt("$_op* $_op::create(llvm_dialects::Builder& $_builder", &fmt);
  for (const auto& builderArg : m_arguments)
    out << tgfmt(", $0 $1", &fmt, builderArg.cppType, builderArg.name);

  out << tgfmt(R"() {
    ::llvm::LLVMContext& $_context = $_builder.getContext();
    ::llvm::Module& $_module = *$_builder.GetInsertBlock()->getModule();
  
  )", &fmt);

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
    )", &fmt);
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
      fn = $_module.getOrInsertFunction(newNameStream.str(), $fnType, $attrs);
    }
    assert(::llvm::isa<::llvm::Function>($fn.getCallee()));
    assert($fn.getFunctionType() == $fnType);
    assert(::llvm::cast<::llvm::Function>($fn.getCallee())->getFunctionType() == $fn.getFunctionType());

  )", &fmt);

  if (!fullArguments.empty()) {
    for (const auto &[opArg, builderArg, attrType] :
         llvm::zip(fullArguments, opArgBuilderArgs, m_attrTypes)) {
      if (auto *attr = dyn_cast<Attr>(opArg.type)) {
        out << tgfmt(attr->getCheck(), &fmt, builderArg.name) << '\n';
      }
    }

    fmt.addSubst("args", symbols.chooseName({"args", "arguments"}));
    out << tgfmt("::llvm::Value* const $args[] = {\n", &fmt);
    for (const auto& [opArg, builderArg, attrType]
              : llvm::zip_first(fullArguments, opArgBuilderArgs, m_attrTypes)) {
      if (auto* attr = dyn_cast<Attr>(opArg.type)) {
        out << tgfmt(attr->getToLlvmValue(), &fmt, builderArg.name, attrType);
      } else if (opArg.type->isTypeArg()) {
        out << tgfmt("::llvm::PoisonValue::get($0)", &fmt, builderArg.name);
      } else {
        out << builderArg.name;
      }
      out << ",\n";
    }

    out << tgfmt(R"(
      };

      return ::llvm::cast<$_op>($_builder.CreateCall($fn, $args));
    )", &fmt);
  } else {
    out << tgfmt("return ::llvm::cast<$_op>($_builder.CreateCall($fn));\n",
                 &fmt);
  }

  out << "}\n\n";
}
