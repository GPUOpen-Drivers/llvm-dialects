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
#include "llvm-dialects/TableGen/LlvmTypeBuilder.h"
#include "llvm-dialects/TableGen/Predicates.h"

#include "llvm/TableGen/Record.h"

using namespace llvm;
using namespace llvm_dialects;

Operation::~Operation() = default;

static std::vector<NamedValue> parseArguments(GenDialectsContext *context,
                                                Record *rec) {
  Record *superClassRec = rec->getValueAsDef("superclass");
  OpClass *superclass = context->getOpClass(superClassRec);
  DagInit *argsInit = rec->getValueAsDag("arguments");
  std::vector<NamedValue> arguments;

  if (argsInit->getOperatorAsDef({})->getName() != "ins")
    report_fatal_error(Twine(rec->getName()) +
                       " argument operator must be 'ins'");

  for (unsigned i = 0; i < argsInit->getNumArgs(); ++i) {
    if (superclass && i == 0) {
      if (argsInit->getArgName(0) ||
          argsInit->getArg(0) != superClassRec->getDefInit())
        report_fatal_error(Twine(rec->getName()) +
                           ": superclass must be first in arguments list");
      continue;
    }

    NamedValue opArg;
    opArg.name = argsInit->getArgNameStr(i);
    if (auto *arg = dyn_cast_or_null<DefInit>(argsInit->getArg(i)))
      opArg.type = context->getConstraint(arg->getDef());
    if (!opArg.type) {
      report_fatal_error(Twine(rec->getName()) + " argument " + Twine(i) +
                         ": bad type constraint");
    }
    arguments.push_back(std::move(opArg));
  }

  return arguments;
}

std::unique_ptr<OpClass> OpClass::parse(GenDialectsContext *context,
                                        Record *record) {
  auto opClass = std::make_unique<OpClass>();
  opClass->dialect = context->getDialect(record->getValueAsDef("dialect"));
  opClass->name = record->getName();
  opClass->superclass = context->getOpClass(record->getValueAsDef("superclass"));
  opClass->arguments = parseArguments(context, record);

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

void Operation::parse(GenDialectsContext *context, GenDialect *dialect,
                      Record *record) {
  auto op = std::make_unique<Operation>();
  op->superclass = context->getOpClass(record->getValueAsDef("superclass"));
  if (op->superclass)
    op->superclass->operations.push_back(op.get());
  op->name = record->getName();
  op->mnemonic = record->getValueAsString("mnemonic");
  for (Record *traitRec : record->getValueAsListOfDefs("traits"))
    op->traits.push_back(context->getTrait(traitRec));

  op->arguments = parseArguments(context, record);
  for (const auto &arg : op->arguments) {
    if (!isa<Type>(arg.type) && !isa<Attr>(arg.type))
      op->m_haveArgumentOverloads = true;
  }

  DagInit *results = record->getValueAsDag("results");
  assert(results->getOperatorAsDef({})->getName() == "outs");
  assert(results->getNumArgs() <= 1 &&
          "multiple result values not supported");
  for (unsigned i = 0; i < results->getNumArgs(); ++i) {
    NamedValue opResult;
    opResult.name = results->getArgNameStr(i);
    if (auto *result = dyn_cast_or_null<DefInit>(results->getArg(i)))
      opResult.type = context->getConstraint(result->getDef());
    if (!opResult.type) {
      report_fatal_error(Twine("Operation '") + op->mnemonic + "' result " +
                          Twine(i) + ": bad type constraint");
    }
    if (isa<Attr>(opResult.type)) {
      report_fatal_error(Twine("Operation '") + op->mnemonic +
                           "': result cannot be an attribute");
    }

    if (!isa<Type>(opResult.type))
      op->m_haveResultOverloads = true;

    op->results.push_back(std::move(opResult));
  }

  ListInit *verifier = record->getValueAsListInit("verifier");
  for (Init *ruleInit : *verifier) {
    auto *rule = dyn_cast<DagInit>(ruleInit);
    if (!rule) {
      report_fatal_error(Twine("Operation '") + op->mnemonic +
                          "': verifier rules must be dags");
    }

    op->verifier.push_back(context->parsePredicateExpr(rule));
  }

  // Derive the default builder method.
  BuilderMethod builder{*op};
  const auto fullArguments = op->getFullArguments();
  SmallVector<std::string> opArgumentBuilderNames;

  for (const auto &opArg : fullArguments) {
    opArgumentBuilderNames.push_back(builder.m_symbolTable.chooseName(
        convertToCamelFromSnakeCase(opArg.name)));
  }

  if (!op->results.empty()) {
    assert(op->results.size() == 1);

    if (!isa<Type>(op->results[0].type)) {
      // Check if the builder can derive the result type from a SameTypes
      // constraint.
      for (const auto &expr : op->verifier) {
        if (const auto *apply = dyn_cast<PredicateApply>(expr.get())) {
          if (apply->getPredicate()->getKind() == Constraint::Kind::SameTypes) {
            bool haveResult = false;
            bool haveArgument = false;
            size_t argumentIdx = 0;
            for (const auto &applyArg : apply->arguments()) {
              if (applyArg == op->results[0].name) {
                haveResult = true;
              } else if (!haveArgument) {
                auto it =
                    llvm::find_if(op->arguments,
                        [&](const NamedValue &opArg) {
                          return applyArg == opArg.name;
                        });
                if (it != op->arguments.end()) {
                  haveArgument = true;
                  argumentIdx = std::distance(op->arguments.begin(), it);
                }
              }
            }

            if (haveResult && haveArgument) {
              builder.m_resultTypeFromBuilderArg =
                  opArgumentBuilderNames[argumentIdx] + "->getType()";
              break;
            }
          }
        }
      }

      if (builder.m_resultTypeFromBuilderArg.empty()) {
        builder.m_resultTypeFromBuilderArg = builder.m_symbolTable.chooseName(
            convertToCamelFromSnakeCase(op->results[0].name) + "Type");

        BuilderMethod::Arg builderArg;
        builderArg.name = builder.m_resultTypeFromBuilderArg;
        builderArg.cppType = "::llvm::Type*";
        builder.m_arguments.push_back(std::move(builderArg));
      }
    }
  }

  builder.m_beginOpArguments = builder.m_arguments.size();
  for (const auto &[opArg, builderName]
      : llvm::zip(fullArguments, opArgumentBuilderNames)) {
    BuilderMethod::Arg builderArg;
    builderArg.name = builderName;
    builderArg.cppType = opArg.type->getCppType();
    builder.m_arguments.push_back(std::move(builderArg));
  }

  builder.m_builder = builder.m_symbolTable.chooseName({"b", "builder"});

  op->m_builders.push_back(std::move(builder));

  dialect->operations.push_back(std::move(op));
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
  fmt.withContext(symbols.chooseName("context"));
  fmt.addSubst("_module", symbols.chooseName("module"));

  LlvmTypeBuilder typeBuilder{out, symbols, fmt};

  out << tgfmt("$_op* $_op::create(llvm_dialects::Builder& $_builder", &fmt);
  for (const auto& builderArg : m_arguments)
    out << tgfmt(", $0 $1", &fmt, builderArg.cppType, builderArg.name);

  out << tgfmt(R"() {
    ::llvm::LLVMContext& $_context = $_builder.getContext();
    ::llvm::Module& $_module = *$_builder.GetInsertBlock()->getModule();
  
  )", &fmt);

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

  if (!m_resultTypeFromBuilderArg.empty()) {
    assert(m_operation.results.size() == 1);
    fmt.addSubst("resultType", m_resultTypeFromBuilderArg);
  } else {
    assert(m_operation.results.size() <= 1);
    Constraint *theResultType = m_operation.results.empty()
                                    ? genContext.getVoidTy()
                                    : m_operation.results[0].type;
    fmt.addSubst("resultType", typeBuilder.build(theResultType));
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
    for (const auto& [opArg, builderArg]
        : llvm::zip(fullArguments, opArgBuilderArgs)) {
      if (isa<Attr>(opArg.type))
        argTypes.push_back(typeBuilder.build(opArg.type));
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

  )", &fmt);

  if (!fullArguments.empty()) {
    SmallVector<std::string> argTypes;
    for (const auto& [opArg, builderArg]
        : llvm::zip(fullArguments, opArgBuilderArgs)) {
      if (isa<Attr>(opArg.type))
        argTypes.push_back(typeBuilder.build(opArg.type));
      else
        argTypes.push_back("<skip>");
    }

    fmt.addSubst("args", symbols.chooseName({"args", "arguments"}));
    out << tgfmt("::llvm::Value* const $args[] = {\n", &fmt);
    for (const auto& [opArg, builderArg, argType]
              : llvm::zip_first(fullArguments, opArgBuilderArgs, argTypes)) {
      if (auto* attr = dyn_cast<Attr>(opArg.type)) {
        out << tgfmt(attr->getToLlvmValue(), &fmt, builderArg.name, argType);
      } else if (isa<TypeArg>(opArg.type)) {
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
