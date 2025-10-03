/*
 ***********************************************************************************************************************
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
 ***********************************************************************************************************************
 */

#include "llvm-dialects/TableGen/DialectType.h"

#include "llvm-dialects/TableGen/Dialects.h"
#include "llvm-dialects/TableGen/Evaluator.h"
#include "llvm-dialects/TableGen/Format.h"

#include "llvm/TableGen/Record.h"

using namespace llvm;
using namespace llvm_dialects;

bool DialectType::init(raw_ostream &errs, GenDialectsContext &context,
                       const Init *theInit) {
  if (!BaseCppPredicate::init(errs, context, theInit))
    return false;

  RecordTy *record = cast<DefInit>(theInit)->getDef();

  m_dialectRec = record->getValueAsDef("dialect");
  if (!m_dialectRec->isSubClassOf("Dialect")) {
    report_fatal_error(Twine("'dialect' field of type constraint '") +
                       record->getName() + "' is not a subclass of Dialect");
  }
  m_name = record->getName().str();
  m_mnemonic = record->getValueAsString("mnemonic");
  m_summary = record->getValueAsString("summary");
  m_description = record->getValueAsString("description");

  if (auto *dag =
          cast<DagInit>(record->getValue("representation")->getValue())) {
    if (cast<DefInit>(dag->getOperator())->getDef()->getName() ==
        "repr_struct") {
      m_structBacked = true;

      if (dag->getNumArgs() != 1) {
        errs << "'repr_struct' expects exactly one type argument\n";
        return false;
      }
      m_structSentinelBitWidth =
          llvm::cast<llvm::IntInit>(
              llvm::cast<llvm::DagInit>(dag->getArg(0))->getArg(0))
              ->getValue();
    }
  }
  if (auto *p = record->getValue("structPrefix"))
    m_structPrefix = record->getValueAsString("structPrefix").str();
  else
    m_structPrefix =
        (m_dialectRec->getValueAsString("name").str() + "." + m_mnemonic + ".");

  for (unsigned argIdx = 0; argIdx < m_arguments.size(); ++argIdx)
    m_canDerive.push_back(true);
  m_canCheckFromSelf = true;

  m_defaultGetterHasExplicitContextArgument =
      record->getValueAsBit("defaultGetterHasExplicitContextArgument");

  FmtContext fmt;
  fmt.addSubst("_type", m_name);
  {
    raw_string_ostream evaluate(m_evaluate);
    evaluate << tgfmt("$_type::get(", &fmt);

    bool haveArg = false;
    if (m_defaultGetterHasExplicitContextArgument) {
      evaluate << "$_context";
      haveArg = true;
    }

    for (const auto &argument : typeArguments()) {
      if (haveArg)
        evaluate << ", ";
      evaluate << "$" << argument.name;
      haveArg = true;
    }

    evaluate << ')';
  }
  if (m_structBacked) {
    m_check = tgfmt("::llvm::isa<::llvm::StructType>($$self)", &fmt);
  } else {
    m_check = tgfmt("::llvm::isa<$_type>($$self)", &fmt);
  }

  if (!m_arguments[0].type || !m_arguments[0].type->isTypeArg() ||
      m_arguments[0].constraint) {
    errs << "self argument of DialectType must be of type `type` without"
         << " constraint\n";
    return false;
  }

  std::vector<std::string> getterArgNames;
  for (size_t i = 1; i < m_arguments.size(); ++i) {
    auto &argument = m_arguments[i];
    m_capture.push_back(
        tgfmt("::llvm::cast<$_type>($$self)->get$0()", &fmt,
              convertToCamelFromSnakeCase(argument.name, true)));

    getterArgNames.push_back(m_symbols.chooseName(
        convertToCamelFromSnakeCase(argument.name, false)));

    auto variable = m_scope.getVariable(argument.name);
    if (argument.constraint) {
      if (!m_system.addConstraint(errs, argument.constraint, variable))
        return false;
    }
  }

  m_context = m_symbols.chooseName({"ctx", "context"});

  if (m_defaultGetterHasExplicitContextArgument) {
    GetterArg arg;
    arg.cppType = "::llvm::LLVMContext &";
    arg.name = m_context;
    m_getterArguments.push_back(std::move(arg));
  } else {
    std::string typeArgument;
    for (auto [argument, getterName] :
         llvm::zip(typeArguments(), getterArgNames)) {
      if (argument.type->isTypeArg()) {
        typeArgument = getterName;
        break;
      }
    }
    if (typeArgument.empty()) {
      errs << "DialectType " << m_name
           << " without type argument needs an "
              "explicit context argument\n";
      errs << "Hint: Set defaultGetterHasExplicitContextArgument to true.\n";
      return false;
    }

    raw_string_ostream prelude(m_prelude);
    FmtContext fmt;
    fmt.withContext(m_context);

    prelude << tgfmt("::llvm::LLVMContext &$_context = $0->getContext();\n",
                     &fmt, typeArgument);
  }

  m_argBegin = m_getterArguments.size();
  for (auto [argument, getterName] :
       llvm::zip(typeArguments(), getterArgNames)) {
    GetterArg arg;
    arg.cppType = argument.type->getCppType();
    arg.name = getterName;
    m_getterArguments.push_back(std::move(arg));
  }

  return true;
}

void DialectType::emitDeclaration(raw_ostream &out, GenDialect *dialect) const {
  FmtContext fmt;
  fmt.withContext(m_context);
  fmt.addSubst("dialect", dialect->name);
  fmt.addSubst("_type", getName());
  fmt.addSubst("mnemonic", getMnemonic());

  if (m_structBacked) {
    out << tgfmt(R"(
    class $_type : public ::llvm::StructType {
      using ::llvm::StructType::StructType;
    public:
      static constexpr ::llvm::StringLiteral s_prefix{"$dialect.$mnemonic."};

      using ::llvm::StructType::getElementType;

      static $_type *get(
  )",
                 &fmt);

    bool contextPresent =
        !m_getterArguments.empty() && m_getterArguments.front().cppType.find(
                                          "LLVMContext") != std::string::npos;
    if (!contextPresent) {
      out << "::llvm::LLVMContext &" << m_context;
      if (!m_getterArguments.empty())
        out << ", ";
    }
    for (const auto &argument : llvm::enumerate(m_getterArguments)) {
      if (argument.index() != 0)
        out << ", ";
      out << argument.value().cppType << ' ' << argument.value().name;
    }
    out << ");\n\n";

    out << "      static bool classof(const ::llvm::Type *t);\n\n";

    unsigned fieldIdx = 1; // sentinel
    for (const auto &argument : typeArguments()) {
      std::string camel = convertToCamelFromSnakeCase(argument.name, true);
      out << tgfmt(
          R"(      unsigned get$0() const {
        ::llvm::Type *elt = getElementType($1);
        if (elt->isStructTy())
          return 0;
        return ::llvm::cast<::llvm::IntegerType>(elt)->getBitWidth();
      }
)",
          &fmt, camel, fieldIdx++);
    }

    out << "    };\n\n";
  } else {

    // TargetExtType

    out << tgfmt(R"(
    class $_type : public ::llvm::TargetExtType {
      static constexpr ::llvm::StringLiteral s_name{"$dialect.$mnemonic"};

    public:
      static bool classof(const ::llvm::TargetExtType *t) {
        return t->getName() == s_name;
      }
      static bool classof(const ::llvm::Type* t) {
        return llvm::isa<::llvm::TargetExtType>(t) &&
                classof(llvm::cast<::llvm::TargetExtType>(t));
      }

      bool verifier(::llvm::raw_ostream &errs) const;

  )",
                 &fmt);

    out << tgfmt("static $_type *get(", &fmt);
    for (const auto &argument : llvm::enumerate(m_getterArguments)) {
      if (argument.index() != 0)
        out << ", ";
      out << argument.value().cppType << ' ' << argument.value().name;
    }
    out << ");\n\n";

    for (const auto &argument : typeArguments()) {
      out << tgfmt("$0 get$1() const;\n", &fmt, argument.type->getCppType(),
                   convertToCamelFromSnakeCase(argument.name, true));
    }

    out << "};\n\n";
  }
}

void DialectType::emitDefinition(raw_ostream &out, GenDialect *dialect) const {
  SymbolTable symbols(&m_symbols);
  FmtContext fmt;
  fmt.withContext(m_context);
  fmt.addSubst("_type", getName());
  fmt.addSubst("mnemonic", getMnemonic());
  fmt.addSubst("type", symbols.chooseName("type"));
  fmt.addSubst("types", symbols.chooseName("types"));
  fmt.addSubst("ints", symbols.chooseName("ints"));
  fmt.addSubst("_errs", symbols.chooseName("errs"));

  if (m_structBacked) {
    out << tgfmt("$_type* $_type::get(", &fmt);
    bool contextPresent =
        !m_getterArguments.empty() && m_getterArguments.front().cppType.find(
                                          "LLVMContext") != std::string::npos;
    if (!contextPresent) {
      out << "::llvm::LLVMContext &" << m_context;
      if (!m_getterArguments.empty())
        out << ", ";
    }
    for (auto argument : llvm::enumerate(m_getterArguments)) {
      if (argument.index() != 0)
        out << ", ";
      out << argument.value().cppType << ' ' << argument.value().name;
    }
    out << ") {\n";

    auto getterArgs =
        ArrayRef<GetterArg>(m_getterArguments).drop_front(m_argBegin);

    for (const auto &[argument, getterArg] :
         llvm::zip(typeArguments(), getterArgs)) {
      if (auto *attr = dyn_cast<Attr>(argument.type)) {
        out << tgfmt(attr->getCheck(), &fmt, getterArg.name) << '\n';
      }
    }

    out << "  std::string __name; ::llvm::raw_string_ostream __os(__name);\n";
    out << tgfmt("  __os << \"$0\";\n", &fmt, m_structPrefix);
    for (const auto &getterArg : getterArgs)
      out << "  __os << (uint64_t)" << getterArg.name << " << '.';\n";

    out << tgfmt("  ::std::vector<::llvm::Type*> __fields;\n", &fmt);
    out << tgfmt(
        "  __fields.push_back(::llvm::IntegerType::get($_context, $0));\n",
        &fmt, Twine(m_structSentinelBitWidth));

    for (const auto &getterArg : getterArgs) {
      out << tgfmt(R"(
  if ($0 == 0)
    __fields.push_back(::llvm::StructType::get($_context));
  else
    __fields.push_back(::llvm::IntegerType::get($_context, $0));
)",
                   &fmt, getterArg.name);
    }
    out << tgfmt("  auto *__st = ::llvm::StructType::create($_context, "
                 "__fields, __os.str(), /*isPacked=*/false);\n",
                 &fmt);
    out << tgfmt("  return static_cast<$_type *>(__st);\n}\n\n", &fmt);

    out << tgfmt(R"(
bool $_type::classof(const ::llvm::Type *t) {
  auto *st = ::llvm::dyn_cast<::llvm::StructType>(t);
  if (!st)
    return false;
  return st->getNumElements() &&
         st->getElementType(0)->isIntegerTy($0);
}
)",
                 &fmt, Twine(m_structSentinelBitWidth));
  } else {
    // TargetExtType

    // Output the type argument getters.
    unsigned typeIdx = 0;
    unsigned intIdx = 0;
    for (const auto &argument : typeArguments()) {
      std::string expr;
      if (argument.type->isTypeArg()) {
        expr = tgfmt("type_params()[$0]", &fmt, typeIdx);
        ++typeIdx;
      } else {
        expr = tgfmt("int_params()[$0]", &fmt, intIdx);
        expr = tgfmt(cast<Attr>(argument.type)->getFromUnsigned(), &fmt, expr);
        ++intIdx;
      }

      FmtContextScope scope{fmt};
      fmt.addSubst("type", argument.type->getCppType());
      fmt.addSubst("name", convertToCamelFromSnakeCase(argument.name, true));
      fmt.addSubst("expr", expr);

      out << tgfmt(R"(
      $type $_type::get$name() const {
        return $expr;
      }

    )",
                   &fmt, expr);
    }

    // Output the default getter.
    out << tgfmt("$_type* $_type::get(", &fmt);
    for (auto argument : llvm::enumerate(m_getterArguments)) {
      if (argument.index() != 0)
        out << ", ";
      out << argument.value().cppType << ' ' << argument.value().name;
    }
    out << ") {\n";

    out << m_prelude;

    auto getterArgs =
        ArrayRef<GetterArg>(m_getterArguments).drop_front(m_argBegin);

    for (const auto &[argument, getterArg] :
         llvm::zip(typeArguments(), getterArgs)) {
      if (auto *attr = dyn_cast<Attr>(argument.type)) {
        out << tgfmt(attr->getCheck(), &fmt, getterArg.name) << '\n';
      }
    }

    out << tgfmt("::std::array<::llvm::Type *, $0> $types = {\n", &fmt,
                 typeIdx);
    for (const auto &[argument, getterArg] :
         llvm::zip(typeArguments(), getterArgs)) {
      if (argument.type->isTypeArg())
        out << getterArg.name << ",\n";
    }
    out << tgfmt(R"(
    };
    ::std::array<unsigned, $0> $ints = {
  )",
                 &fmt, intIdx);
    for (const auto &[argument, getterArg] :
         llvm::zip(typeArguments(), getterArgs)) {
      if (!argument.type->isTypeArg()) {
        std::string expr = tgfmt(cast<Attr>(argument.type)->getToUnsigned(),
                                 &fmt, getterArg.name);
        out << expr << ",\n";
      }
    }

    out << tgfmt(R"(
      };

      auto *$type = ::llvm::cast<$_type>(::llvm::TargetExtType::get($_context, s_name, $types, $ints));
      assert(::llvm_dialects::runTypeVerifier([$type](llvm::raw_ostream &errs) {
        return $type->verifier(errs);
      }, $type));
      return $type;
    }
  )",
                 &fmt);

    // Output the verifier.
    out << tgfmt(R"(
    bool $_type::verifier(::llvm::raw_ostream &$_errs) const {
      ::llvm::LLVMContext &$_context = getContext();
      (void)$_context;

      using ::llvm_dialects::printable;

      if (getNumTypeParameters() != $0) {
        errs << "  wrong number of type parameters\n";
        errs << "    expected: $0\n";
        errs << "      actual: " << getNumTypeParameters() << '\n';
        return false;
      }

      if (getNumIntParameters() != $1) {
        errs << "  wrong number of int parameters\n";
        errs << "    expected: $1\n";
        errs << "      actual: " << getNumIntParameters() << '\n';
        return false;
      }
  )",
                 &fmt, typeIdx, intIdx);

    Assignment assignment;
    Evaluator eval(symbols, assignment, m_system, out, fmt);

    for (const auto &[argument, getterArg] :
         llvm::zip(typeArguments(), getterArgs)) {
      FmtContextScope scope{fmt};
      fmt.addSubst("getter", convertToCamelFromSnakeCase(argument.name, true));
      fmt.addSubst("name", getterArg.name);
      out << tgfmt("auto $name = get$getter();\n(void)$name;\n", &fmt);

      auto variable = m_scope.findVariable(argument.name);
      assignment.assign(variable, fmt.getSubstFor("name").value());
    }

    eval.check(true);

    out << "return true;\n}\n\n";
  }
}
