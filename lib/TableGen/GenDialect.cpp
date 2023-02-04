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

#include "llvm-dialects/TableGen/GenDialect.h"
#include "llvm-dialects/TableGen/Common.h"
#include "llvm-dialects/TableGen/Constraints.h"
#include "llvm-dialects/TableGen/Dialects.h"
#include "llvm-dialects/TableGen/Format.h"
#include "llvm-dialects/TableGen/LlvmTypeBuilder.h"
#include "llvm-dialects/TableGen/Operations.h"
#include "llvm-dialects/TableGen/Predicates.h"
#include "llvm-dialects/TableGen/SymbolTable.h"
#include "llvm-dialects/TableGen/Traits.h"

#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/TableGen/Record.h"

#include <unordered_set>

using namespace llvm_dialects;
using namespace llvm;

namespace {

cl::opt<std::string> g_dialect("dialect", cl::desc("the dialect to generate"), cl::init(""));

} // anonymous namespace

static std::pair<GenDialectsContext, GenDialect *>
getSelectedDialect(RecordKeeper &records) {
  if (g_dialect.empty())
    report_fatal_error(Twine("Must select a dialect using the --dialect option"));

  GenDialectsContext context;
  DenseSet<StringRef> dialects;
  dialects.insert(g_dialect);

  context.init(records, dialects);

  for (Record* dialectRec : records.getAllDerivedDefinitions("Dialect")) {
    if (dialectRec->getValueAsString("name") == g_dialect) {
      GenDialect *selectedDialect = context.getDialect(dialectRec);
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

namespace llvm {
class raw_ostream;
} // namespace llvm

namespace llvm_dialects {
class Builder;
} // namespace llvm_dialects
#endif // GET_INCLUDES

#ifdef GET_DIALECT_DECLS
#undef GET_DIALECT_DECLS

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

    private:
      $Dialect(::llvm::LLVMContext& context);

      static ::llvm_dialects::Dialect* make(::llvm::LLVMContext& context);
  )",
               &fmt);

  if (!dialect->attribute_lists_empty()) {
    out << tgfmt(R"(
      public:
        ::llvm::AttributeList getAttributeList(size_t index) const {
          return m_attributeLists[index];
        }

      private:
        ::std::array<::llvm::AttributeList, $0> m_attributeLists;
    )",
                 &fmt, dialect->attribute_lists_size());
  }

  out << "};\n";

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

  // Operation class class declarations
  for (OpClass* opClass : dialect->opClasses) {
    FmtContextScope scope{fmt};
    fmt.withOp(opClass->name);

    out << tgfmt(R"(
      class $_op : public $0 {
      public:
        static bool classof(const ::llvm::CallInst* i);
        static bool classof(const ::llvm::Value* v) {
          return ::llvm::isa<::llvm::CallInst>(v) &&
                 classof(::llvm::cast<::llvm::CallInst>(v));
        }
    )", &fmt, opClass->superclass ? opClass->superclass->name : "::llvm::CallInst");

    for (const auto& arg : opClass->arguments) {
      out << tgfmt("$0 get$1();\n", &fmt, arg.type->getCppType(),
                   convertToCamelFromSnakeCase(arg.name, true));
    }

    out << R"(
      };
    )";

  }

  // Operation class declarations
  for (const auto& opPtr : dialect->operations) {
    const Operation& op = *opPtr;
    FmtContextScope scope{fmt};
    fmt.withOp(op.name);
    fmt.addSubst("mnemonic", op.mnemonic);

    out << tgfmt(R"(
      class $_op : public $0 {
        static const ::llvm::StringLiteral s_name; //{"$dialect.$mnemonic"};

      public:
        static bool classof(const ::llvm::CallInst* i) {
          return ::llvm_dialects::detail::$1(i, s_name);
        }
        static bool classof(const ::llvm::Value* v) {
          return ::llvm::isa<::llvm::CallInst>(v) &&
                 classof(::llvm::cast<::llvm::CallInst>(v));
        }
    )",
                 &fmt, op.superclass ? op.superclass->name : "::llvm::CallInst",
                 !op.haveResultOverloads() ? "isSimpleOperation"
                                           : "isOverloadedOperation");

    for (const auto &builder : op.builders())
      builder.emitDeclaration(out, fmt);
    out << '\n';

    out << "bool verifier(::llvm::raw_ostream &errs);\n\n";

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

static void emitVerifierMethod(raw_ostream &out, FmtContext &fmt,
                               GenDialectsContext &genContext,
                               const Operation &op) {
  FmtContextScope scope{fmt};
  fmt.withContext("context");

  out << tgfmt(R"(
    bool $_op::verifier(::llvm::raw_ostream &errs) {
      ::llvm::LLVMContext &$_context = getModule()->getContext();
      (void)$_context;

      if (arg_size() != $0) {
        errs << "  wrong number of arguments: " << arg_size()
              << ", expected $0\n";
        return false;
      }
  )", &fmt, op.getNumFullArguments());

  SmallVector<NamedValue> argsAndResults = op.getFullArguments();
  argsAndResults.append(op.results.begin(), op.results.end());

  DenseMap<StringRef, std::string> argToCppExprMap;
  for (const auto &arg : argsAndResults) {
    std::string cppExpr =
        tgfmt("get$0()", &fmt, convertToCamelFromSnakeCase(arg.name, true));
    if (!isa<Attr>(arg.type) && !isa<TypeArg>(arg.type))
      cppExpr += "->getType()";

    argToCppExprMap[arg.name] = cppExpr;

    std::string check = arg.type->apply(&fmt, {cppExpr});
    if (check != "true") {
      out << tgfmt(R"(
        if (!($0)) {
          errs << "  op arg verifier failed: $0\n";
          return false;
        }
      )", &fmt, check);
    }
  }

  for (const auto &expr : op.verifier) {
    std::string check = expr->evaluate(&fmt, argToCppExprMap);
    if (check != "true") {
      out << tgfmt(R"(
        if (!($0)) {
          errs << "  op verifier failed: $0\n";
          return false;
        }
      )", &fmt, check);
    }
  }

  out << "  return true;\n}\n\n";
}

void llvm_dialects::genDialectDefs(raw_ostream& out, RecordKeeper& records) {
  auto [genDialectsContext, dialect] = getSelectedDialect(records);

  emitHeader(out);

  out << R"(
#ifdef GET_INCLUDES
#undef GET_INCLUDES
#include "llvm-dialects/Dialect/Builder.h"
#include "llvm-dialects/Dialect/OpDescription.h"
#include "llvm-dialects/Dialect/Utils.h"
#include "llvm/IR/InstrTypes.h"
)";

  if (!noMemoryEffects()) {
    out << R"(
#include "llvm/Support/ModRef.h"
)";
  }

  out << R"(
#include "llvm/Support/raw_ostream.h"
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

    ::llvm_dialects::Dialect* $Dialect::make(::llvm::LLVMContext& context) {
      return new $Dialect(context);
    }

    $Dialect::$Dialect(::llvm::LLVMContext& context) : DialectImpl(context) {
  )",
               &fmt);

  if (!dialect->attribute_lists_empty()) {
    FmtContextScope scope{fmt};
    fmt.addSubst("attrBuilder", "attrBuilder");

    for (const auto &enumeratedTraits : enumerate(dialect->attribute_lists())) {
      out << tgfmt("{\n  ::llvm::AttrBuilder $attrBuilder{context};\n", &fmt);

      for (const Trait *trait : enumeratedTraits.value()) {
        if (auto *llvmAttribute = dyn_cast<LlvmAttributeTrait>(trait)) {
          llvmAttribute->addAttribute(out, fmt);
        } else {
          llvm_unreachable("unsupported trait kind");
        }
      }

      out << tgfmt("m_attributeLists[$0] = ::llvm::AttributeList::get(context, "
                   "::llvm::AttributeList::FunctionIndex, $attrBuilder);\n}\n",
                   &fmt, enumeratedTraits.index());
    }
  }

  out << "}\n\n";

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

  // Operation class class definitions.
  for (const OpClass* opClass : dialect->opClasses) {
    FmtContextScope scope{fmt};
    fmt.withOp(opClass->name);

    // Define the classof method.
    out << tgfmt(R"(
      bool $_op::classof(const ::llvm::CallInst* i) {
    )", &fmt);

    for (OpClass* subclass : opClass->subclasses) {
      out << tgfmt(R"(
        if ($0::classof(i)) return true;
      )", &fmt, subclass->name);
    }

    for (Operation* op : opClass->operations) {
      out << tgfmt(R"(
        if ($0::classof(i)) return true;
      )", &fmt, op->name);
    }

    out << tgfmt(R"(
        return false;
      }

    )", &fmt);

    // Emit argument getters.
    unsigned numSuperclassArgs = 0;
    if (opClass->superclass)
      numSuperclassArgs = opClass->superclass->getNumFullArguments();
    for (auto indexedArg : llvm::enumerate(opClass->arguments)) {
      const NamedValue& arg = indexedArg.value();
      std::string value = llvm::formatv("getArgOperand({0})",
                                        numSuperclassArgs + indexedArg.index());
      if (auto* attr = dyn_cast<Attr>(arg.type))
        value = tgfmt(attr->getFromLlvmValue(), &fmt, value);
      out << tgfmt(R"(
        $0 $_op::get$1() {
          return $2;
        }
      )", &fmt, arg.type->getCppType(), convertToCamelFromSnakeCase(arg.name, true), value);
    }

    out << '\n';
  }

  // Operation class definitions.
  for (const auto& opPtr : dialect->operations) {
    const Operation& op = *opPtr;

    FmtContextScope scope{fmt};
    fmt.withOp(op.name);
    fmt.addSubst("mnemonic", op.mnemonic);

    out << tgfmt(R"(
      const ::llvm::StringLiteral $_op::s_name{"$dialect.$mnemonic"};

    )", &fmt);

    for (const auto &builder : op.builders())
      builder.emitDefinition(out, fmt, genDialectsContext);

    emitVerifierMethod(out, fmt, genDialectsContext, op);

    // Emit argument getters.
    unsigned numSuperclassArgs = 0;
    if (op.superclass)
      numSuperclassArgs = op.superclass->getNumFullArguments();
    for (auto indexedArg : llvm::enumerate(op.arguments)) {
      const NamedValue& arg = indexedArg.value();
      std::string value = llvm::formatv("getArgOperand({0})",
                                        numSuperclassArgs + indexedArg.index());
      if (auto* attr = dyn_cast<Attr>(arg.type))
        value = tgfmt(attr->getFromLlvmValue(), &fmt, value);
      else if (isa<TypeArg>(arg.type))
        value += "->getType()";

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

  // Define specializations of OpDescription::get for reflection
  for (const auto &opPtr : dialect->operations) {
    Operation &op = *opPtr;

    FmtContextScope scope{fmt};
    fmt.withOp(op.name);
    fmt.addSubst("mnemonic", op.mnemonic);

    out << tgfmt(R"(
      template <>
      const ::llvm_dialects::OpDescription &
      ::llvm_dialects::OpDescription::get<$namespace::$_op>() {
        static const ::llvm_dialects::OpDescription desc{$0, "$dialect.$mnemonic"};
        return desc;
      }

    )",
                 &fmt, op.haveResultOverloads() ? "true" : "false");
  }

  out << R"(
#endif // GET_DIALECT_DEFS
)";
}
