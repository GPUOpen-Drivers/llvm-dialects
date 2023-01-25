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
#include "llvm-dialects/TableGen/Operations.h"
#include "llvm-dialects/TableGen/Predicates.h"
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

/// Helper class for choosing unique variable names.
class SymbolTable {
  std::unordered_set<std::string> m_names;

public:
  std::string chooseName(StringRef name) { return chooseName(ArrayRef(name)); }

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
    )", &fmt, op.superclass ? op.superclass->name : "::llvm::CallInst",
    op.overloadKeys.empty() ? "isSimpleOperation" : "isOverloadedOperation");

    SymbolTable symbols;
    SmallVector<OpNamedValue> fullArguments = op.getFullArguments();
    SmallVector<std::string> argNames;
    SmallVector<std::string> resultNames;

    if (op.builderHasExplicitResultTypes) {
      for (const auto& result : op.results) {
        resultNames.push_back(symbols.chooseName(
            convertToCamelFromSnakeCase(result.name, false) + "Type"));
      }
    }

    for (const auto& arg : fullArguments)
      argNames.push_back(symbols.chooseName(convertToCamelFromSnakeCase(arg.name, false)));

    fmt.withBuilder(symbols.chooseName({"b", "builder"}));

    out << tgfmt("static ::llvm::Value* create(::llvm_dialects::Builder& $_builder", &fmt);
    for (const auto& resultName : resultNames)
      out << tgfmt(", ::llvm::Type* $0", &fmt, resultName);
    for (const auto& [argName, arg] : llvm::zip_first(argNames, fullArguments))
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
  auto [GenDialectsContext, dialect] = getSelectedDialect(records);

  emitHeader(out);

  out << R"(
#ifdef GET_INCLUDES
#undef GET_INCLUDES
#include "llvm-dialects/Dialect/Builder.h"
#include "llvm-dialects/Dialect/OpDescription.h"
#include "llvm-dialects/Dialect/Utils.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/Support/ModRef.h"
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
      const OpNamedValue& arg = indexedArg.value();
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

    // Emit create() method definition.
    SmallVector<OpNamedValue> fullArguments = op.getFullArguments();
    SmallVector<std::string> resultNames;
    SmallVector<std::string> argNames;
    SymbolTable symbols;

    if (op.builderHasExplicitResultTypes) {
      for (const auto& result : op.results) {
        resultNames.push_back(symbols.chooseName(
            convertToCamelFromSnakeCase(result.name, false) + "Type"));
      }
    }

    for (const auto& arg : fullArguments) {
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
    for (const auto& resultName : resultNames)
      out << tgfmt(", ::llvm::Type* $0", &fmt, resultName);
    for (const auto& [argName, arg] : llvm::zip_first(argNames, fullArguments)) {
      out << tgfmt(", $0 $1", &fmt, arg.type->getCppType(), argName);
    }

    out << tgfmt(R"() {
      ::llvm::LLVMContext& $_context = $_builder.getContext();
      ::llvm::Module& $_module = *$_builder.GetInsertBlock()->getModule();
    
    )", &fmt);

    // Map TableGen names of arguments to C++ expressions to be used by
    // predicates.
    DenseMap<StringRef, std::string> argToCppExprMap;
    for (const auto &[arg, argName] : llvm::zip(fullArguments, argNames)) {
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
      FmtContextScope scope{fmt};
      fmt.addSubst("attrBuilder", symbols.chooseName("attrBuilder"));

      out << tgfmt("::llvm::AttrBuilder $attrBuilder{$_context};\n", &fmt);
      for (const Trait *trait : op.traits) {
        if (auto *llvmAttribute = dyn_cast<LlvmAttributeTrait>(trait)) {
          llvmAttribute->addAttribute(out, fmt);
        } else {
          llvm_unreachable("unsupported trait kind");
        }
      }

      out << tgfmt("const auto $0 = ::llvm::AttributeList::get($_context, "
                   "::llvm::AttributeList::FunctionIndex, $attrBuilder);\n",
                   &fmt, attrs);
    }

    LlvmTypeBuilder typeBuilder{out, symbols, fmt};
    SmallVector<std::string> argTypes;
    for (const auto& [arg, argName] : llvm::zip(fullArguments, argNames)) {
      if (isa<Attr>(arg.type))
        argTypes.push_back(typeBuilder.build(arg.type));
      else
        argTypes.push_back(argName + "->getType()");
    }

    std::string resultTypeName;
    if (op.builderHasExplicitResultTypes) {
      assert(resultNames.size() == 1);
      resultTypeName = resultNames[0];
    } else {
      assert(op.results.size() <= 1);
      Constraint *theResultType = op.results.empty()
                                      ? GenDialectsContext.getVoidTy()
                                      : op.results[0].type;
      resultTypeName = typeBuilder.build(theResultType);
    }

    StringRef fnName;

    if (!op.overloadKeys.empty()) {
      out << tgfmt("std::string $0 = ::llvm_dialects::getMangledName(s_name, {\n", &fmt,
                   mangledName);
      for (const auto &key : op.overloadKeys) {
        if (key.kind == OverloadKey::Argument) {
          out << argNames[key.index] << "->getType(),\n";
        } else {
          assert(key.kind == OverloadKey::Result);
          out << resultNames[key.index] << ",\n";
        }
      }
      out << "});\n";

      fnName = mangledName;
    } else {
      fnName = "s_name";
    }

    out << tgfmt("\nauto $0 = $_module.getOrInsertFunction($1, $2, $3",
                 &fmt, fn, fnName, attrs, resultTypeName);
    for (const auto& argType : argTypes) {
      out << ", " << argType;
    }
    out << ");\n\n";

    for (const auto& [name, arg] : llvm::zip_first(argNames, fullArguments)) {
      if (auto* type = dyn_cast<Type>(arg.type)) {
        StringRef filter = type->getBuilderArgumentFilter();
        if (!filter.empty()) {
          FmtContextScope scope{fmt};
          fmt.withSelf(name);
          out << tgfmt(filter, &fmt);
        }
      }
    }

    if (!argNames.empty()) {
      out << tgfmt("::llvm::Value* const $0[] = {\n", &fmt, args);
      for (const auto& [name, type, arg]
               : llvm::zip_first(argNames, argTypes, fullArguments)) {
        if (auto* attr = dyn_cast<Attr>(arg.type)) {
          out << tgfmt(attr->getToLlvmValue(), &fmt, name, type);
        } else {
          out << name;
        }
        out << ",\n";
      }
      out << "};\n\n";

      out << tgfmt("return $_builder.CreateCall($0, $1);\n", &fmt, fn, args);
    } else
      out << tgfmt("return $_builder.CreateCall($0);\n", &fmt, fn);
    out << "}\n\n";

    // Emit argument getters.
    unsigned numSuperclassArgs = 0;
    if (op.superclass)
      numSuperclassArgs = op.superclass->getNumFullArguments();
    for (auto indexedArg : llvm::enumerate(op.arguments)) {
      const OpNamedValue& arg = indexedArg.value();
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

    )", &fmt, op.overloadKeys.empty() ? "false" : "true");
  }

  out << R"(
#endif // GET_DIALECT_DEFS
)";
}
