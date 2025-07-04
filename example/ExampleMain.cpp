/*
 ***********************************************************************************************************************
 * Copyright (c) 2023 Advanced Micro Devices, Inc. All Rights Reserved.
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

#include "ExampleDialect.h"

#include "llvm-dialects/Dialect/Builder.h"
#include "llvm-dialects/Dialect/OpDescription.h"
#include "llvm-dialects/Dialect/OpSet.h"
#include "llvm-dialects/Dialect/Verifier.h"

#include "llvm/AsmParser/Parser.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IRPrinter/IRPrintingPasses.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"

using namespace llvm;
using namespace llvm_dialects;

enum class Action {
  Build,
  Verify,
  Visit,
};

cl::opt<Action> g_action(
    cl::desc("Action to perform:"), cl::init(Action::Build),
    cl::values(clEnumValN(Action::Build, "build", "Example IRBuilder use"),
               clEnumValN(Action::Verify, "verify", "Verify an input module"),
               clEnumValN(Action::Visit, "visit", "Example Visitor use")));

// Input sources
cl::list<std::string> g_inputs(cl::Positional, cl::ZeroOrMore,
                               cl::desc("Input file(s) (\"-\" for stdin)"));

cl::opt<bool> g_rpot("rpot", cl::init(false),
                     cl::desc("Visit functions in reverse post-order"));

/// Test intrinsics that are generic over unnamed struct types.
void useUnnamedStructTypes(Builder &b) {
  StructType *t1 = StructType::create({b.getInt32Ty()}, "");
  StructType *t2 = StructType::create({b.getInt32Ty(), b.getInt32Ty()}, "");
  StructType *t3 = StructType::create({b.getInt64Ty()}, "");

  Value *z1 = b.create<xd::cpp::ReadOp>(t1);
  Value *z2 = b.create<xd::cpp::ReadOp>(t2);
  Value *z3 = b.create<xd::cpp::ReadOp>(t3);
  b.create<xd::cpp::WriteOp>(z1);
  b.create<xd::cpp::WriteOp>(z2);
  b.create<xd::cpp::WriteOp>(z3);
}

void createFunctionExample(Module &module, const Twine &name) {
  Builder b{module.getContext()};

  Function *fn = Function::Create(FunctionType::get(b.getVoidTy(), false),
                                  GlobalValue::ExternalLinkage, name, module);

  BasicBlock *bb = BasicBlock::Create(module.getContext(), "entry", fn);
  b.SetInsertPoint(bb);

  Value *x1 = b.create<xd::cpp::ReadOp>(b.getInt32Ty());
  Value *sizeOf = b.create<xd::cpp::SizeOfOp>(b.getHalfTy());
  Value *sizeOf32 = b.create<xd::cpp::ITruncOp>(b.getInt32Ty(), sizeOf);
  Value *x2 = b.create<xd::cpp::Add32Op>(x1, sizeOf32, 11);
  Value *x3 = b.create<xd::cpp::CombineOp>(x2, x1);
  Value *x4 = b.create<xd::cpp::IExtOp>(b.getInt64Ty(), x3);
  b.create<xd::cpp::WriteOp>(x4);

  cast<xd::cpp::SizeOfOp>(sizeOf)->setSizeofType(b.getDoubleTy());
  cast<xd::cpp::Add32Op>(x2)->setExtra(7);

  Value *q1 = b.create<xd::cpp::ReadOp>(FixedVectorType::get(b.getInt32Ty(), 2));
  Value *q2 = b.create<xd::cpp::FromFixedVectorOp>(
      xd::cpp::XdVectorType::get(xd::cpp::VectorKind::BigEndian, b.getInt32Ty(), 2), q1);

  Value *y1 = b.create<xd::cpp::ReadOp>(
      xd::cpp::XdVectorType::get(xd::cpp::VectorKind::BigEndian, b.getInt32Ty(), 4));
  Value *y2 = b.create<xd::cpp::ExtractElementOp>(y1, x1);
  Value *y3 = b.create<xd::cpp::ExtractElementOp>(y1, b.getInt32(2));
  Value *y4 = b.CreateAdd(y2, y3);
  Value *y5 = b.create<xd::cpp::InsertElementOp>(q2, y4, x1);
  auto *y6 = b.create<xd::cpp::InsertElementOp>(y5, y2, b.getInt32(5));
  b.create<xd::cpp::WriteOp>(y6);

  y6->setIndex(b.getInt32(1));

  Value *p1 = b.create<xd::cpp::ReadOp>(b.getPtrTy());
  p1->setName("p1");
  Value *p2 = b.create<xd::cpp::StreamAddOp>(p1, b.getInt64(14), b.getInt8(0));
  p2->setName("p2");
  b.create<xd::cpp::WriteOp>(p2);

  assert(xd::cpp::ExampleDialect::isDialectOp(*cast<CallInst>(p2)));

  SmallVector<Value *> varArgs;
  varArgs.push_back(p1);
  varArgs.push_back(p2);
  b.create<xd::cpp::WriteVarArgOp>(p2, varArgs);
  b.create<xd::cpp::HandleGetOp>();

  auto *replaceable = b.create<xd::cpp::WriteVarArgOp>(p2, varArgs);
  SmallVector<Metadata *, 1> MD;
  MD.push_back(ConstantAsMetadata::get(
      ConstantInt::get(Type::getInt32Ty(bb->getContext()), 1)));
  replaceable->setMetadata("testMd", MDNode::get(bb->getContext(), MD));

  SmallVector<Value *> varArgs2 = varArgs;
  varArgs2.push_back(p2);

  replaceable->replaceArgs(varArgs2);
  b.create<xd::cpp::SetReadOp>(FixedVectorType::get(b.getInt32Ty(), 2));
  b.create<xd::cpp::SetWriteOp>(y6);

  useUnnamedStructTypes(b);

  b.create<xd::cpp::HandleGetOp>("name.of.llvm.value");
  b.create<xd::cpp::InstNameConflictOp>(b.getInt32(1));
  b.create<xd::cpp::InstNameConflictOp>(b.getInt32(1), "name.foo");
  b.create<xd::cpp::InstNameConflictDoubleOp>(b.getInt32(1), b.getInt32(2));
  b.create<xd::cpp::InstNameConflictDoubleOp>(b.getInt32(1), b.getInt32(2), "bar");
  SmallVector<Value *> moreVarArgs = varArgs;
  b.create<xd::cpp::InstNameConflictVarargsOp>(moreVarArgs);
  b.create<xd::cpp::InstNameConflictVarargsOp>(moreVarArgs, "two.varargs");
  moreVarArgs.push_back(b.getInt32(3));
  b.create<xd::cpp::InstNameConflictVarargsOp>(moreVarArgs, "three.varargs");
  moreVarArgs.push_back(b.getInt32(4));
  b.create<xd::cpp::InstNameConflictVarargsOp>(moreVarArgs, "four.varargs");

  b.create<xd::cpp::StringAttrOp>("Hello world!");

  b.CreateRetVoid();
}

std::unique_ptr<Module> createModuleExample(LLVMContext &context) {
  auto module = std::make_unique<Module>("example", context);
  createFunctionExample(*module, "example");
  return module;
}

struct VisitorInnermost {
  int counter = 0;
};

struct VisitorNest {
  raw_ostream *out = nullptr;
  VisitorInnermost inner;

  void visitBinaryOperator(BinaryOperator &inst) {
    *out << "visiting BinaryOperator: " << inst << '\n';
  }
  VisitorResult visitUnaryInstruction(UnaryInstruction &inst) {
    *out << "visiting UnaryInstruction (pre): " << inst << '\n';
    return isa<LoadInst>(inst) ? VisitorResult::Stop : VisitorResult::Continue;
  }
};

struct VisitorContainer {
  int padding;
  VisitorNest nest;
};

template <>
struct llvm_dialects::VisitorPayloadProjection<VisitorNest, raw_ostream> {
  static raw_ostream &project(VisitorNest &nest) { return *nest.out; }
};

LLVM_DIALECTS_VISITOR_PAYLOAD_PROJECT_FIELD(VisitorContainer, nest)
LLVM_DIALECTS_VISITOR_PAYLOAD_PROJECT_FIELD(VisitorNest, inner)

// Get a visitor with RPOT or default strategy.
//
// Using a template in this way is a bit convoluted, but the point here is to
// demonstrate the pattern of constructing a visitor as a function-local
// static variable whose initialization doesn't refer to any runtime values,
// i.e. if C++ had a strong enough compile-time evaluation (constexpr), it
// should be possible to evaluate the initialization entirely at compile-time.
template <bool rpot> const Visitor<VisitorContainer> &getExampleVisitor() {
  static const auto complexSet = OpSet::fromOpDescriptions(
      {OpDescription::fromCoreOp(Instruction::Ret),
       OpDescription::fromIntrinsic(Intrinsic::umin)});

  static const auto visitor =
      VisitorBuilder<VisitorContainer>()
          .nest<VisitorNest>([](VisitorBuilder<VisitorNest> &b) {
            b.add<xd::cpp::ReadOp>([](VisitorNest &self, xd::cpp::ReadOp &op) {
              *self.out << "visiting ReadOp: " << op << '\n';
            });
            b.add(&VisitorNest::visitUnaryInstruction);
            b.add<xd::cpp::SetReadOp>([](VisitorNest &self, xd::cpp::SetReadOp &op) {
              *self.out << "visiting SetReadOp: " << op << '\n';
              return op.getType()->isIntegerTy(1) ? VisitorResult::Stop
                                                  : VisitorResult::Continue;
            });
            b.addSet<xd::cpp::SetReadOp, xd::cpp::SetWriteOp>(
                [](VisitorNest &self, llvm::Instruction &op) {
                  if (isa<xd::cpp::SetReadOp>(op)) {
                    *self.out << "visiting SetReadOp (set): " << op << '\n';
                  } else if (isa<xd::cpp::SetWriteOp>(op)) {
                    *self.out << "visiting SetWriteOp (set): " << op << '\n';
                  }
                });
            b.addSet(complexSet, [](VisitorNest &self, llvm::Instruction &op) {
              assert((op.getOpcode() == Instruction::Ret ||
                      (isa<IntrinsicInst>(&op) &&
                          cast<IntrinsicInst>(&op)->getIntrinsicID() ==
                              Intrinsic::umin)) &&
                     "Unexpected operation detected while visiting OpSet!");

              if (op.getOpcode() == Instruction::Ret) {
                *self.out << "visiting Ret (set): " << op << '\n';
              } else if (auto *II = dyn_cast<IntrinsicInst>(&op)) {
                if (II->getIntrinsicID() == Intrinsic::umin) {
                  *self.out << "visiting umin (set): " << op << '\n';
                }
              }
            });
            b.add<UnaryInstruction>(
                [](VisitorNest &self, UnaryInstruction &inst) {
                  *self.out << "visiting UnaryInstruction: " << inst << '\n';
                });
            b.add(&VisitorNest::visitBinaryOperator);
            b.nest<raw_ostream>([](VisitorBuilder<raw_ostream> &b) {
              b.add<xd::cpp::WriteOp>([](raw_ostream &out, xd::cpp::WriteOp &op) {
                out << "visiting WriteOp: " << op << '\n';
              });
              b.add<xd::cpp::WriteVarArgOp>(
                  [](raw_ostream &out, xd::cpp::WriteVarArgOp &op) {
                    out << "visiting WriteVarArgOp: " << op << ":\n";
                    for (Value *arg : op.getArgs())
                      out << "  " << *arg << '\n';
                  });
              b.add<xd::cpp::StringAttrOp>(
                  [](raw_ostream &out, xd::cpp::StringAttrOp &op) {
                    out << "visiting StringAttrOp: " << op.getVal() << '\n';
                  });
              b.add<ReturnInst>([](raw_ostream &out, ReturnInst &ret) {
                out << "visiting ReturnInst: " << ret << '\n';
              });
              b.add<CallInst>([](raw_ostream &out, CallInst &ret) {
                out << "visiting CallInst: " << ret << '\n';
              });
              b.add<CallBrInst>([](raw_ostream &out, CallBrInst &ret) {
                out << "visiting CallBrInst: " << ret << '\n';
              });
              b.addIntrinsic(
                  Intrinsic::umax, [](raw_ostream &out, IntrinsicInst &umax) {
                    out << "visiting umax intrinsic: " << umax << '\n';
                  });
            });
            b.nest<VisitorInnermost>([](VisitorBuilder<VisitorInnermost> &b) {
              b.add<xd::cpp::ITruncOp>([](VisitorInnermost &inner,
                                     xd::cpp::ITruncOp &op) { inner.counter++; });
            });
          })
          .setStrategy(rpot ? VisitorStrategy::ReversePostOrder
                            : VisitorStrategy::Default)
          .build();
  return visitor;
}

void exampleVisit(Module &module) {
  const auto &visitor =
      g_rpot ? getExampleVisitor<true>() : getExampleVisitor<false>();

  VisitorContainer container;
  container.nest.out = &outs();
  visitor.visit(container, module);

  outs() << "inner.counter = " << container.nest.inner.counter << '\n';
}

int main(int argc, char **argv) {
  llvm::cl::ParseCommandLineOptions(argc, argv);

  LLVMContext context;
  auto dialectContext = DialectContext::make<xd::cpp::ExampleDialect>(context);

  if (g_action == Action::Build) {
    auto module = createModuleExample(context);
    module->print(llvm::outs(), nullptr, false);
  } else {
    if (g_inputs.size() != 1) {
      errs() << "Need exactly one input module\n";
      return 1;
    }

    auto fileOrErr = MemoryBuffer::getFileOrSTDIN(g_inputs[0]);
    if (std::error_code errorCode = fileOrErr.getError()) {
      errs() << "Could not open input file " << g_inputs[0] << ": "
             << errorCode.message() << '\n';
      return 1;
    }

    SMDiagnostic error;
    std::unique_ptr<Module> module = parseAssembly(**fileOrErr, error, context);
    if (!module) {
      error.print("llvm-dialects-example", errs());
      errs() << "\n";
      return 1;
    }

    if (g_action == Action::Verify) {
      if (!verify(*module, outs()))
        return 1;
    } else if (g_action == Action::Visit) {
      exampleVisit(*module);
    } else {
      report_fatal_error("unhandled action");
    }
  }

  return 0;
}
