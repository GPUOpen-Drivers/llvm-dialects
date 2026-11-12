/*
 ***********************************************************************************************************************
 * Copyright (c) Advanced Micro Devices, Inc., or its affiliates.
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

#include "TestDialect.h"
#include "llvm-dialects/Dialect/Builder.h"
#include "llvm-dialects/Dialect/Dialect.h"
#include "llvm-dialects/Dialect/Visitor.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include "gtest/gtest.h"

#include <memory>

using namespace llvm;
using namespace llvm_dialects;

class VisitorIRTestFixture : public testing::Test {
protected:
  void SetUp() override {
    setupDialectsContext();
    makeModule();
  }

  LLVMContext Context;
  std::unique_ptr<DialectContext> DC;
  std::unique_ptr<Module> Mod;
  Function *EP = nullptr;

  BasicBlock *getEntryBlock() { return EntryBlock; }

private:
  BasicBlock *EntryBlock = nullptr;

  void makeModule() {
    Mod = std::make_unique<Module>("dialects_test", Context);
    const std::array<Type *, 1> Args = {Type::getInt32Ty(Mod->getContext())};
    FunctionCallee FC = Mod->getOrInsertFunction(
        "main",
        FunctionType::get(Type::getVoidTy(Mod->getContext()), Args, false));
    EP = cast<Function>(FC.getCallee());
    EntryBlock = BasicBlock::Create(Mod->getContext(), "entry", EP);
  }

  void setupDialectsContext() {
    DC = DialectContext::make<test::TestDialect>(Context);
  }
};

TEST_F(VisitorIRTestFixture, VisitOp) {
  llvm_dialects::Builder Builder{Context};
  Builder.SetInsertPoint(getEntryBlock());

  auto *Mul1 = Builder.create<test::MulOp>();
  auto *Mul2 = Builder.create<test::MulOp>();
  auto *Add1 = Builder.create<test::AddOp>();
  auto *Add2 = Builder.create<test::AddOp>();
  auto *DialectOp1 = Builder.create<test::DialectOp1>();

  DenseSet<test::MulOp *> Ops;
  static const auto Visitor =
      llvm_dialects::VisitorBuilder<DenseSet<test::MulOp *>>()
          .add<test::MulOp>([](auto &Ops, test::MulOp &Op) { Ops.insert(&Op); })
          .build();
  Visitor.visit(Ops, *Mod);
  EXPECT_EQ(Ops.size(), 2);
  for (const auto *Op : Ops) {
    EXPECT_TRUE(Op == Mul1 || Op == Mul2);
  }
}

TEST_F(VisitorIRTestFixture, VisitOpClass) {
  llvm_dialects::Builder Builder{Context};
  Builder.SetInsertPoint(getEntryBlock());

  auto *Mul1 = Builder.create<test::MulOp>();
  auto *Mul2 = Builder.create<test::MulOp>();
  auto *Add1 = Builder.create<test::AddOp>();
  auto *Add2 = Builder.create<test::AddOp>();
  auto *DialectOp1 = Builder.create<test::DialectOp1>();

  DenseSet<test::SomeBaseOpClass *> Ops;
  static const auto Visitor =
      llvm_dialects::VisitorBuilder<DenseSet<test::SomeBaseOpClass *>>()
          .add<test::SomeBaseOpClass>(
              [](auto &Ops, test::SomeBaseOpClass &Op) { Ops.insert(&Op); })
          .build();
  Visitor.visit(Ops, *Mod);
  EXPECT_EQ(Ops.size(), 4);
  for (const auto *Op : Ops) {
    EXPECT_TRUE(Op == Mul1 || Op == Mul2 || Op == Add1 || Op == Add2);
  }
}

// Visiting a nested op class must cover the ops of the nested class itself.
TEST_F(VisitorIRTestFixture, VisitNestedOpClass) {
  llvm_dialects::Builder Builder{Context};
  Builder.SetInsertPoint(getEntryBlock());

  Builder.create<test::MulOp>();
  Builder.create<test::AddOp>();
  auto *Sub1 = Builder.create<test::SubOp>();
  auto *Div1 = Builder.create<test::DivOp>();
  Builder.create<test::DialectOp1>();

  DenseSet<test::SomeDerivedOpClass *> Ops;
  static const auto Visitor =
      llvm_dialects::VisitorBuilder<DenseSet<test::SomeDerivedOpClass *>>()
          .add<test::SomeDerivedOpClass>(
              [](auto &Ops, test::SomeDerivedOpClass &Op) { Ops.insert(&Op); })
          .build();
  Visitor.visit(Ops, *Mod);
  EXPECT_EQ(Ops.size(), 2);
  for (const auto *Op : Ops) {
    EXPECT_TRUE(Op == Sub1 || Op == Div1);
  }
}

// Visiting an op class must cover ops that are members of a nested op class, and not just
// the ops that are direct members of the visited class.
TEST_F(VisitorIRTestFixture, VisitOpClassWithNestedOpClass) {
  llvm_dialects::Builder Builder{Context};
  Builder.SetInsertPoint(getEntryBlock());

  auto *Mul1 = Builder.create<test::MulOp>();
  auto *Add1 = Builder.create<test::AddOp>();
  auto *Sub1 = Builder.create<test::SubOp>();
  auto *Div1 = Builder.create<test::DivOp>();
  Builder.create<test::DialectOp1>();

  // All four ops are members of SomeBaseOpClass as far as isa<> is concerned.
  EXPECT_TRUE(isa<test::SomeBaseOpClass>(Mul1));
  EXPECT_TRUE(isa<test::SomeBaseOpClass>(Add1));
  EXPECT_TRUE(isa<test::SomeBaseOpClass>(Sub1));
  EXPECT_TRUE(isa<test::SomeBaseOpClass>(Div1));

  DenseSet<test::SomeBaseOpClass *> Ops;
  static const auto Visitor =
      llvm_dialects::VisitorBuilder<DenseSet<test::SomeBaseOpClass *>>()
          .add<test::SomeBaseOpClass>(
              [](auto &Ops, test::SomeBaseOpClass &Op) { Ops.insert(&Op); })
          .build();
  Visitor.visit(Ops, *Mod);

  EXPECT_EQ(Ops.size(), 4);
  for (const auto *Op : Ops) {
    EXPECT_TRUE(Op == Mul1 || Op == Add1 || Op == Sub1 || Op == Div1);
  }
}

// isa<> must see through the full op class hierarchy, independently of the visitor.
TEST_F(VisitorIRTestFixture, NestedOpClassIsa) {
  llvm_dialects::Builder Builder{Context};
  Builder.SetInsertPoint(getEntryBlock());

  auto *Mul1 = Builder.create<test::MulOp>();
  auto *Sub1 = Builder.create<test::SubOp>();
  auto *DialectOp1 = Builder.create<test::DialectOp1>();

  // An op of the nested class is a member of both the nested and the base class.
  EXPECT_TRUE(isa<test::SomeDerivedOpClass>(Sub1));
  EXPECT_TRUE(isa<test::SomeBaseOpClass>(Sub1));

  // An op that is a direct member of the base class is not a member of the nested class.
  EXPECT_FALSE(isa<test::SomeDerivedOpClass>(Mul1));
  EXPECT_TRUE(isa<test::SomeBaseOpClass>(Mul1));

  // An unrelated op is a member of neither.
  EXPECT_FALSE(isa<test::SomeDerivedOpClass>(DialectOp1));
  EXPECT_FALSE(isa<test::SomeBaseOpClass>(DialectOp1));
}
