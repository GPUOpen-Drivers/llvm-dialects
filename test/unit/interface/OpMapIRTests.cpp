/*
***********************************************************************************************************************
*
*  Copyright (c) 2023 Advanced Micro Devices, Inc. All Rights Reserved.
*
*  Permission is hereby granted, free of charge, to any person obtaining a copy
*  of this software and associated documentation files (the "Software"), to deal
*  in the Software without restriction, including without limitation the rights
*  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*  copies of the Software, and to permit persons to whom the Software is
*  furnished to do so, subject to the following conditions:
*
*  The above copyright notice and this permission notice shall be included in
*all copies or substantial portions of the Software.
*
*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*  SOFTWARE.
*
**********************************************************************************************************************/

#include "TestDialect.h"
#include "llvm-dialects/Dialect/Builder.h"
#include "llvm-dialects/Dialect/Dialect.h"
#include "llvm-dialects/Dialect/OpMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include "gtest/gtest.h"

#include <memory>

using namespace llvm;
using namespace llvm_dialects;

class OpMapIRTestFixture : public testing::Test {
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

TEST_F(OpMapIRTestFixture, CoreOpMatchesInstructionTest) {
  OpMap<StringRef> map;

  IRBuilder<> Builder{Context};
  Builder.SetInsertPoint(getEntryBlock());

  const OpDescription SubDesc = OpDescription::fromCoreOp(Instruction::Sub);
  map[SubDesc] = "Sub";
  map[OpDescription::fromCoreOp(Instruction::Add)] = "Add";

  Value *Arg = EP->getArg(0);

  Value *Mul = Builder.CreateMul(Arg, Arg);
  Builder.CreateSub(Arg, Mul);

  const Instruction &SubInst = *&getEntryBlock()->back();

  Builder.CreateAdd(Arg, Arg);

  const Instruction &AddInst = *&getEntryBlock()->back();

  EXPECT_FALSE(map.lookup(SubInst) == map.lookup(AddInst));
  EXPECT_EQ(map.lookup(SubInst), "Sub");
  EXPECT_EQ(map.lookup(AddInst), "Add");

  map[SubDesc] = "Sub_Override";
  EXPECT_EQ(map.lookup(SubInst), "Sub_Override");
}

TEST_F(OpMapIRTestFixture, IntrinsicOpMatchesInstructionTest) {
  OpMap<StringRef> map;

  llvm_dialects::Builder B{Context};
  B.SetInsertPoint(getEntryBlock());

  const OpDescription SideEffectDesc =
      OpDescription::fromIntrinsic(Intrinsic::sideeffect);
  const OpDescription AssumeDesc =
      OpDescription::fromIntrinsic(Intrinsic::assume);

  map.insert(SideEffectDesc, "sideeffect");
  map.insert(AssumeDesc, "assume");

  EXPECT_EQ(map[SideEffectDesc], "sideeffect");
  EXPECT_EQ(map[AssumeDesc], "assume");

  const auto &SideEffect = *B.CreateCall(
      Intrinsic::getDeclaration(Mod.get(), Intrinsic::sideeffect));

  const std::array<Value *, 1> AssumeArgs = {
      ConstantInt::getBool(Type::getInt1Ty(Context), true)};
  const auto &Assume = *B.CreateCall(
      Intrinsic::getDeclaration(Mod.get(), Intrinsic::assume), AssumeArgs);

  EXPECT_FALSE(map.lookup(SideEffect) == map.lookup(Assume));
  EXPECT_EQ(map.lookup(SideEffect), "sideeffect");
  EXPECT_EQ(map.lookup(Assume), "assume");

  map[OpDescription::fromIntrinsic(Intrinsic::sideeffect)] =
      "sideeffect_Override";
  EXPECT_EQ(map.lookup(SideEffect), "sideeffect_Override");
}

TEST_F(OpMapIRTestFixture, DialectOpMatchesInstructionTest) {
  OpMap<StringRef> map;

  map.insert<test::DialectOp1>("DialectOp1");
  map.insert<test::DialectOp2>("DialectOp2");

  llvm_dialects::Builder B{Context};
  B.SetInsertPoint(getEntryBlock());

  const Instruction &Op = *B.create<test::DialectOp1>();
  const Instruction &Op2 = *B.create<test::DialectOp2>();

  EXPECT_FALSE(map.lookup(Op) == map.lookup(Op2));
  EXPECT_EQ(map.lookup(Op), "DialectOp1");
  EXPECT_EQ(map.lookup(Op2), "DialectOp2");

  map[OpDescription::get<test::DialectOp1>()] = "DialectOp1_Override";
  EXPECT_EQ(map.lookup(Op), "DialectOp1_Override");
}

TEST_F(OpMapIRTestFixture, MixedOpMatchesInstructionTest) {
  OpMap<StringRef> map;

  llvm_dialects::Builder B{Context};
  B.SetInsertPoint(getEntryBlock());

  const OpDescription SideEffectDesc =
      OpDescription::fromIntrinsic(Intrinsic::sideeffect);

  map.insert(SideEffectDesc, "sideeffect");

  const Instruction &Op1 = *B.create<test::DialectOp2>();
  const Instruction &Op2 = *B.create<test::DialectOp3>();

  EXPECT_EQ(map[SideEffectDesc], "sideeffect");

  const auto &SideEffect = *B.CreateCall(
      Intrinsic::getDeclaration(Mod.get(), Intrinsic::sideeffect));

  EXPECT_EQ(map.lookup(SideEffect), "sideeffect");

  map[OpDescription::get<test::DialectOp2>()] = "DO2";
  map[OpDescription::get<test::DialectOp3>()] = "DO3";

  map[OpDescription::fromIntrinsic(Intrinsic::sideeffect)] =
      "sideeffect_Override";

  EXPECT_EQ(map.lookup(SideEffect), "sideeffect_Override");
  EXPECT_EQ(map.lookup(Op1), "DO2");
  EXPECT_EQ(map.lookup(Op2), "DO3");
}

TEST_F(OpMapIRTestFixture, DialectOpMatchesFunctionTest) {
  OpMap<StringRef> map;

  map.insert<test::DialectOp1>("DialectOp1");
  map.insert<test::DialectOp2>("DialectOp2");

  llvm_dialects::Builder B{Context};
  B.SetInsertPoint(getEntryBlock());

  const auto &Op = *B.create<test::DialectOp1>();
  const auto &Op2 = *B.create<test::DialectOp2>();

  const Function &DO1 = *Op.getCalledFunction();
  const Function &DO2 = *Op2.getCalledFunction();

  EXPECT_FALSE(map.lookup(DO1) == map.lookup(DO2));
  EXPECT_EQ(map.lookup(DO1), "DialectOp1");
  EXPECT_EQ(map.lookup(DO2), "DialectOp2");

  map[OpDescription::get<test::DialectOp1>()] = "DialectOp1_Override";
  EXPECT_EQ(map.lookup(DO1), "DialectOp1_Override");
}

TEST_F(OpMapIRTestFixture, OpMapLookupTests) {
  OpMap<StringRef> map;

  map.insert<test::DialectOp1>("DialectOp1");
  map.insert(OpDescription::fromCoreOp(Instruction::Ret), "RetInst");

  llvm_dialects::Builder B{Context};
  B.SetInsertPoint(getEntryBlock());

  const auto &Op = *B.create<test::DialectOp1>();
  const auto &Ret = *B.CreateRetVoid();

  EXPECT_EQ(map.lookup(Op), "DialectOp1");
  EXPECT_EQ(map.lookup(*Op.getCalledFunction()), "DialectOp1");
  EXPECT_EQ(map.lookup(Op), "DialectOp1");

  EXPECT_EQ(map.lookup(Ret), "RetInst");
}

TEST_F(OpMapIRTestFixture, DialectOpOverloadTests) {
  OpMap<StringRef> map;

  map.insert<test::DialectOp4>("DialectOp4");

  llvm_dialects::Builder B{Context};
  B.SetInsertPoint(getEntryBlock());

  Value *Arg = EP->getArg(0);

  Value *Mul = B.CreateMul(Arg, Arg);
  B.CreateSub(Arg, Mul);

  Value *AddInt = B.CreateAdd(Arg, Arg);
  const auto &Op1 =
      *B.create<test::DialectOp4>(Type::getInt32Ty(Mod->getContext()), AddInt);

  auto *AddFloat = B.CreateBitCast(AddInt, Type::getFloatTy(Mod->getContext()));
  const auto &Op2 = *B.create<test::DialectOp4>(
      Type::getFloatTy(Mod->getContext()), AddFloat);

  EXPECT_EQ(map.lookup(Op1), "DialectOp4");
  EXPECT_EQ(map.lookup(Op2), "DialectOp4");
}
