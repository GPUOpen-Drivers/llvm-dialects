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
      Intrinsic::getOrInsertDeclaration(Mod.get(), Intrinsic::sideeffect));

  const std::array<Value *, 1> AssumeArgs = {
      ConstantInt::getBool(Type::getInt1Ty(Context), true)};
  const auto &Assume = *B.CreateCall(
      Intrinsic::getOrInsertDeclaration(Mod.get(), Intrinsic::assume),
      AssumeArgs);

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
      Intrinsic::getOrInsertDeclaration(Mod.get(), Intrinsic::sideeffect));

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

TEST_F(OpMapIRTestFixture, CallCoreOpMatchesInstructionTest) {
  OpMap<StringRef> map;
  llvm_dialects::Builder B{Context};

  // Define types
  PointerType *PtrTy = B.getPtrTy();
  IntegerType *I32Ty = Type::getInt32Ty(Context);

  // Declare: ptr @ProcOpaqueHandle(i32, ptr)
  FunctionType *ProcOpaqueHandleFuncTy =
      FunctionType::get(PtrTy, {I32Ty, PtrTy}, false);
  FunctionCallee ProcOpaqueHandleFunc =
      Mod->getOrInsertFunction("ProcOpaqueHandle", ProcOpaqueHandleFuncTy);

  B.SetInsertPoint(getEntryBlock());

  // Declare %OpaqueTy = type opaque
  StructType *OpaqueTy = StructType::create(Context, "OpaqueTy");

  // Create a dummy global variable of type %OpaqueTy*
  GlobalVariable *GV = new GlobalVariable(
      *Mod, OpaqueTy, false, GlobalValue::PrivateLinkage, nullptr, "handle");
  GV->setInitializer(ConstantAggregateZero::get(OpaqueTy));
  Value *Op2 = GV;

  // Create a constant value (e.g., 123)
  Value *Op1 = B.getInt32(123);

  // Build a call instruction
  Value *Args[] = {Op1, Op2};
  const CallInst &Call = *B.CreateCall(ProcOpaqueHandleFunc, Args);

  // Create basic blocks for the function  
  auto *FC = getEntryBlock()->getParent();
  BasicBlock *Label1BB = BasicBlock::Create(Context, "label1", FC);  
  BasicBlock *Label2BB = BasicBlock::Create(Context, "label2", FC);  
  BasicBlock *ContinueBB = BasicBlock::Create(Context, "continue", FC);  
  
  // Simulate a function that can branch to multiple labels  
  // For demonstration purposes, we'll create a placeholder function that represents this behavior  
  FunctionType *BranchFuncTy = FunctionType::get(Type::getVoidTy(Context), false);  
  FunctionCallee BranchFunc = Mod->getOrInsertFunction("Branch", BranchFuncTy);
  
  // Create the CallBr instruction  
  const CallBrInst &CallBr = *B.CreateCallBr(BranchFunc, ContinueBB, {Label1BB, Label2BB}); 

  // Load and test OpMap with Call and CallBr

  // Add Instruction::Call to OpMap
  const OpDescription CallDesc = OpDescription::fromCoreOp(Instruction::Call);
  map[CallDesc] = "Call";

  // Add Instruction::CallBr to OpMap
  const OpDescription CallBrDesc = OpDescription::fromCoreOp(Instruction::CallBr);
  map[CallBrDesc] = "CallBr";

  // Look up the Call and CallBr in the map and verify it finds the entries for
  // Instruction::Call and Instruction::CallBr
  EXPECT_EQ(map.lookup(Call), "Call");
  EXPECT_EQ(map.lookup(CallBr), "CallBr");
}
