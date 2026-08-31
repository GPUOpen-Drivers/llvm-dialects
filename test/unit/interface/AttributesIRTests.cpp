/*
 ***********************************************************************************************************************
 * Copyright (c) 2026 Advanced Micro Devices, Inc. All Rights Reserved.
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
#include "llvm/IR/Attributes.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include "gtest/gtest.h"

#include <memory>
#include <optional>

using namespace llvm;
using namespace llvm_dialects;

class AttributesIRTestFixture : public testing::Test {
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
    FunctionCallee FC = Mod->getOrInsertFunction(
        "main", FunctionType::get(Type::getVoidTy(Mod->getContext()), false));
    EP = cast<Function>(FC.getCallee());
    EntryBlock = BasicBlock::Create(Mod->getContext(), "entry", EP);
  }

  void setupDialectsContext() {
    DC = DialectContext::make<test::TestDialect>(Context);
  }
};

TEST_F(AttributesIRTestFixture, AllocSizeAndAllocKindAttributes) {
  Builder B{Context};
  B.SetInsertPoint(getEntryBlock());

  auto *Op = B.create<test::AllocatingOp>(B.getInt64(42));
  Function *Fn = Op->getCalledFunction();
  ASSERT_NE(Fn, nullptr);

  ASSERT_TRUE(Fn->hasFnAttribute(Attribute::AllocSize));
  EXPECT_EQ(Fn->getFnAttribute(Attribute::AllocSize),
            Attribute::getWithAllocSizeArgs(Context, 0, std::nullopt));
  EXPECT_EQ(Fn->getFnAttribute(Attribute::AllocSize).getAllocSizeArgs(),
            std::make_pair(0u, std::optional<unsigned>{}));

  ASSERT_TRUE(Fn->hasFnAttribute(Attribute::AllocKind));
  EXPECT_EQ(Fn->getFnAttribute(Attribute::AllocKind).getAllocKind(),
            AllocFnKind::Alloc | AllocFnKind::Uninitialized);

  B.CreateRetVoid();
  EXPECT_FALSE(verifyModule(*Mod, &errs()));
}

TEST_F(AttributesIRTestFixture, TwoArgAllocSizeAttribute) {
  Builder B{Context};
  B.SetInsertPoint(getEntryBlock());

  auto *Op = B.create<test::AllocatingArrayOp>(B.getInt32(13), B.getInt32(8));
  Function *Fn = Op->getCalledFunction();
  ASSERT_NE(Fn, nullptr);

  ASSERT_TRUE(Fn->hasFnAttribute(Attribute::AllocSize));
  EXPECT_EQ(Fn->getFnAttribute(Attribute::AllocSize),
            Attribute::getWithAllocSizeArgs(Context, 1, 0));
  EXPECT_EQ(Fn->getFnAttribute(Attribute::AllocSize).getAllocSizeArgs(),
            std::make_pair(1u, std::optional<unsigned>(0u)));

  ASSERT_TRUE(Fn->hasFnAttribute(Attribute::AllocKind));
  EXPECT_EQ(Fn->getFnAttribute(Attribute::AllocKind).getAllocKind(),
            AllocFnKind::Alloc | AllocFnKind::Zeroed);

  B.CreateRetVoid();
  EXPECT_FALSE(verifyModule(*Mod, &errs()));
}
