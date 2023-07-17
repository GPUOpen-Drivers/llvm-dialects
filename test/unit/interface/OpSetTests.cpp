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
#include "llvm-dialects/Dialect/OpDescription.h"
#include "llvm-dialects/Dialect/OpSet.h"
#include "llvm/IR/Intrinsics.h"
#include "gtest/gtest.h"

using namespace llvm;
using namespace llvm_dialects;

[[maybe_unused]] constexpr const char DialectsOpSetSizeTestsName[] =
    "DialectsOpSetSizeTests";

[[maybe_unused]] constexpr const char DialectsOpSetContainsTestsName[] =
    "DialectsOpSetContainsTests";

#define EXPECT_EQ_SIZE(Expression, Value)                                      \
  EXPECT_EQ(static_cast<int>(Expression), Value)

TEST(DialectsOpSetSizeTestsName, EmptyTest) {
  const OpSet set;
  EXPECT_TRUE(set.getCoreOpcodes().empty());
  EXPECT_TRUE(set.getIntrinsicIDs().empty());
  EXPECT_TRUE(set.getDialectOps().empty());
}

TEST(DialectsOpSetSizeTestsName, NonEmptyCoreOpcodesTest) {
  const OpSet set = OpSet::fromCoreOpcodes({1, 2});
  EXPECT_EQ_SIZE(set.getCoreOpcodes().size(), 2);
}

TEST(DialectsOpSetSizeTestsName, NonEmptyIntrinsicsTest) {
  const OpSet set = OpSet::fromIntrinsicIDs(
      {Intrinsic::lifetime_start, Intrinsic::lifetime_end});
  EXPECT_EQ_SIZE(set.getIntrinsicIDs().size(), 2);
}

TEST(DialectsOpSetSizeTestsName, NonEmptyOpDescriptionsTest) {
  OpDescription const desc1 = OpDescription::get<test::DialectOp1>();
  OpDescription const desc2 = OpDescription::get<test::DialectOp2>();
  const OpSet set = OpSet::fromOpDescriptions({desc1, desc2});
  EXPECT_EQ_SIZE(set.getDialectOps().size(), 2);
}

TEST(DialectsOpSetSizeTestsName, NonEmptyOpDescriptionsTemplatizedMakerTest) {
  const OpSet set = OpSet::get<test::DialectOp1, test::DialectOp2>();
  EXPECT_EQ_SIZE(set.getDialectOps().size(), 2);
}

TEST(DialectsOpSetContainsTestsName, containsCoreOps) {
  const OpSet set = OpSet::fromCoreOpcodes({1, 2}); // Ret, Br
  EXPECT_TRUE(set.containsCoreOp(Instruction::Ret));
  EXPECT_TRUE(set.containsCoreOp(Instruction::Br));
  EXPECT_FALSE(set.containsCoreOp(Instruction::Switch));
}

TEST(DialectsOpSetContainsTestsName, ContainsIntrinsicIDs) {
  const OpSet set = OpSet::fromIntrinsicIDs({Intrinsic::sqrt, Intrinsic::fabs});
  EXPECT_TRUE(set.containsIntrinsicID(Intrinsic::sqrt));
  EXPECT_TRUE(set.containsIntrinsicID(Intrinsic::fabs));
  EXPECT_FALSE(set.containsIntrinsicID(Intrinsic::floor));
}

TEST(DialectsOpSetContainsTestsName, contains) {
  const OpSet set = OpSet::get<test::DialectOp1, test::DialectOp2>();
  EXPECT_TRUE(set.contains<test::DialectOp1>());
  EXPECT_TRUE(set.contains<test::DialectOp2>());
  EXPECT_TRUE(set.contains<test::DialectOp1>());
  EXPECT_TRUE(set.contains<test::DialectOp2>());
  EXPECT_FALSE(set.contains<test::DialectOp3>());
}

TEST(DialectsOpSetSizeTestsName,
     StoreDuplicateOpDescriptionsOnceTemplatizedMaker) {
  const OpSet set = OpSet::get<test::DialectOp1, test::DialectOp1>();
  EXPECT_EQ_SIZE(set.getDialectOps().size(), 1);
  EXPECT_TRUE(set.contains<test::DialectOp1>());
  EXPECT_FALSE(set.contains<test::DialectOp2>());
}

TEST(DialectsOpSetSizeTestsName, StoreDuplicateOpDescriptionsOnce) {
  const OpDescription desc1 = OpDescription::get<test::DialectOp1>();
  const OpDescription desc2 = OpDescription::get<test::DialectOp1>();
  OpSet set = OpSet::fromOpDescriptions({desc1, desc2});
  EXPECT_EQ_SIZE(set.getDialectOps().size(), 1);
  EXPECT_TRUE(set.contains<test::DialectOp1>());
  EXPECT_FALSE(set.contains<test::DialectOp2>());
}

#undef EXPECT_EQ_SIZE
