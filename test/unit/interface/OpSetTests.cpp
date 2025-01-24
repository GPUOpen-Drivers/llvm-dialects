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
