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
#include "llvm-dialects/Dialect/OpMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/Intrinsics.h"
#include "gtest/gtest.h"

#include <string>

using namespace llvm;
using namespace llvm_dialects;

[[maybe_unused]] constexpr const char OpMapBasicTestsName[] = "OpMapBasicTests";

TEST(OpMapBasicTestsName, CoreOpContainsTests) {
  OpMap<StringRef> map;

  OpDescription retDesc = OpDescription::fromCoreOp(Instruction::Ret);
  OpDescription brDesc = OpDescription::fromCoreOp(Instruction::Br);
  map[retDesc] = "RetInst";

  EXPECT_TRUE(map.containsCoreOp(Instruction::Ret));
  EXPECT_FALSE(map.containsCoreOp(Instruction::Br));
  EXPECT_EQ(map[retDesc], "RetInst");

  map[brDesc] = "BrInst";
  EXPECT_EQ(map[retDesc], "RetInst");
  EXPECT_TRUE(map.containsCoreOp(Instruction::Br));
  EXPECT_EQ(map[brDesc], "BrInst");
}

TEST(OpMapBasicTestsName, IntrinsicOpContainsTests) {
  OpMap<StringRef> map;

  OpDescription memCpyDesc = OpDescription::fromIntrinsic(Intrinsic::memcpy);
  OpDescription memMoveDesc = OpDescription::fromIntrinsic(Intrinsic::memmove);
  map[memCpyDesc] = "MemCpy";

  EXPECT_TRUE(map.containsIntrinsic(Intrinsic::memcpy));
  EXPECT_FALSE(map.containsIntrinsic(Intrinsic::memmove));
  EXPECT_EQ(map[memCpyDesc], "MemCpy");

  map[memMoveDesc] = "MemMove";
  EXPECT_EQ(map[memMoveDesc], "MemMove");
  EXPECT_TRUE(map.containsIntrinsic(Intrinsic::memmove));
  EXPECT_EQ(map[memMoveDesc], "MemMove");
}

TEST(OpMapBasicTestsName, DialectOpContainsTests) {
  OpMap<StringRef> map;
  const OpDescription sampleDesc = OpDescription::get<test::DialectOp1>();

  map[OpDescription::get<test::DialectOp1>()] = "Hello";

  EXPECT_TRUE(map.contains<test::DialectOp1>());
  EXPECT_TRUE(map.contains(sampleDesc));
  EXPECT_FALSE(map.contains<test::DialectOp2>());
  EXPECT_EQ(map[OpDescription::get<test::DialectOp1>()], "Hello");

  map[OpDescription::get<test::DialectOp2>()] = "World";
  map[OpDescription::get<test::DialectOp1>()] = "DialectOp1";

  EXPECT_TRUE(map.contains<test::DialectOp2>());
  EXPECT_EQ(map[OpDescription::get<test::DialectOp1>()], "DialectOp1");
  EXPECT_EQ(map[OpDescription::get<test::DialectOp2>()], "World");

  map[OpDescription::get<test::DialectOp3>()] = "DialectOp3";
  EXPECT_TRUE(map.contains<test::DialectOp3>());
  EXPECT_EQ(map[OpDescription::get<test::DialectOp3>()], "DialectOp3");
}

TEST(OpMapBasicTestsName, OpMapLookupTests) {
  OpMap<StringRef> map;
  map.insert<test::DialectOp1>("Hello");
  map.insert<test::DialectOp2>("World");
  map.insert<test::DialectOp3>("DO3");

  EXPECT_EQ(static_cast<int>(map.size()), 3);
  EXPECT_EQ(map[OpDescription::get<test::DialectOp1>()], "Hello");
  EXPECT_EQ(map[OpDescription::get<test::DialectOp2>()], "World");
  EXPECT_EQ(map[OpDescription::get<test::DialectOp3>()], "DO3");
  map[OpDescription::get<test::DialectOp3>()] = "DO3_Override";
  EXPECT_EQ(map[OpDescription::get<test::DialectOp3>()], "DO3_Override");
  map.erase<test::DialectOp3>();
  EXPECT_EQ(static_cast<int>(map.size()), 2);
  EXPECT_FALSE(map.contains<test::DialectOp3>());
}

TEST(OpMapBasicTestsName, OpMapInitializerTests) {
  OpMap<StringRef> map = {
      {{OpDescription::get<test::DialectOp1>(), "Hello"},
       {OpDescription::get<test::DialectOp2>(), "World"},
       {OpDescription::get<test::DialectOp3>(), "DO3"},
       {OpDescription::fromCoreOp(Instruction::Ret), "Ret"},
       {OpDescription::fromIntrinsic(Intrinsic::assume), "Assume"}}};

  EXPECT_TRUE(map.contains<test::DialectOp1>());
  EXPECT_TRUE(map.contains<test::DialectOp2>());
  EXPECT_TRUE(map.contains<test::DialectOp3>());
  EXPECT_TRUE(map.contains(OpDescription::fromCoreOp(Instruction::Ret)));
  EXPECT_TRUE(map.contains(OpDescription::fromIntrinsic(Intrinsic::assume)));

  EXPECT_EQ(map[OpDescription::get<test::DialectOp1>()], "Hello");
  EXPECT_EQ(map[OpDescription::get<test::DialectOp2>()], "World");
  EXPECT_EQ(map[OpDescription::get<test::DialectOp3>()], "DO3");
  EXPECT_EQ(map[OpDescription::fromCoreOp(Instruction::Ret)], "Ret");
  EXPECT_EQ(map[OpDescription::fromIntrinsic(Intrinsic::assume)], "Assume");

  map[OpDescription::get<test::DialectOp1>()] = "DO1";
  EXPECT_EQ(map[OpDescription::get<test::DialectOp1>()], "DO1");

  map[OpDescription::fromCoreOp(Instruction::Ret)] = "RetInst";
  EXPECT_EQ(map[OpDescription::fromCoreOp(Instruction::Ret)], "RetInst");
}

TEST(OpMapBasicTestsName, OpMapEqualityTests) {
  OpMap<StringRef> map = {{{OpDescription::get<test::DialectOp1>(), "Hello"},
                           {OpDescription::get<test::DialectOp2>(), "World"},
                           {OpDescription::get<test::DialectOp3>(), "DO3"}}};

  OpMap<StringRef> map2 = {{{OpDescription::get<test::DialectOp1>(), "Hello"},
                            {OpDescription::get<test::DialectOp2>(), "World"},
                            {OpDescription::get<test::DialectOp3>(), "DO3"}}};

  EXPECT_EQ(map, map2);

  map[OpDescription::get<test::DialectOp1>()] = "DO1";

  EXPECT_NE(map, map2);
}

TEST(OpMapBasicTestsName, OpMapEqualityOrderingTests) {
  OpMap<StringRef> map = {{{OpDescription::get<test::DialectOp1>(), "Hello"},
                           {OpDescription::get<test::DialectOp3>(), "DO3"},
                           {OpDescription::get<test::DialectOp2>(), "World"}}};

  OpMap<StringRef> map2 = {{{OpDescription::get<test::DialectOp1>(), "Hello"},
                            {OpDescription::get<test::DialectOp2>(), "World"},
                            {OpDescription::get<test::DialectOp3>(), "DO3"}}};

  EXPECT_EQ(map, map2);
}

TEST(OpMapBasicTestsName, OpMapEqualityEraseTests) {
  OpMap<StringRef> map = {{{OpDescription::get<test::DialectOp1>(), "Hello"},
                           {OpDescription::get<test::DialectOp2>(), "World"},
                           {OpDescription::get<test::DialectOp3>(), "DO3"}}};

  OpMap<StringRef> map2 = {{{OpDescription::get<test::DialectOp1>(), "Hello"},
                            {OpDescription::get<test::DialectOp2>(), "World"},
                            {OpDescription::get<test::DialectOp3>(), "DO3"}}};

  EXPECT_EQ(map, map2);

  map.erase<test::DialectOp1>();

  EXPECT_NE(map, map2);
}

TEST(OpMapBasicTestsName, OpMapCopyTests) {
  OpMap<StringRef> map = {{{OpDescription::get<test::DialectOp1>(), "Hello"},
                           {OpDescription::get<test::DialectOp2>(), "World"},
                           {OpDescription::get<test::DialectOp3>(), "DO3"}}};

  OpMap<StringRef> map2 = map;

  (void)map2;
  EXPECT_EQ(map, map2);
}

TEST(OpMapBasicTestsName, OpMapMoveTests) {
  OpMap<StringRef> map = {{{OpDescription::get<test::DialectOp1>(), "Hello"},
                           {OpDescription::get<test::DialectOp2>(), "World"},
                           {OpDescription::get<test::DialectOp3>(), "DO3"}}};

  OpMap<StringRef> map2 = std::move(map);

  EXPECT_TRUE(map.empty());
  EXPECT_NE(map, map2);
}

TEST(OpMapBasicTestsName, OpMapIteratorBaseTests) {
  OpMap<StringRef> map = {
      {{OpDescription::get<test::DialectOp1>(), "Hello"},
       {OpDescription::get<test::DialectOp2>(), "World"},
       {OpDescription::fromIntrinsic(Intrinsic::fabs), "fabs"},
       {OpDescription::get<test::DialectOp3>(), "DO3"}}};

  EXPECT_EQ(*map.find(OpDescription::get<test::DialectOp1>()).val(), "Hello");
  EXPECT_EQ(*map.find(OpDescription::get<test::DialectOp2>()).val(), "World");
  EXPECT_EQ(*map.find(OpDescription::fromIntrinsic(Intrinsic::fabs)).val(),
            "fabs");
  EXPECT_EQ(*map.find(OpDescription::get<test::DialectOp3>()).val(), "DO3");
}

TEST(OpMapBasicTestsName, OpMapIteratorIncTests) {
  OpMap<StringRef> map;

  const OpDescription desc1 = OpDescription::get<test::DialectOp1>();
  const OpDescription desc2 = OpDescription::get<test::DialectOp2>();
  const OpDescription desc3 = OpDescription::fromIntrinsic(Intrinsic::fabs);
  const OpDescription desc4 = OpDescription::get<test::DialectOp3>();
  const OpDescription desc5 = OpDescription::fromCoreOp(Instruction::FAdd);

  map[desc1] = "DialectOp1";
  map[desc2] = "DialectOp2";
  map[desc3] = "Fabs";
  map[desc4] = "DialectOp3";
  map[desc5] = "FAdd";

  size_t Idx = 0;
  for (auto it = map.begin(); Idx < 5 && it != map.end(); ++it) {
    switch (Idx) {
    case 0:
      EXPECT_EQ((*it).second, "FAdd");
      break;
    case 1:
      EXPECT_EQ((*it).second, "Fabs");
      break;
    case 2:
      EXPECT_EQ((*it).second, "DialectOp1");
      break;
    case 3:
      EXPECT_EQ((*it).second, "DialectOp2");
      break;
    case 4:
      EXPECT_EQ((*it).second, "DialectOp3");
      break;
    }

    ++Idx;
  }
}