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
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "gtest/gtest.h"

using namespace llvm;
using namespace test;

// Test custom llvmTypeNameOverride for TargetExtType
TEST(DialectTypeTest, CustomNameTargetExtType) {
  LLVMContext ctx;

  auto *type = CustomNameTargetExtType::get(ctx, 10, 20);
  ASSERT_NE(type, nullptr);

  // Verify it's recognized as the correct type via classof
  EXPECT_TRUE(isa<CustomNameTargetExtType>(type));
  EXPECT_TRUE(CustomNameTargetExtType::classof(type));

  // Verify the type name is the custom one, not the default
  auto *targetExtType = cast<TargetExtType>(type);
  EXPECT_EQ(targetExtType->getName(), "custom.renamed.target");

  // Verify parameters are accessible
  EXPECT_EQ(type->getParam1(), 10u);
  EXPECT_EQ(type->getParam2(), 20u);
}

// Test custom llvmTypeNameOverride for struct-backed type
TEST(DialectTypeTest, CustomNameStructType) {
  LLVMContext ctx;

  auto *type = CustomNameStructType::get(ctx, 16, 16);
  ASSERT_NE(type, nullptr);

  // Verify it's recognized as the correct type via classof
  EXPECT_TRUE(isa<CustomNameStructType>(type));
  EXPECT_TRUE(CustomNameStructType::classof(type));

  // Verify the struct type name has the custom prefix
  auto *structType = cast<StructType>(type);
  EXPECT_TRUE(structType->getName().starts_with("custom.renamed.struct."));

  // Verify parameters are accessible
  EXPECT_EQ(type->getRows(), 16u);
  EXPECT_EQ(type->getCols(), 16u);
}

// Test default naming behavior (no llvmTypeNameOverride specified)
TEST(DialectTypeTest, DefaultNameTargetExtType) {
  LLVMContext ctx;

  auto *type = DefaultNameTargetExtType::get(ctx, 42);
  ASSERT_NE(type, nullptr);

  // Verify it's recognized as the correct type
  EXPECT_TRUE(isa<DefaultNameTargetExtType>(type));
  EXPECT_TRUE(DefaultNameTargetExtType::classof(type));

  // Verify the type name uses the default dialect.mnemonic format
  auto *targetExtType = cast<TargetExtType>(type);
  EXPECT_EQ(targetExtType->getName(), "test.default.target");

  // Verify parameter is accessible
  EXPECT_EQ(type->getValue(), 42u);
}

// Test that different custom types are distinguishable
TEST(DialectTypeTest, TypeDistinction) {
  LLVMContext ctx;

  auto *customTarget = CustomNameTargetExtType::get(ctx, 1, 2);
  auto *defaultTarget = DefaultNameTargetExtType::get(ctx, 3);
  auto *customStruct = CustomNameStructType::get(ctx, 4, 5);

  // Types should be distinct
  EXPECT_NE(static_cast<llvm::Type *>(customTarget),
            static_cast<llvm::Type *>(defaultTarget));
  EXPECT_NE(static_cast<llvm::Type *>(customTarget),
            static_cast<llvm::Type *>(customStruct));
  EXPECT_NE(static_cast<llvm::Type *>(defaultTarget),
            static_cast<llvm::Type *>(customStruct));

  // classof should work correctly
  EXPECT_TRUE(CustomNameTargetExtType::classof(customTarget));
  EXPECT_FALSE(CustomNameTargetExtType::classof(defaultTarget));
  EXPECT_FALSE(CustomNameTargetExtType::classof(customStruct));

  EXPECT_FALSE(DefaultNameTargetExtType::classof(customTarget));
  EXPECT_TRUE(DefaultNameTargetExtType::classof(defaultTarget));
  EXPECT_FALSE(DefaultNameTargetExtType::classof(customStruct));

  EXPECT_FALSE(CustomNameStructType::classof(customTarget));
  EXPECT_FALSE(CustomNameStructType::classof(defaultTarget));
  EXPECT_TRUE(CustomNameStructType::classof(customStruct));
}
