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

#pragma once

#include "llvm-dialects/TableGen/Predicates.h"

#include "llvm/ADT/SmallVector.h"

namespace llvm_dialects {

class Constraint;
class Operation;
class Trait;

struct OpNamedValue {
  std::string name;
  Constraint *type = nullptr;
};

struct OverloadKey {
  enum Kind {
    Result,
    Argument,
  };

  Kind kind;
  unsigned index;
};

class OpClass {
public:
  OpClass *superclass = nullptr;
  std::string name;

  /// List of arguments specific to this class; does not contain superclass
  /// arguments, if any.
  std::vector<OpNamedValue> arguments;
  std::vector<OpClass *> subclasses;
  std::vector<Operation *> operations;

  llvm::SmallVector<OpNamedValue> getFullArguments() const;
  unsigned getNumFullArguments() const;
};

class Operation {
public:
  OpClass *superclass = nullptr;
  std::string name;
  std::string mnemonic;
  std::vector<Trait *> traits;

  /// List of arguments specific to this operation; does not contain superclass
  /// arguments, if any.
  std::vector<OpNamedValue> arguments;
  std::vector<OpNamedValue> results;
  std::vector<std::unique_ptr<PredicateExpr>> verifier;
  std::vector<OverloadKey> overloadKeys;
  bool builderHasExplicitResultTypes = false;

  llvm::SmallVector<OpNamedValue> getFullArguments() const;
  unsigned getNumFullArguments() const;
};

} // namespace llvm_dialects
