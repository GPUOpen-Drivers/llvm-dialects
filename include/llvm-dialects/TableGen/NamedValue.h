/*
 * Copyright (c) 2022-2023 Advanced Micro Devices, Inc. All Rights Reserved.
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

#include <optional>
#include <string>
#include <vector>

namespace llvm {
class DagInit;
class Init;
class raw_ostream;
} // namespace llvm

namespace llvm_dialects {

class Constraint;
class GenDialectsContext;
class MetaType;

struct NamedValue {
  /// The meta-type of the value, if known.
  MetaType *type = nullptr;

  /// The name of the value.
  std::string name;

  /// The implied constraint on the value.
  llvm::Init *constraint = nullptr;

  enum class Parser {
    ApplyArguments,
    PredicateArguments,
    DialectTypeArguments,
    OperationArguments,
    OperationResults,
  };

  static std::optional<std::vector<NamedValue>>
  parseList(llvm::raw_ostream &errs, GenDialectsContext &context,
            llvm::DagInit *init, unsigned begin, Parser mode);

  std::string toString() const;
};

} // namespace llvm_dialects
