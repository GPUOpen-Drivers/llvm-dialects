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

#include <memory>

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"

namespace llvm {
class DagInit;
class Record;
class RecordKeeper;
} // namespace llvm

namespace llvm_dialects {

class BuiltinType;
class Constraint;
class DialectType;
class OpClass;
struct OpNamedValue;
class Operation;
class PredicateExpr;
class Trait;

class GenDialect {
public:
  llvm::Record *record;
  std::string cppName;
  std::string name;
  std::string cppNamespace;
  std::vector<DialectType *> types;
  std::vector<OpClass *> opClasses;
  std::vector<std::unique_ptr<Operation>> operations;

public:
  void finalize();

  llvm::ArrayRef<std::vector<Trait *>> attribute_lists() const {
    return m_attributeLists;
  }
  bool attribute_lists_empty() const { return m_attributeLists.empty(); }
  size_t attribute_lists_size() const { return m_attributeLists.size(); }

private:
  std::vector<std::vector<Trait *>> m_attributeLists;
};

class GenDialectsContext {
public:
  void init(llvm::RecordKeeper &records,
            const llvm::DenseSet<llvm::StringRef> &dialects);

  Trait *getTrait(llvm::Record *traitRec);
  Constraint *getConstraint(llvm::Record *constraintRec);
  OpClass *getOpClass(llvm::Record *opClassRec);
  GenDialect *getDialect(llvm::Record *dialectRec);

  BuiltinType *getVoidTy() const { return m_voidTy; }

  std::unique_ptr<PredicateExpr> parsePredicateExpr(llvm::DagInit *dag);

private:
  std::vector<OpNamedValue> parseArguments(llvm::Record *rec);

  BuiltinType *m_voidTy = nullptr;
  llvm::DenseMap<llvm::Record *, std::unique_ptr<Trait>> m_traits;
  llvm::DenseMap<llvm::Record *, std::unique_ptr<Constraint>> m_constraints;
  llvm::DenseMap<llvm::Record *, std::unique_ptr<OpClass>> m_opClasses;
  llvm::DenseMap<llvm::Record *, std::unique_ptr<GenDialect>> m_dialects;
};

} // namespace llvm_dialects
