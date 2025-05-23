/*
 ***********************************************************************************************************************
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
 ***********************************************************************************************************************
 */

#pragma once

#include <memory>
#include <unordered_map>

#include "llvm-dialects/TableGen/Common.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/StringMap.h"

namespace llvm {
class DagInit;
class Init;
class raw_ostream;
class Record;
class RecordKeeper;
} // namespace llvm

namespace llvm_dialects {

class Attr;
class DialectType;
class OpClass;
class Operation;
class Predicate;
class Trait;

class GenDialect {
public:
  RecordTy *record;
  std::string cppName;
  std::string name;
  std::string cppNamespace;
  std::vector<DialectType *> types;
  std::vector<OpClass *> opClasses;
  llvm::StringMap<unsigned> operationCounts;
  std::vector<std::unique_ptr<Operation>> operations;

public:
  void finalize(llvm::raw_ostream &errs);

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
  GenDialectsContext();
  ~GenDialectsContext();

  void init(RecordKeeperTy &records,
            const llvm::DenseSet<llvm::StringRef> &dialects);

  Trait *getTrait(RecordTy *traitRec, int idx = -1);
  Predicate *getPredicate(const llvm::Init *init, llvm::raw_ostream &errs);
  Attr *getAttr(RecordTy *record, llvm::raw_ostream &errs);
  OpClass *getOpClass(RecordTy *opClassRec);
  GenDialect *getDialect(RecordTy *dialectRec);

  const llvm::Init *getVoidTy() const { return m_voidTy; }
  const llvm::Init *getAny() const { return m_any; }

private:
  GenDialectsContext(const GenDialectsContext &rhs) = delete;
  GenDialectsContext &operator=(const GenDialectsContext &rhs) = delete;
  GenDialectsContext(GenDialectsContext &&rhs) = delete;
  GenDialectsContext &operator=(GenDialectsContext &&rhs) = delete;

  Predicate *getPredicateImpl(const llvm::Init *record,
                              llvm::raw_ostream &errs);

  const llvm::Init *m_voidTy = nullptr;
  const llvm::Init *m_any = nullptr;
  bool m_attrsComplete = false;
  llvm::DenseMap<RecordTy *, std::unique_ptr<Trait>> m_traits;
  llvm::DenseMap<RecordTy *, std::unordered_map<int, std::unique_ptr<Trait>>> m_valueTraits;
  llvm::DenseMap<const llvm::Init *, std::unique_ptr<Predicate>> m_predicates;
  llvm::DenseMap<RecordTy *, std::unique_ptr<Attr>> m_attrs;
  llvm::DenseMap<RecordTy *, std::unique_ptr<OpClass>> m_opClasses;
  llvm::DenseMap<RecordTy *, std::unique_ptr<GenDialect>> m_dialects;
};

} // namespace llvm_dialects
