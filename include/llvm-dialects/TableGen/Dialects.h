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
  llvm::Record *record;
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

  void init(llvm::RecordKeeper &records,
            const llvm::DenseSet<llvm::StringRef> &dialects);

  Trait *getTrait(llvm::Record *traitRec);
  Predicate *getPredicate(llvm::Init *init, llvm::raw_ostream &errs);
  Attr *getAttr(llvm::Record *record, llvm::raw_ostream &errs);
  OpClass *getOpClass(llvm::Record *opClassRec);
  GenDialect *getDialect(llvm::Record *dialectRec);

  llvm::Init *getVoidTy() const { return m_voidTy; }
  llvm::Init *getAny() const { return m_any; }

private:
  GenDialectsContext(const GenDialectsContext &rhs) = delete;
  GenDialectsContext &operator=(const GenDialectsContext &rhs) = delete;
  GenDialectsContext(GenDialectsContext &&rhs) = delete;
  GenDialectsContext &operator=(GenDialectsContext &&rhs) = delete;

  Predicate *getPredicateImpl(llvm::Init *record, llvm::raw_ostream &errs);

  llvm::Init *m_voidTy = nullptr;
  llvm::Init *m_any = nullptr;
  bool m_attrsComplete = false;
  llvm::DenseMap<llvm::Record *, std::unique_ptr<Trait>> m_traits;
  llvm::DenseMap<llvm::Init *, std::unique_ptr<Predicate>> m_predicates;
  llvm::DenseMap<llvm::Record *, std::unique_ptr<Attr>> m_attrs;
  llvm::DenseMap<llvm::Record *, std::unique_ptr<OpClass>> m_opClasses;
  llvm::DenseMap<llvm::Record *, std::unique_ptr<GenDialect>> m_dialects;
};

} // namespace llvm_dialects
