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

#include "llvm-dialects/TableGen/Dialects.h"

#include "llvm-dialects/TableGen/Constraints.h"
#include "llvm-dialects/TableGen/DialectType.h"
#include "llvm-dialects/TableGen/Operations.h"
#include "llvm-dialects/TableGen/Predicates.h"
#include "llvm-dialects/TableGen/Traits.h"

#include "llvm/ADT/Twine.h"
#include "llvm/TableGen/Record.h"

using namespace llvm;
using namespace llvm_dialects;

void GenDialect::finalize(raw_ostream &errs) {
  // Build the list of different attribute lists.
  auto traitLess = [](Trait *lhs, Trait *rhs) {
    return lhs->getName() < rhs->getName();
  };
  auto opLess = [traitLess](Operation *lhs, Operation *rhs) {
    return std::lexicographical_compare(lhs->traits.begin(), lhs->traits.end(),
                                        rhs->traits.begin(), rhs->traits.end(),
                                        traitLess);
  };

  std::vector<Operation *> traitOperations;

  bool hasDuplicates = false;
  for (const auto &[op, count] : operationCounts) {
    if (count != 1) {
      errs << "Found op with non-unique mnemonic: " << op << '\n';
      hasDuplicates = true;
    }
  }

  if (hasDuplicates)
    report_fatal_error(
        "Aborting dialect generation since non-unique mnemonics were found!");

  for (const auto &op : operations) {
    if (op->traits.empty())
      continue;

    llvm::sort(op->traits, traitLess);

    traitOperations.push_back(op.get());
  }

  llvm::sort(traitOperations, opLess);

  Operation *current = nullptr;
  for (Operation *op : traitOperations) {
    if (!current || opLess(current, op)) {
      m_attributeLists.push_back(op->traits);
      current = op;
    }
    op->m_attributeListIdx = m_attributeLists.size() - 1;
  }
}

GenDialectsContext::GenDialectsContext() = default;
GenDialectsContext::~GenDialectsContext() = default;

Trait *GenDialectsContext::getTrait(Record *traitRec) {
  if (!traitRec->isSubClassOf("Trait"))
    report_fatal_error(Twine("Trying to use '") + traitRec->getName() +
                       "' as a trait, but it is not a subclass of 'Trait'");

  auto &result = m_traits[traitRec];
  if (!result)
    result = Trait::fromRecord(this, traitRec);
  return result.get();
}

Predicate *GenDialectsContext::getPredicate(Init *init, raw_ostream &errs) {
  Predicate *op = getPredicateImpl(init, errs);
  if (!op)
    errs << "... while looking up predicate: " << init->getAsString() << '\n';
  return op;
}

Predicate *GenDialectsContext::getPredicateImpl(Init *init, raw_ostream &errs) {
  auto it = m_predicates.find(init);
  if (it != m_predicates.end()) {
    if (!it->second)
      errs << "  repeated predicate lookup\n";
    return it->second.get();
  }

  // Leave a marker to guard against recursive lookups.
  m_predicates.try_emplace(init);

  auto owner = Predicate::parse(errs, *this, init);
  Predicate *op = owner.get();
  m_predicates.find(init)->second = std::move(owner);

  return op;
}

Attr *GenDialectsContext::getAttr(llvm::Record *record,
                                  llvm::raw_ostream &errs) {
  auto it = m_attrs.find(record);
  if (it == m_attrs.end()) {
    errs << "  not an attribute: " << record->getName() << '\n';
    return nullptr;
  }

  Attr *attr = it->second.get();
  assert(!m_attrsComplete || attr->getLlvmType());
  return attr;
}

GenDialect *GenDialectsContext::getDialect(Record *dialectRec) {
  if (!dialectRec->isSubClassOf("Dialect"))
    report_fatal_error(Twine("Trying to use '") + dialectRec->getName() +
                       "' as a dialect, but it is not a subclass of 'Dialect'");

  auto it = m_dialects.find(dialectRec);
  if (it == m_dialects.end())
    report_fatal_error(Twine("Trying to use dialect '") +
                       dialectRec->getName() +
                       "', but has not been initialized");

  return it->second.get();
}

OpClass *GenDialectsContext::getOpClass(Record *opClassRec) {
  if (opClassRec->getName() == "NoSuperClass")
    return nullptr;

  if (!opClassRec->isSubClassOf("OpClass"))
    report_fatal_error(
        Twine("Trying to use '") + opClassRec->getName() +
        "' as operation class, but it is not a subclass of 'OpClass'");

  auto it = m_opClasses.find(opClassRec);
  if (it != m_opClasses.end()) {
    if (!it->second)
      report_fatal_error(Twine("Circular superclass relations involving '") +
                         opClassRec->getName() + "'");
    return it->second.get();
  }

  // Leave a marker in the map to detect recursive superclass relations.
  m_opClasses.try_emplace(opClassRec);

  auto opClassOwner = OpClass::parse(llvm::errs(), *this, opClassRec);
  if (!opClassOwner) {
    llvm::errs() << "... in operation class " << opClassRec->getName() << '\n';
    report_fatal_error("parse error in operation class");
  }
  OpClass *opClass = opClassOwner.get();

  // Re-find the class record in case there were recursive lookups.
  m_opClasses.find(opClassRec)->second = std::move(opClassOwner);
  return opClass;
}

void GenDialectsContext::init(RecordKeeper &records,
                              const DenseSet<StringRef> &dialects) {
  for (Record *record : records.getAllDerivedDefinitions("Attr")) {
    auto owner = Attr::parse(llvm::errs(), *this, record);
    if (!record)
      report_fatal_error(Twine("Error parsing Attr ") + record->getName());

    m_attrs.try_emplace(record, std::move(owner));
  }

  for (Record *record : records.getAllDerivedDefinitions("AttrLlvmType")) {
    Attr *attr = getAttr(record->getValueAsDef("attr"), llvm::errs());
    assert(attr);
    attr->setLlvmType(record->getValueInit("llvmType"));
  }
  m_attrsComplete = true;

  m_voidTy = records.getDef("VoidTy")->getDefInit();
  m_any = records.getDef("any")->getDefInit();
  assert(m_voidTy && m_any);

  for (Record *dialectRec : records.getAllDerivedDefinitions("Dialect")) {
    auto name = dialectRec->getValueAsString("name");
    if (!dialects.contains(name))
      continue;

    auto dialect = std::make_unique<GenDialect>();
    dialect->record = dialectRec;
    dialect->cppName = dialectRec->getName();
    dialect->name = name;
    dialect->cppNamespace = dialectRec->getValueAsString("cppNamespace");
    m_dialects.insert(std::make_pair(dialectRec, std::move(dialect)));
  }

  for (Record *typeRec : records.getAllDerivedDefinitions("DialectType")) {
    auto *dialectType =
        cast<DialectType>(getPredicate(typeRec->getDefInit(), llvm::errs()));
    if (!dialectType) {
      report_fatal_error(Twine("failed to parse DialectType ") +
                         typeRec->getName());
    }
    auto dialectIt = m_dialects.find(dialectType->getDialectRec());
    if (dialectIt != m_dialects.end())
      dialectIt->second->types.push_back(dialectType);
  }

  for (Record *opRec : records.getAllDerivedDefinitions("Op")) {
    Record *dialectRec = opRec->getValueAsDef("dialect");
    auto dialectIt = m_dialects.find(dialectRec);
    if (dialectIt == m_dialects.end())
      continue;

    if (!Operation::parse(llvm::errs(), this, dialectIt->second.get(), opRec)) {
      llvm::errs() << "... in operation " << opRec->getName() << '\n';
      report_fatal_error("error parsing operation");
    }
  }

  for (auto &dialectEntry : m_dialects)
    dialectEntry.second->finalize(llvm::errs());
}
