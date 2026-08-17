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

#include "llvm-dialects/TableGen/Traits.h"
#include "llvm-dialects/TableGen/Common.h"

#include "llvm-dialects/TableGen/Format.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/TableGen/Record.h"

using namespace llvm;
using namespace llvm_dialects;

namespace {

static cl::opt<bool> NoMemoryEffects(
    "no-memory-effects", cl::init(false),
    cl::desc("as a workaround for supporting older LLVM versions, do not emit "
             "code using the memory(...) attribute"));

class LlvmEnumAttributeTrait : public LlvmAttributeTrait {
public:
  LlvmEnumAttributeTrait() : LlvmAttributeTrait(Kind::LlvmEnumAttributeTrait) {}

  void init(GenDialectsContext *context, RecordTy *record, int idx) override;

  virtual void addAttribute(llvm::raw_ostream &out,
                            FmtContext &fmt) const override;

  llvm::StringRef getLlvmEnum() const { return m_llvmEnum; }

  int getIdx() const { return m_idx - 1; }

  static bool classof(const Trait *t) {
    return t->getKind() == Kind::LlvmEnumAttributeTrait;
  }

private:
  std::string m_llvmEnum;
  int m_idx;
};

class LlvmMemoryAttributeTrait : public LlvmAttributeTrait {
public:
  LlvmMemoryAttributeTrait()
      : LlvmAttributeTrait(Kind::LlvmMemoryAttributeTrait) {}

  void init(GenDialectsContext *context, RecordTy *record, int idx) override;

  void addAttribute(llvm::raw_ostream &out, FmtContext &fmt) const override;

  static bool classof(const Trait *t) {
    return t->getKind() == Kind::LlvmMemoryAttributeTrait;
  }

  struct Effect {
    bool read = false;
    bool write = false;
    SmallVector<std::string> locations; // empty vector means all locations
  };

private:
  SmallVector<Effect> m_effects;
  std::string m_llvmEnum;
};

class LlvmAllocSizeAttributeTrait : public LlvmAttributeTrait {
public:
  LlvmAllocSizeAttributeTrait()
      : LlvmAttributeTrait(Kind::LlvmAllocSizeAttributeTrait) {}

  void init(GenDialectsContext *context, RecordTy *record, int idx) override;

  void verifyArguments(StringRef opName, unsigned numFullArgs,
                       bool hasVariadicArgument) const override;

  void addAttribute(llvm::raw_ostream &out, FmtContext &fmt) const override;

  static bool classof(const Trait *t) {
    return t->getKind() == Kind::LlvmAllocSizeAttributeTrait;
  }

private:
  int64_t m_sizeArgIdx = 0;
  int64_t m_numElemsArgIdx = -1;
};

class LlvmAllocKindAttributeTrait : public LlvmAttributeTrait {
public:
  LlvmAllocKindAttributeTrait()
      : LlvmAttributeTrait(Kind::LlvmAllocKindAttributeTrait) {}

  void init(GenDialectsContext *context, RecordTy *record, int idx) override;

  void addAttribute(llvm::raw_ostream &out, FmtContext &fmt) const override;

  static bool classof(const Trait *t) {
    return t->getKind() == Kind::LlvmAllocKindAttributeTrait;
  }

private:
  SmallVector<std::string> m_kinds; // names of llvm::AllocFnKind enumerators
};

} // anonymous namespace

bool llvm_dialects::noMemoryEffects() {
  return NoMemoryEffects;
}

std::unique_ptr<Trait> Trait::fromRecord(GenDialectsContext *context,
                                         RecordTy *traitRec, int idx) {
  std::unique_ptr<Trait> result;
  if (traitRec->isSubClassOf("LlvmEnumAttributeTrait")) {
    result = std::make_unique<LlvmEnumAttributeTrait>();
  } else if (traitRec->isSubClassOf("Memory")) {
    result = std::make_unique<LlvmMemoryAttributeTrait>();
  } else if (traitRec->isSubClassOf("AllocSize")) {
    result = std::make_unique<LlvmAllocSizeAttributeTrait>();
  } else if (traitRec->isSubClassOf("AllocKind")) {
    result = std::make_unique<LlvmAllocKindAttributeTrait>();
  } else {
    report_fatal_error(Twine("unsupported trait: ") + traitRec->getName());
  }
  result->init(context, traitRec, idx);
  return result;
}

void Trait::init(GenDialectsContext *context, RecordTy *record, int idx) {
  m_record = record;
}

StringRef Trait::getName() const { return m_record->getName(); }

void LlvmEnumAttributeTrait::init(GenDialectsContext *context,
                                  RecordTy *record, int idx) {
  LlvmAttributeTrait::init(context, record, idx);
  m_llvmEnum = record->getValueAsString("llvmEnum");
  m_idx = idx;
}

void LlvmEnumAttributeTrait::addAttribute(raw_ostream &out,
                                          FmtContext &fmt) const {
  if (m_idx < 0) {
    // Function attribute.
    out << tgfmt("$attrBuilder.addAttribute(::llvm::Attribute::$0);\n", &fmt,
                 getLlvmEnum());
  } else if (m_idx == 0) {
    // Return attribute.
    out << tgfmt("$argAttrList = $argAttrList.addRetAttribute(context, "
                 "::llvm::Attribute::$0);\n",
                 &fmt, getLlvmEnum());
  } else {
    // Param attribute.
    if (getLlvmEnum() == "NoCapture") {
      out << tgfmt("$argAttrList = $argAttrList.addParamAttribute(context, $0, "
                   "::llvm::Attribute::getWithCaptureInfo(context, "
                   "llvm::CaptureInfo::none()));\n",
                   &fmt, getIdx());
    } else {
      out << tgfmt("$argAttrList = $argAttrList.addParamAttribute(context, $0, "
                   "::llvm::Attribute::$1);\n",
                   &fmt, getIdx(), getLlvmEnum());
    }
  }
}

void LlvmMemoryAttributeTrait::init(GenDialectsContext *context,
                                    RecordTy *record, int idx) {
  LlvmAttributeTrait::init(context, record, idx);

  auto *effects = record->getValueAsListInit("effects");
  for (auto *effectInit : *effects) {
    Effect effect;
    auto *effectDag = cast<DagInit>(effectInit);
    RecordTy *op = effectDag->getOperatorAsDef(record->getLoc());
    if (op->getName() == "read") {
      effect.read = true;
    } else if (op->getName() == "write") {
      effect.write = true;
    } else if (op->getName() == "readwrite") {
      effect.read = true;
      effect.write = true;
    } else {
      report_fatal_error(Twine("bad operator ") + op->getName() +
                         " in memory effect");
    }

    for (auto *locationInit :
         make_range(effectDag->arg_begin(), effectDag->arg_end())) {
      auto *locationDef = dyn_cast<DefInit>(locationInit);
      auto *location = locationDef ? locationDef->getDef() : nullptr;
      if (!location || !location->isSubClassOf("LlvmMemoryLocation")) {
        report_fatal_error(Twine("memory effect location must be an "
                                 "LlvmMemoryLocation, but is ") +
                           locationInit->getAsString());
      }

      effect.locations.emplace_back(location->getValueAsString("name"));
    }

    m_effects.push_back(std::move(effect));
  }
}

namespace {

struct EffectWriter {
  EffectWriter(FmtContext &fmt, const LlvmMemoryAttributeTrait::Effect &effect)
      : fmt(fmt), effect(effect) {}

  FmtContext &fmt;
  const LlvmMemoryAttributeTrait::Effect &effect;
};

raw_ostream &operator<<(raw_ostream &out, const EffectWriter &writer) {
  StringRef mri;
  if (writer.effect.read && writer.effect.write)
    mri = "::llvm::ModRefInfo::ModRef";
  else if (writer.effect.write)
    mri = "::llvm::ModRefInfo::Mod";
  else
    mri = "::llvm::ModRefInfo::Ref";

  if (writer.effect.locations.empty()) {
    out << tgfmt("::llvm::MemoryEffects($0)", &writer.fmt, mri);
  } else {
    bool first = true;
    for (const auto &location : writer.effect.locations) {
      if (!first)
        out << " | ";
      first = false;
      out << tgfmt("::llvm::MemoryEffects(::llvm::MemoryEffects::Location::$0, $1)",
                   &writer.fmt, location, mri);
    }
  }

  return out;
}

} // anonymous namespace

void LlvmMemoryAttributeTrait::addAttribute(raw_ostream &out,
                                            FmtContext &fmt) const {
  if (noMemoryEffects())
    return;

  if (m_effects.empty()) {
    out << tgfmt("$attrBuilder.addMemoryAttr(::llvm::MemoryEffects::none());\n",
                 &fmt);
    return;
  }
  if (m_effects.size() == 1) {
    out << tgfmt("$attrBuilder.addMemoryAttr($0);\n", &fmt,
                 EffectWriter(fmt, m_effects[0]));
    return;
  }

  out << "{\nauto effects = ::llvm::MemoryEffects::none();\n";
  for (const auto &effect : m_effects)
    out << "  effects |= " << EffectWriter(fmt, effect) << ";\n";
  out << tgfmt("  $attrBuilder.addMemoryAttr(effects);\n}\n", &fmt);
}

void LlvmAllocSizeAttributeTrait::init(GenDialectsContext *context,
                                       RecordTy *record, int idx) {
  LlvmAttributeTrait::init(context, record, idx);

  m_sizeArgIdx = record->getValueAsInt("sizeArgIdx");
  m_numElemsArgIdx = record->getValueAsInt("numElemsArgIdx");

  if (m_sizeArgIdx < 0) {
    report_fatal_error(Twine("AllocSize: sizeArgIdx must be non-negative, ") +
                       "but is " + Twine(m_sizeArgIdx));
  }
  if (m_numElemsArgIdx < -1) {
    report_fatal_error(
        Twine("AllocSize: numElemsArgIdx must be non-negative ") +
        "or -1 (absent), but is " + Twine(m_numElemsArgIdx));
  }
  if (m_numElemsArgIdx == m_sizeArgIdx) {
    report_fatal_error(
        Twine("AllocSize: sizeArgIdx and numElemsArgIdx must be ") +
        "distinct, but both are " + Twine(m_sizeArgIdx));
  }
}

void LlvmAllocSizeAttributeTrait::verifyArguments(
    StringRef opName, unsigned numFullArgs, bool hasVariadicArgument) const {
  // The declaration of an operation with a variadic argument list has no
  // fixed parameters at all, and LLVM's IR verifier rejects allocsize
  // argument indices that do not refer to fixed parameters of the declaration.
  if (hasVariadicArgument) {
    report_fatal_error(Twine("AllocSize: cannot be used on operation '") +
                       opName + "' which has a variadic argument list");
  }
  if (m_sizeArgIdx >= numFullArgs) {
    report_fatal_error(Twine("AllocSize: sizeArgIdx (") + Twine(m_sizeArgIdx) +
                       ") is out of bounds for operation '" + opName +
                       "' with " + Twine(numFullArgs) + " argument(s)");
  }
  if (m_numElemsArgIdx >= 0 && m_numElemsArgIdx >= numFullArgs) {
    report_fatal_error(Twine("AllocSize: numElemsArgIdx (") +
                       Twine(m_numElemsArgIdx) +
                       ") is out of bounds for operation '" + opName +
                       "' with " + Twine(numFullArgs) + " argument(s)");
  }
}

void LlvmAllocSizeAttributeTrait::addAttribute(raw_ostream &out,
                                               FmtContext &fmt) const {
  std::string numElemsArg =
      m_numElemsArgIdx < 0 ? "std::nullopt" : std::to_string(m_numElemsArgIdx);
  out << tgfmt("$attrBuilder.addAllocSizeAttr($0, $1);\n", &fmt, m_sizeArgIdx,
               numElemsArg);
}

void LlvmAllocKindAttributeTrait::init(GenDialectsContext *context,
                                       RecordTy *record, int idx) {
  LlvmAttributeTrait::init(context, record, idx);

  // Map of the allockind(...) names accepted by LLVM IR to the corresponding
  // llvm::AllocFnKind enumerators.
  static const std::pair<StringRef, StringRef> kindNames[] = {
      {"alloc", "Alloc"},   {"realloc", "Realloc"},
      {"free", "Free"},     {"uninitialized", "Uninitialized"},
      {"zeroed", "Zeroed"}, {"aligned", "Aligned"},
  };

  for (StringRef kind : record->getValueAsListOfStrings("kinds")) {
    const auto *it = llvm::find_if(
        kindNames, [kind](const std::pair<StringRef, StringRef> &entry) {
          return entry.first == kind;
        });
    if (it == std::end(kindNames)) {
      report_fatal_error(Twine("AllocKind: unknown allocation kind '") + kind +
                         "'; valid kinds are: alloc, realloc, free, "
                         "uninitialized, zeroed, aligned");
    }
    m_kinds.emplace_back(it->second);
  }

  if (m_kinds.empty())
    report_fatal_error("AllocKind: the list of allocation kinds is empty");
}

void LlvmAllocKindAttributeTrait::addAttribute(raw_ostream &out,
                                               FmtContext &fmt) const {
  std::string kinds;
  for (const auto &kind : m_kinds) {
    if (!kinds.empty())
      kinds += " | ";
    kinds += "::llvm::AllocFnKind::" + kind;
  }
  out << tgfmt("$attrBuilder.addAllocKindAttr($0);\n", &fmt, kinds);
}
