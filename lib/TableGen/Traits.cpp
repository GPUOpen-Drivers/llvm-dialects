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

#include "llvm-dialects/TableGen/Traits.h"

#include "llvm-dialects/TableGen/Format.h"

#include "llvm/TableGen/Record.h"

using namespace llvm;
using namespace llvm_dialects;

namespace {

class LlvmEnumAttributeTrait : public LlvmAttributeTrait {
public:
  LlvmEnumAttributeTrait() : LlvmAttributeTrait(Kind::LlvmEnumAttributeTrait) {}

  void init(GenDialectsContext *context, llvm::Record *record) override;

  void addAttribute(llvm::raw_ostream &out, FmtContext &fmt) const override;

  llvm::StringRef getLlvmEnum() const { return m_llvmEnum; }

  static bool classof(const Trait *t) {
    return t->getKind() == Kind::LlvmEnumAttributeTrait;
  }

private:
  std::string m_llvmEnum;
};

class LlvmMemoryAttributeTrait : public LlvmAttributeTrait {
public:
  LlvmMemoryAttributeTrait()
      : LlvmAttributeTrait(Kind::LlvmMemoryAttributeTrait) {}

  void init(GenDialectsContext *context, llvm::Record *record) override;

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

} // anonymous namespace

std::unique_ptr<Trait> Trait::fromRecord(GenDialectsContext *context,
                                         llvm::Record *traitRec) {
  std::unique_ptr<Trait> result;
  if (traitRec->isSubClassOf("LlvmEnumAttributeTrait")) {
    result = std::make_unique<LlvmEnumAttributeTrait>();
  } else if (traitRec->isSubClassOf("Memory")) {
    result = std::make_unique<LlvmMemoryAttributeTrait>();
  } else {
    report_fatal_error(Twine("unsupported trait: ") + traitRec->getName());
  }
  result->init(context, traitRec);
  return result;
}

void Trait::init(GenDialectsContext *context, Record *record) {
  m_record = record;
}

StringRef Trait::getName() const { return m_record->getName(); }

void LlvmEnumAttributeTrait::init(GenDialectsContext *context, Record *record) {
  LlvmAttributeTrait::init(context, record);
  m_llvmEnum = record->getValueAsString("llvmEnum");
}

void LlvmEnumAttributeTrait::addAttribute(raw_ostream &out,
                                          FmtContext &fmt) const {
  out << tgfmt("$attrBuilder.addAttribute(::llvm::Attribute::$0);\n", &fmt,
               getLlvmEnum());
}

void LlvmMemoryAttributeTrait::init(GenDialectsContext *context,
                                    llvm::Record *record) {
  LlvmAttributeTrait::init(context, record);

  auto *effects = record->getValueAsListInit("effects");
  for (auto *effectInit : *effects) {
    Effect effect;
    auto *effectDag = cast<DagInit>(effectInit);
    Record *op = effectDag->getOperatorAsDef(record->getLoc());
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
