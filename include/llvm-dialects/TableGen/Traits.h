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

#include <cstdint>
#include <memory>
#include <string>

#include "llvm/ADT/StringRef.h"

namespace llvm {
class raw_ostream;
class Record;
}

namespace llvm_dialects {

class FmtContext;
class GenDialectsContext;

class Trait {
public:
  enum class Kind : uint8_t {
    LlvmAttributeTrait_First,
    LlvmEnumAttributeTrait = LlvmAttributeTrait_First,
    LlvmMemoryAttributeTrait,
    LlvmAttributeTrait_Last = LlvmMemoryAttributeTrait,
  };

  static std::unique_ptr<Trait> fromRecord(GenDialectsContext *context,
                                           llvm::Record *record);

  virtual ~Trait() = default;

  virtual void init(GenDialectsContext *context, llvm::Record *record);

  Kind getKind() const { return m_kind; }
  llvm::Record *getRecord() const { return m_record; }
  llvm::StringRef getName() const;

protected:
  Trait(Kind kind) : m_kind(kind) {}

private:
  const Kind m_kind;
  llvm::Record *m_record = nullptr;
};

class LlvmAttributeTrait : public Trait {
public:
  LlvmAttributeTrait(Kind kind) : Trait(kind) {}

  virtual void addAttribute(llvm::raw_ostream &out, FmtContext &fmt) const = 0;

  static bool classof(const Trait *t) {
    return t->getKind() >= Kind::LlvmAttributeTrait_First &&
           t->getKind() <= Kind::LlvmAttributeTrait_Last;
  }
};

} // namespace llvm_dialects
