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

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include <optional>

namespace llvm {
class Record;
} // namespace llvm

namespace llvm_dialects {

class GenDialectsContext;
class FmtContext;

class Constraint {
public:
  enum class Kind : uint8_t {
    Type_First,
    BuiltinType = Type_First,
    DialectType,
    Type_Last = DialectType,

    Attr,
    BaseCPred_First,
    BaseCPred = BaseCPred_First,
    SameTypes,
    BaseCPred_Last = SameTypes,
  };

  virtual ~Constraint() = default;

  virtual void init(GenDialectsContext *context, llvm::Record *record);

  Kind getKind() const { return m_kind; }
  llvm::Record *getRecord() const { return m_record; }
  llvm::StringRef getName() const;
  llvm::StringRef getCppType() const;

  virtual std::pair<unsigned, unsigned> getMinMaxArgs() const = 0;

  /// Return a string containing a C++ expression that applies this predicate
  /// to the given arguments.
  virtual std::string
  apply(FmtContext *fmt, llvm::ArrayRef<llvm::StringRef> arguments) const = 0;

protected:
  Constraint(Kind kind) : m_kind(kind) {}

private:
  const Kind m_kind;
  llvm::Record *m_record = nullptr;
};

class Type : public Constraint {
public:
  static bool classof(const Constraint *c) {
    return c->getKind() >= Kind::Type_First && c->getKind() <= Kind::Type_Last;
  }

  void init(GenDialectsContext *context, llvm::Record *record) override {
    Constraint::init(context, record);
  }

  std::pair<unsigned, unsigned> getMinMaxArgs() const final { return {1, 1}; }

  std::string apply(FmtContext *fmt,
                    llvm::ArrayRef<llvm::StringRef> arguments) const final;

  /// Return a string containing a C++ expression that returns the `llvm::Type*`
  /// for this type.
  virtual std::string getLlvmType(FmtContext *fmt) const = 0;

protected:
  Type(Kind kind) : Constraint(kind) {}
};

class BuiltinType : public Type {
public:
  BuiltinType() : Type(Kind::BuiltinType) {}

  void init(GenDialectsContext *context, llvm::Record *record) override;

  llvm::StringRef getGetter() const { return m_getter; }

  static bool classof(const Constraint *c) {
    return c->getKind() == Kind::BuiltinType;
  }

  std::string getLlvmType(FmtContext *fmt) const final;

private:
  std::string m_getter;
};

class DialectType : public Type {
public:
  DialectType() : Type(Kind::DialectType) {}

  void init(GenDialectsContext *context, llvm::Record *record) override;

  llvm::Record *getDialectRec() const { return m_dialectRec; }
  llvm::StringRef getMnemonic() const { return m_mnemonic; }

  static bool classof(const Constraint *c) {
    return c->getKind() == Kind::DialectType;
  }

  std::string getLlvmType(FmtContext *fmt) const final;

private:
  llvm::Record *m_dialectRec = nullptr;
  std::string m_mnemonic;
};

class Attr : public Constraint {
public:
  Attr() : Constraint(Kind::Attr) {}

  void init(GenDialectsContext *context, llvm::Record *record) override;

  std::pair<unsigned, unsigned> getMinMaxArgs() const final { return {1, 1}; }

  std::string apply(FmtContext *fmt,
                    llvm::ArrayRef<llvm::StringRef> arguments) const final {
    // Attr constraints don't imply additional constraints at the moment, but
    // this could be added in the future, e.g. for enum attributes.
    return "true";
  }

  llvm::StringRef getCppType() const { return m_cppType; }
  Type *getLlvmType() const { return m_llvmType; }
  llvm::StringRef getToLlvmValue() const { return m_toLlvmValue; }
  llvm::StringRef getFromLlvmValue() const { return m_fromLlvmValue; }

  static bool classof(const Constraint *c) {
    return c->getKind() == Kind::Attr;
  }

private:
  std::string m_cppType;
  Type *m_llvmType = nullptr;
  std::string m_toLlvmValue;
  std::string m_fromLlvmValue;
};

class BaseCPred : public Constraint {
public:
  BaseCPred(llvm::StringRef name) : Constraint(getBaseCPredKind(name)) {}

  static bool classof(const Constraint *c) {
    return c->getKind() >= Kind::BaseCPred_First &&
           c->getKind() <= Kind::BaseCPred_Last;
  }

  void init(GenDialectsContext *context, llvm::Record *record) override;

  std::pair<unsigned, unsigned> getMinMaxArgs() const final {
    unsigned min = m_arguments.size();
    return {min, m_variadic ? std::numeric_limits<unsigned>::max() : min};
  }

  std::string apply(FmtContext *fmt,
                    llvm::ArrayRef<llvm::StringRef> arguments) const final;

private:
  static Kind getBaseCPredKind(llvm::StringRef name) {
    if (name == "SameTypes")
      return Kind::SameTypes;
    return Kind::BaseCPred;
  }

  struct Argument {
    std::string name;
  };

  llvm::SmallVector<Argument> m_arguments;
  std::optional<Argument> m_variadic;
  std::string m_predExpr;
};

} // namespace llvm_dialects
