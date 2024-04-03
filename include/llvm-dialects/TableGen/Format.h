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
//===----------------------------------------------------------------------===//
//
// NOTE: This is a copy, with minor modifications, of mlir/TableGen/Format.h
//       from the LLVM source. Perhaps we should make that utility part of
//       llvm/Support and then remove this copy once we depend on recent enough
//       LLVM?
//
// This file declares utilities for formatting strings. They are specially
// tailored to the needs of TableGen'ing op definitions and rewrite rules,
// so they are not expected to be used as widely applicable utilities.
//
//===----------------------------------------------------------------------===//

#pragma once

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/FormatVariadic.h"
#include <optional>

namespace llvm_dialects {

namespace llvm_support_detail =
#if !defined(LLVM_MAIN_REVISION) || LLVM_MAIN_REVISION >= 494496
    llvm::support::detail;
#else
    llvm::detail;
#endif

/// Format context containing substitutions for special placeholders.
///
/// This context divides special placeholders into two categories: builtin ones
/// and custom ones.
///
/// Builtin placeholders are baked into `FmtContext` and each one of them has a
/// dedicated setter. They can be used in all dialects. Their names follow the
/// convention of `$_<name>`. The rationale of the leading underscore is to
/// avoid confusion and name collision: op arguments/attributes/results are
/// named as $<name>, and we can potentially support referencing those entities
/// directly in the format template in the future.
//
/// Custom ones are registered by dialect-specific TableGen backends and use the
/// same unified setter.
class FmtContext {
public:
  // Placeholder kinds
  enum class PHKind : char {
    None,
    Custom,  // For custom placeholders
    Context, // For the $_context placeholder
    Builder, // For the $_builder placeholder
    Op,      // For the $_op placeholder
    Self,    // For the $_self placeholder
  };

  FmtContext() : current(&base) {}

  // Create a format context with a list of substitutions.
  FmtContext(llvm::ArrayRef<std::pair<llvm::StringRef, llvm::StringRef>> subs);

  // Setter for custom placeholders
  FmtContext &addSubst(llvm::StringRef placeholder, const llvm::Twine &subst);

  // Setters for builtin placeholders
  FmtContext &withContext(llvm::Twine subst);
  FmtContext &withBuilder(llvm::Twine subst);
  FmtContext &withOp(llvm::Twine subst);
  FmtContext &withSelf(llvm::Twine subst);

  std::optional<llvm::StringRef> getSubstFor(PHKind placeholder) const;
  std::optional<llvm::StringRef> getSubstFor(llvm::StringRef placeholder) const;

  static PHKind getPlaceHolderKind(llvm::StringRef str);

private:
  friend class FmtContextScope;

  struct PHKindInfo : llvm::DenseMapInfo<PHKind> {
    using CharInfo = DenseMapInfo<char>;

    static inline PHKind getEmptyKey() {
      return static_cast<PHKind>(CharInfo::getEmptyKey());
    }
    static inline PHKind getTombstoneKey() {
      return static_cast<PHKind>(CharInfo::getTombstoneKey());
    }
    static unsigned getHashValue(const PHKind &val) {
      return CharInfo::getHashValue(static_cast<char>(val));
    }

    static bool isEqual(const PHKind &lhs, const PHKind &rhs) {
      return lhs == rhs;
    }
  };

  struct Substitutions {
    llvm::SmallDenseMap<PHKind, std::string, 4, PHKindInfo> builtinSubstMap;
    llvm::StringMap<std::string> customSubstMap;
  };

  Substitutions* current;
  Substitutions base;

};

/// RAII-type for opening a scope in which substitutions of the given context
/// can be added or changed. The context is reset to its original state when
/// the scope ends.
///
/// @example Example usage:
/// @code
///   FmtContext fmt;
///   fmt.withContext(...);
///
///   ... use fmt here ...
///
///   {
///     FmtContextScope scope{fmt};
///     fmt.addSubst("custom", ...);
///
///     ... use fmt here ...
///   }
/// @endcode
class FmtContextScope {
public:
  explicit FmtContextScope(FmtContext& ctx);
  ~FmtContextScope();

private:
  FmtContext &context;
  FmtContext::Substitutions *old;
  FmtContext::Substitutions substitutions;
};

/// Struct representing a replacement segment for the formatted string. It can
/// be a segment of the formatting template (for `Literal`) or a replacement
/// parameter (for `PositionalPH`, `PositionalRangePH`, `SpecialPH`, and
/// `IndirectSpecialPH`).
struct FmtReplacement {
  enum class Type {
    Empty,
    Literal,
    PositionalPH,
    PositionalRangePH,
    SpecialPH,
    IndirectSpecialPH,
  };

  FmtReplacement() = default;
  explicit FmtReplacement(llvm::StringRef literal)
      : type(Type::Literal), spec(literal) {}
  FmtReplacement(llvm::StringRef spec, size_t index, Type type)
      : type(type), spec(spec), index(index) {}
  FmtReplacement(llvm::StringRef spec, FmtContext::PHKind placeholder)
      : type(Type::SpecialPH), spec(spec), placeholder(placeholder) {}

  Type type = Type::Empty;
  llvm::StringRef spec;
  size_t index = 0;
  FmtContext::PHKind placeholder = FmtContext::PHKind::None;

  static constexpr size_t kUnset = -1;
};

class FmtObjectBase {
private:
  static std::pair<FmtReplacement, llvm::StringRef> splitFmtSegment(llvm::StringRef fmt);
  static std::vector<FmtReplacement> parseFormatString(llvm::StringRef fmt);

protected:
  // The parameters are stored in a std::tuple, which does not provide runtime
  // indexing capabilities.  In order to enable runtime indexing, we use this
  // structure to put the parameters into a std::vector.  Since the parameters
  // are not all the same type, we use some type-erasure by wrapping the
  // parameters in a template class that derives from a non-template superclass.
  // Essentially, we are converting a std::tuple<Derived<Ts...>> to a
  // std::vector<Base*>.
  struct CreateAdapters {
    template <typename... Ts>
    std::vector<llvm_support_detail::format_adapter *> operator()(Ts &...items) {
      return std::vector<llvm_support_detail::format_adapter *>{&items...};
    }
  };

  llvm::StringRef fmt;
  const FmtContext *context;
  std::vector<llvm_support_detail::format_adapter *> adapters;
  std::vector<FmtReplacement> replacements;

public:
  FmtObjectBase(llvm::StringRef fmt, const FmtContext *ctx, size_t numParams)
      : fmt(fmt), context(ctx), replacements(parseFormatString(fmt)) {}

  FmtObjectBase(const FmtObjectBase &that) = delete;

  FmtObjectBase(FmtObjectBase &&that)
      : fmt(that.fmt), context(that.context),
        adapters(), // adapters are initialized by FmtObject
        replacements(std::move(that.replacements)) {}

  void format(llvm::raw_ostream &s) const;

  std::string str() const {
    std::string result;
    llvm::raw_string_ostream s(result);
    format(s);
    return s.str();
  }

  template <unsigned N> llvm::SmallString<N> sstr() const {
    llvm::SmallString<N> result;
    llvm::raw_svector_ostream s(result);
    format(s);
    return result;
  }

  template <unsigned N> operator llvm::SmallString<N>() const { return sstr<N>(); }

  operator std::string() const { return str(); }
};

template <typename Tuple> class FmtObject : public FmtObjectBase {
  // Storage for the parameter adapters.  Since the base class erases the type
  // of the parameters, we have to own the storage for the parameters here, and
  // have the base class store type-erased pointers into this tuple.
  Tuple parameters;

public:
  FmtObject(llvm::StringRef fmt, const FmtContext *ctx, Tuple &&params)
      : FmtObjectBase(fmt, ctx, std::tuple_size<Tuple>::value),
        parameters(std::move(params)) {
    adapters.reserve(std::tuple_size<Tuple>::value);
    adapters = std::apply(CreateAdapters(), parameters);
  }

  FmtObject(FmtObject const &that) = delete;

  FmtObject(FmtObject &&that)
      : FmtObjectBase(std::move(that)), parameters(std::move(that.parameters)) {
    adapters.reserve(that.adapters.size());
    adapters = std::apply(CreateAdapters(), parameters);
  }
};

class FmtStrVecObject : public FmtObjectBase {
public:
  using StrFormatAdapter =
      decltype(llvm_support_detail::build_format_adapter(std::declval<std::string>()));

  FmtStrVecObject(llvm::StringRef fmt, const FmtContext *ctx,
                  llvm::ArrayRef<std::string> params);
  FmtStrVecObject(FmtStrVecObject const &that) = delete;
  FmtStrVecObject(FmtStrVecObject &&that);

private:
  llvm::SmallVector<StrFormatAdapter, 16> parameters;
};

/// Formats text by substituting placeholders in format string with replacement
/// parameters.
///
/// There are two categories of placeholders accepted, both led by a '$' sign:
///
/// 1.a Positional placeholder: $[0-9]+
/// 1.b Positional range placeholder: $[0-9]+...
/// 2. Special placeholder:    $[a-zA-Z_][a-zA-Z0-9_]*
/// 3. Indirect special placeholder: $\*[0-9]+
///
/// Replacement parameters for positional placeholders are supplied as the
/// `vals` parameter pack with 1:1 mapping. That is, $0 will be replaced by the
/// first parameter in `vals`, $1 by the second one, and so on. Note that you
/// can use the positional placeholders in any order and repeat any times, for
/// example, "$2 $1 $1 $0" is accepted.
///
/// Replace parameters for positional range placeholders are supplied as if
/// positional placeholders were specified with commas separating them.
///
/// Replacement parameters for special placeholders are supplied using the `ctx`
/// format context.
///
/// Indirect special placeholders take a positional argument as the name of a
/// special placeholder that is supplied using the `ctx` format context.
/// For example, if the first parameter in `vals` is "foo", then "$*0" has the
/// same replacement as "$foo".
///
/// The `fmt` is recorded as a `StringRef` inside the returned `FmtObject`.
/// The caller needs to make sure the underlying data is available when the
/// `FmtObject` is used.
///
/// `ctx` accepts a nullptr if there is no special placeholder is used.
///
/// If no substitution is provided for a placeholder or any error happens during
/// format string parsing or replacement, the placeholder will be outputted
/// as-is with an additional marker '<no-subst-found>', to aid debugging.
///
/// To print a '$' literally, escape it with '$$'.
///
/// This utility function is inspired by LLVM formatv(), with modifications
/// specially tailored for TableGen C++ generation usage:
///
/// 1. This utility use '$' instead of '{' and '}' for denoting the placeholder
///    because '{' and '}' are frequently used in C++ code.
/// 2. This utility does not support format layout because it is rarely needed
///    in C++ code generation.
template <typename... Ts>
inline auto tgfmt(llvm::StringRef fmt, const FmtContext *ctx, Ts &&...vals)
    -> FmtObject<decltype(std::make_tuple(
        llvm_support_detail::build_format_adapter(std::forward<Ts>(vals))...))> {
  using ParamTuple = decltype(std::make_tuple(
      llvm_support_detail::build_format_adapter(std::forward<Ts>(vals))...));
  return FmtObject<ParamTuple>(
      fmt, ctx,
      std::make_tuple(
          llvm_support_detail::build_format_adapter(std::forward<Ts>(vals))...));
}

inline FmtStrVecObject tgfmt(llvm::StringRef fmt, const FmtContext *ctx,
                             llvm::ArrayRef<std::string> params) {
  return FmtStrVecObject(fmt, ctx, params);
}

} // namespace llvm_dialects
