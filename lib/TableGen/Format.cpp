//===- Format.cpp - Utilities for String Format ---------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// NOTE: This is a copy, with minor modifications, of mlir/TableGen/Format.cpp
//       from the LLVM source. Perhaps we should make that utility part of
//       llvm/Support and then remove this copy once we depend on recent enough
//       LLVM?
//
// This file defines utilities for formatting strings. They are specially
// tailored to the needs of TableGen'ing op definitions and rewrite rules,
// so they are not expected to be used as widely applicable utilities.
//
//===----------------------------------------------------------------------===//

#include "llvm-dialects/TableGen/Format.h"

#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"

#include <cctype>

using namespace llvm_dialects;
using namespace llvm;

// Marker to indicate an error happened when replacing a placeholder.
static const char kMarkerForNoSubst[] = "<no-subst-found>";

FmtContext::FmtContext(ArrayRef<std::pair<StringRef, StringRef>> subs) {
  for (auto &sub : subs)
    addSubst(sub.first, sub.second);
}

FmtContext &FmtContext::addSubst(StringRef placeholder, const Twine &subst) {
  current->customSubstMap[placeholder] = subst.str();
  return *this;
}

FmtContext &FmtContext::withContext(Twine subst) {
  current->builtinSubstMap[PHKind::Context] = subst.str();
  return *this;
}

FmtContext &FmtContext::withBuilder(Twine subst) {
  current->builtinSubstMap[PHKind::Builder] = subst.str();
  return *this;
}

FmtContext &FmtContext::withOp(Twine subst) {
  current->builtinSubstMap[PHKind::Op] = subst.str();
  return *this;
}

FmtContext &FmtContext::withSelf(Twine subst) {
  current->builtinSubstMap[PHKind::Self] = subst.str();
  return *this;
}

std::optional<StringRef>
FmtContext::getSubstFor(FmtContext::PHKind placeholder) const {
  if (placeholder == FmtContext::PHKind::None ||
      placeholder == FmtContext::PHKind::Custom)
    return {};
  auto it = current->builtinSubstMap.find(placeholder);
  if (it == current->builtinSubstMap.end())
    return {};
  return StringRef(it->second);
}

std::optional<StringRef> FmtContext::getSubstFor(StringRef placeholder) const {
  auto it = current->customSubstMap.find(placeholder);
  if (it == current->customSubstMap.end())
    return {};
  return StringRef(it->second);
}

FmtContext::PHKind FmtContext::getPlaceHolderKind(StringRef str) {
  return StringSwitch<FmtContext::PHKind>(str)
      .Case("_context", FmtContext::PHKind::Context)
      .Case("_builder", FmtContext::PHKind::Builder)
      .Case("_op", FmtContext::PHKind::Op)
      .Case("_self", FmtContext::PHKind::Self)
      .Case("", FmtContext::PHKind::None)
      .Default(FmtContext::PHKind::Custom);
}

FmtContextScope::FmtContextScope(FmtContext& ctx)
  : context(ctx), old(ctx.current) {
  substitutions = *ctx.current;
  context.current = &substitutions;
}

FmtContextScope::~FmtContextScope() {
  context.current = old;
}

std::pair<FmtReplacement, StringRef>
FmtObjectBase::splitFmtSegment(StringRef fmt) {
  size_t begin = fmt.find_first_of('$');
  if (begin == StringRef::npos) {
    // No placeholders: the whole format string should be returned as a
    // literal string.
    return {FmtReplacement{fmt}, StringRef()};
  }
  if (begin != 0) {
    // The first placeholder is not at the beginning: we can split the format
    // string into a literal string and the rest.
    return {FmtReplacement{fmt.substr(0, begin)}, fmt.substr(begin)};
  }

  // The first placeholder is at the beginning

  if (fmt.size() == 1) {
    // The whole format string just contains '$': treat as literal.
    return {FmtReplacement{fmt}, StringRef()};
  }

  // Allow escaping dollar with '$$'
  if (fmt[1] == '$') {
    return {FmtReplacement{fmt.substr(0, 1)}, fmt.substr(2)};
  }

  // First try to see if it's a positional placeholder, and then handle special
  // placeholders.

  size_t from = 1;
  bool isIndirect = false;
  if (fmt[1] == '*') {
    isIndirect = true;
    from = 2;
  }

  size_t end = fmt.find_if_not([](char c) { return std::isdigit(c); }, from);
  if (end != 1) {
    // We have a positional placeholder. Parse the index.
    size_t index = 0;
    if (fmt.substr(from, end - from).consumeInteger(0, index)) {
      llvm_unreachable("invalid replacement sequence index");
    }

    // Check if this is the part of a range specification.
    if (!isIndirect && fmt.substr(end, 3) == "...") {
      // Currently only ranges without upper bound are supported.
      return {
          FmtReplacement{fmt.substr(0, end + 3), index,
                         FmtReplacement::Type::PositionalRangePH},
          fmt.substr(end + 3)};
    }

    auto type =
        isIndirect ? FmtReplacement::Type::IndirectSpecialPH 
                   : FmtReplacement::Type::PositionalPH;
    if (end == StringRef::npos) {
      // All the remaining characters are part of the positional placeholder.
      return {FmtReplacement{fmt, index, type}, StringRef()};
    }
    return {FmtReplacement{fmt.substr(0, end), index, type}, fmt.substr(end)};
  }

  end = fmt.find_if_not([](char c) { return std::isalnum(c) || c == '_'; }, 1);
  auto placeholder = FmtContext::getPlaceHolderKind(fmt.substr(1, end - 1));
  if (end == StringRef::npos) {
    // All the remaining characters are part of the special placeholder.
    return {FmtReplacement{fmt, placeholder}, StringRef()};
  }
  return {FmtReplacement{fmt.substr(0, end), placeholder}, fmt.substr(end)};
}

std::vector<FmtReplacement> FmtObjectBase::parseFormatString(StringRef fmt) {
  std::vector<FmtReplacement> replacements;
  FmtReplacement repl;
  while (!fmt.empty()) {
    std::tie(repl, fmt) = splitFmtSegment(fmt);
    if (repl.type != FmtReplacement::Type::Empty)
      replacements.push_back(repl);
  }
  return replacements;
}

void FmtObjectBase::format(raw_ostream &s) const {
  for (auto &repl : replacements) {
    if (repl.type == FmtReplacement::Type::Empty)
      continue;

    if (repl.type == FmtReplacement::Type::Literal) {
      s << repl.spec;
      continue;
    }

    if (repl.type == FmtReplacement::Type::SpecialPH) {
      if (repl.placeholder == FmtContext::PHKind::None) {
        s << repl.spec;
      } else if (!context) {
        // We need the context to replace special placeholders.
        s << repl.spec << kMarkerForNoSubst;
      } else {
        std::optional<StringRef> subst;
        if (repl.placeholder == FmtContext::PHKind::Custom) {
          // Skip the leading '$' sign for the custom placeholder
          subst = context->getSubstFor(repl.spec.substr(1));
        } else {
          subst = context->getSubstFor(repl.placeholder);
        }
        if (subst)
          s << *subst;
        else
          s << repl.spec << kMarkerForNoSubst;
      }
      continue;
    }

    if (repl.type == FmtReplacement::Type::PositionalRangePH) {
      if (repl.index >= adapters.size()) {
        s << repl.spec << kMarkerForNoSubst;
        continue;
      }
      auto range = ArrayRef<llvm_support_detail::format_adapter *>(adapters);
      range = range.drop_front(repl.index);
      llvm::interleaveComma(range, s,
                            [&](auto &x) { x->format(s, /*Options=*/""); });
      continue;
    }

    if (repl.index >= adapters.size()) {
      s << repl.spec << kMarkerForNoSubst;
      continue;
    }

    if (repl.type == FmtReplacement::Type::IndirectSpecialPH) {
      std::string indirectSpec;
      raw_string_ostream indirectSpecStream(indirectSpec);
      adapters[repl.index]->format(indirectSpecStream, /*Options=*/"");

      auto subst = context->getSubstFor(indirectSpec);
      if (subst.has_value())
        s << *subst;
      else
        s << repl.spec << ':' << indirectSpec << kMarkerForNoSubst;
      continue;
    }

    assert(repl.type == FmtReplacement::Type::PositionalPH);

    adapters[repl.index]->format(s, /*Options=*/"");
  }
}

FmtStrVecObject::FmtStrVecObject(StringRef fmt, const FmtContext *ctx,
                                 ArrayRef<std::string> params)
    : FmtObjectBase(fmt, ctx, params.size()) {
  parameters.reserve(params.size());
  for (std::string p : params)
    parameters.push_back(llvm_support_detail::build_format_adapter(std::move(p)));

  adapters.reserve(parameters.size());
  for (auto &p : parameters)
    adapters.push_back(&p);
}

FmtStrVecObject::FmtStrVecObject(FmtStrVecObject &&that)
    : FmtObjectBase(std::move(that)), parameters(std::move(that.parameters)) {
  adapters.reserve(parameters.size());
  for (auto &p : parameters)
    adapters.push_back(&p);
}
