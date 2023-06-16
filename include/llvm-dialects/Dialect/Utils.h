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
#include "llvm/ADT/STLFunctionalExtras.h"
#include "llvm/ADT/iterator.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/User.h"
#include "llvm/Support/raw_ostream.h"

namespace llvm {
class Instruction;
class Type;
} // namespace llvm

namespace llvm_dialects {

/// Returns true if the given types are all equal. See the SameTypes predicate
/// in TableGen.
bool areTypesEqual(llvm::ArrayRef<llvm::Type *> types);

std::string getMangledName(llvm::StringRef name,
                           llvm::ArrayRef<llvm::Type *> overloadTypes);

bool runInstructionVerifier(
    llvm::function_ref<bool (llvm::raw_ostream &)> verifier,
    llvm::Instruction *instruction,
    llvm::raw_ostream *errs = nullptr);
bool runTypeVerifier(llvm::function_ref<bool(llvm::raw_ostream &)> verifier,
                     llvm::Type *type, llvm::raw_ostream *errs = nullptr);

/// Wrap a value to be printed to an llvm::raw_ostream.
///
/// Specialize this struct if you use AttrEnum arguments in operations or
/// types.
template <typename T> struct Printable {
  T x;

  explicit Printable(const T &x) : x(x) {}

  void print(llvm::raw_ostream &out) const {
    if constexpr (std::is_pointer_v<T>) {
      out << *x;
    } else {
      out << x;
    }
  }
};

template <typename T>
llvm::raw_ostream &operator<<(llvm::raw_ostream &out, const Printable<T> &p) {
  p.print(out);
  return out;
}

/// Wrap a value to be printed to an llvm::raw_ostream.
///
/// This is intended to be used to print constraint system values. All it
/// really does is use C++ overload set trickery to solve the problem of
/// printing type values (which are of C++ type `llvm::Type *`) in a useful way
/// instead of printing out the pointer value.
template <typename T> Printable<T> printable(const T &x) {
  return Printable<T>(x);
}

} // namespace llvm_dialects
