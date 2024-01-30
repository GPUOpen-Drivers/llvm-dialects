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

#include "llvm-dialects/Dialect/Utils.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llvm_dialects;

bool llvm_dialects::areTypesEqual(ArrayRef<Type *> types) {
#if HAVE_LLVM_VERSION_MAJOR >= 16
  return llvm::all_equal(types);
#else
  return llvm::is_splat(types);
#endif
}

// The following function is copied verbatim from
// llvm-project/llvm/lib/IR/Function.cpp
//
// LLVM is under the Apache-2.0 license with LLVM exception.
//
/// Returns a stable mangling for the type specified for use in the name
/// mangling scheme used by 'any' types in intrinsic signatures.  The mangling
/// of named types is simply their name.  Manglings for unnamed types consist
/// of a prefix ('p' for pointers, 'a' for arrays, 'f_' for functions)
/// combined with the mangling of their component types.  A vararg function
/// type will have a suffix of 'vararg'.  Since function types can contain
/// other function types, we close a function type mangling with suffix 'f'
/// which can't be confused with it's prefix.  This ensures we don't have
/// collisions between two unrelated function types. Otherwise, you might
/// parse ffXX as f(fXX) or f(fX)X.  (X is a placeholder for any other type.)
/// The HasUnnamedType boolean is set if an unnamed type was encountered,
/// indicating that extra care must be taken to ensure a unique name.
static std::string getMangledTypeStr(Type *Ty, bool &HasUnnamedType) {
  std::string Result;
  if (PointerType *PTyp = dyn_cast<PointerType>(Ty)) {
    Result += "p" + utostr(PTyp->getAddressSpace());
#if HAVE_LLVM_VERSION_MAJOR <= 17
    // Opaque pointer doesn't have pointee type information, so we just mangle
    // address space for opaque pointer.
    // After llvm-18+ opaque pointer API will be removed from LLVM.
    if (!PTyp->isOpaque())
      Result += getMangledTypeStr(PTyp->getNonOpaquePointerElementType(),
                                  HasUnnamedType);
#endif
  } else if (ArrayType *ATyp = dyn_cast<ArrayType>(Ty)) {
    Result += "a" + utostr(ATyp->getNumElements()) +
              getMangledTypeStr(ATyp->getElementType(), HasUnnamedType);
  } else if (StructType *STyp = dyn_cast<StructType>(Ty)) {
    if (!STyp->isLiteral()) {
      Result += "s_";
      if (STyp->hasName())
        Result += STyp->getName();
      else
        HasUnnamedType = true;
    } else {
      Result += "sl_";
      for (auto Elem : STyp->elements())
        Result += getMangledTypeStr(Elem, HasUnnamedType);
    }
    // Ensure nested structs are distinguishable.
    Result += "s";
  } else if (FunctionType *FT = dyn_cast<FunctionType>(Ty)) {
    Result += "f_" + getMangledTypeStr(FT->getReturnType(), HasUnnamedType);
    for (size_t i = 0; i < FT->getNumParams(); i++)
      Result += getMangledTypeStr(FT->getParamType(i), HasUnnamedType);
    if (FT->isVarArg())
      Result += "vararg";
    // Ensure nested function types are distinguishable.
    Result += "f";
  } else if (VectorType *VTy = dyn_cast<VectorType>(Ty)) {
    ElementCount EC = VTy->getElementCount();
    if (EC.isScalable())
      Result += "nx";
    Result += "v" + utostr(EC.getKnownMinValue()) +
              getMangledTypeStr(VTy->getElementType(), HasUnnamedType);
#if HAVE_LLVM_VERSION_MAJOR >= 17
  } else if (TargetExtType *TETy = dyn_cast<TargetExtType>(Ty)) {
    Result += "t";
    Result += TETy->getName();
    for (Type *ParamTy : TETy->type_params())
      Result += "_" + getMangledTypeStr(ParamTy, HasUnnamedType);
    for (unsigned IntParam : TETy->int_params())
      Result += "_" + utostr(IntParam);
    // Ensure nested target extension types are distinguishable.
    Result += "t";
#endif
  } else if (Ty) {
    switch (Ty->getTypeID()) {
    default: llvm_unreachable("Unhandled type");
    case Type::VoidTyID:      Result += "isVoid";   break;
    case Type::MetadataTyID:  Result += "Metadata"; break;
    case Type::HalfTyID:      Result += "f16";      break;
    case Type::BFloatTyID:    Result += "bf16";     break;
    case Type::FloatTyID:     Result += "f32";      break;
    case Type::DoubleTyID:    Result += "f64";      break;
    case Type::X86_FP80TyID:  Result += "f80";      break;
    case Type::FP128TyID:     Result += "f128";     break;
    case Type::PPC_FP128TyID: Result += "ppcf128";  break;
    case Type::X86_MMXTyID:   Result += "x86mmx";   break;
    case Type::X86_AMXTyID:   Result += "x86amx";   break;
    case Type::IntegerTyID:
      Result += "i" + utostr(cast<IntegerType>(Ty)->getBitWidth());
      break;
    }
  }
  return Result;
}

std::string llvm_dialects::getMangledName(StringRef name,
                                          ArrayRef<Type *> overloadTypes) {
  std::string result = name.str();
  for (Type *ty : overloadTypes) {
    result += "__";

    bool hasUnnamedType = false;
    result += getMangledTypeStr(ty, hasUnnamedType);
  }
  return result;
}

namespace {

template <typename T>
bool runVerifier(function_ref<bool(raw_ostream &)> verifier, T *ir,
                 raw_ostream *errs) {
  std::string errors;
  raw_string_ostream out(errors);

  if (verifier(out))
    return true;

  if (!errs)
    errs = &llvm::errs();

  *errs << "Verifier error in: " << *ir << ":\n" << errors;
  return false;
}

} // anonymous namespace

bool llvm_dialects::runInstructionVerifier(
    function_ref<bool(raw_ostream &)> verifier, llvm::Instruction *instruction,
    raw_ostream *errs) {
  return runVerifier(verifier, instruction, errs);
}

bool llvm_dialects::runTypeVerifier(function_ref<bool(raw_ostream &)> verifier,
                                    llvm::Type *type, raw_ostream *errs) {
  return runVerifier(verifier, type, errs);
}
