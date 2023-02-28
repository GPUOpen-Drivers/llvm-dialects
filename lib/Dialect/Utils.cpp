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

using namespace llvm;
using namespace llvm_dialects;

bool llvm_dialects::areTypesEqual(ArrayRef<Type *> types) {
  return llvm::all_equal(types);
}

static std::string typeToStr(Type *Ty) {
  switch (Ty->getTypeID()) {
  case Type::VoidTyID:
    return "isVoid";
  case Type::MetadataTyID:
    return "Metadata";
  case Type::HalfTyID:
    return "f16";
  case Type::BFloatTyID:
    return "bf16";
  case Type::FloatTyID:
    return "f32";
  case Type::DoubleTyID:
    return "f64";
  case Type::X86_FP80TyID:
    return "f80";
  case Type::FP128TyID:
    return "f128";
  case Type::PPC_FP128TyID:
    return "ppcf128";
  case Type::X86_MMXTyID:
    return "x86mmx";
  case Type::X86_AMXTyID:
    return "x86amx";
  case Type::IntegerTyID:
    return "i" + utostr(cast<IntegerType>(Ty)->getBitWidth());
  default:
      llvm_unreachable("Unhandled type");
  }
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
    // Opaque pointer doesn't have pointee type information, so we just mangle
    // address space for opaque pointer.
    if (!PTyp->isOpaque())
      Result += getMangledTypeStr(PTyp->getNonOpaquePointerElementType(),
                                  HasUnnamedType);
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
  } else if (Ty) {
    Result += typeToStr(Ty);
  }
  return Result;
}

std::string llvm_dialects::getMangledName(StringRef name,
                                          ArrayRef<Type *> overloadTypes) {
  std::string result = name.str();
  for (Type *ty : overloadTypes) {
    result += '.';

    bool hasUnnamedType = false;
    result += getMangledTypeStr(ty, hasUnnamedType);
    assert(!hasUnnamedType);
  }
  return result;
}
