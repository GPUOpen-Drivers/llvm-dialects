/*
 ***********************************************************************************************************************
 * Copyright (c) 2023 Advanced Micro Devices, Inc. All Rights Reserved.
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

#include "ExampleDialect.h"

#define GET_INCLUDES
#include "ExampleDialect.cpp.inc"

namespace xd::cpp {

const char *toString(VectorKind kind) {
  switch (kind) {
  case VectorKind::LittleEndian:
    return "LittleEndian";
  case VectorKind::BigEndian:
    return "BigEndian";
  case VectorKind::MiddleEndian:
    return "MiddleEndian";
  default:
    return "(bad)";
  }
}

llvm::raw_ostream &operator<<(llvm::raw_ostream &out, VectorKind x) {
  out << toString(x);
  return out;
}

} // namespace xd::cpp

#define GET_DIALECT_DEFS
#include "ExampleDialect.cpp.inc"
