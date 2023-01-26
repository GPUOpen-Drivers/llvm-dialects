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

#include "llvm/TableGen/Record.h"

using namespace llvm;
using namespace llvm_dialects;

void Trait::init(GenDialectsContext *context, Record *record) {
  m_record = record;
}

StringRef Trait::getName() const { return m_record->getName(); }

void LlvmAttributeTrait::init(GenDialectsContext *context, Record *record) {
  Trait::init(context, record);
  m_llvmEnum = record->getValueAsString("llvmEnum");
}
