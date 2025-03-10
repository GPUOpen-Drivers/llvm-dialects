/*
 ***********************************************************************************************************************
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
 ***********************************************************************************************************************
 */

#pragma once

#include "llvm/Support/raw_ostream.h"
#include "llvm/TableGen/Record.h"
#include <string>

#if !defined(LLVM_MAIN_REVISION) || LLVM_MAIN_REVISION >= 513628
using RecordKeeperTy = const llvm::RecordKeeper;
using RecordTy = const llvm::Record;
#else
using RecordKeeperTy = llvm::RecordKeeper;
using RecordTy = llvm::Record;
#endif

namespace llvm_dialects {

void emitHeader(llvm::raw_ostream& out);

bool shouldEmitComments();

/// Replace all occurrences of needle in haystack with replacement and return
/// the new string.
std::string replaceSubstring(const std::string &haystack,
                             const std::string &needle,
                             const std::string &replacement = "");

/// Prefix an incoming multi-line string with a single-line comment string line
/// by line.
std::string createCommentFromString(const std::string &input);

} // namespace llvm_dialects
