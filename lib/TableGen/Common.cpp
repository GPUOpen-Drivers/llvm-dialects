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

#include "llvm-dialects/TableGen/Common.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

#include "llvm/Support/CommandLine.h"

#include <regex>
#include <string>

using namespace llvm_dialects;
using namespace llvm;

static cl::opt<bool>
    g_emitComments("emit-comments",
                   cl::desc("emit debug comments in generated source files"),
                   cl::Hidden, cl::init(false));

void llvm_dialects::emitHeader(raw_ostream& out) {
  out << "// DO NOT EDIT! This file is automatically generated by llvm-dialects-tblgen.\n\n";
}

bool llvm_dialects::shouldEmitComments() { return g_emitComments; }

std::string llvm_dialects::replaceSubstring(const std::string &haystack,
                                            const std::string &needle,
                                            const std::string &replacement) {
  return std::regex_replace(haystack, std::regex(needle), replacement);
}

std::string llvm_dialects::createCommentFromString(const std::string &input) {
  const StringRef inRef{input};
  if (inRef.trim().empty())
    return input;

  SmallVector<StringRef> lines;
  inRef.split(lines, '\n');

  std::string outStr;
  for (auto line : lines)
    outStr += "/// " + line.str() + '\n';

  return outStr;
}
