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

#include "llvm-dialects/TableGen/GenDialect.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/TableGen/Main.h"
#include "llvm/TableGen/Record.h"

#include <unordered_map>

using namespace llvm_dialects;
using namespace llvm;

namespace llvm {
cl::opt<bool> EmitLongStrLiterals(
    "long-string-literals",
    cl::desc("when emitting large string tables, prefer string literals over "
             "comma-separated char literals. This can be a readability and "
             "compile-time performance win, but upsets some compilers"),
    cl::Hidden, cl::init(true));
} // end namespace llvm

namespace {

enum class Action {
  PrintRecords,
  GenDialectDecls,
  GenDialectDefs,
};

cl::opt<Action> g_action(
    cl::desc("Action to perform:"),
    cl::values(
        clEnumValN(Action::PrintRecords, "print-records", "Print all records to stdout (default)"),
        clEnumValN(Action::GenDialectDecls, "gen-dialect-decls",
                   "Generate dialect declarations (.h.inc)"),
        clEnumValN(Action::GenDialectDefs, "gen-dialect-defs",
                   "Generate dialect definitions (.cpp.inc)")
        ));

bool llvmDialectsTableGenMain(raw_ostream& out, RecordKeeper& records) {
  switch (g_action) {
  case Action::PrintRecords:
    // Redundant with llvm-tblgen, but may be convenient for users.
    out << records;
    break;
  case Action::GenDialectDecls:
    genDialectDecls(out, records);
    break;
  case Action::GenDialectDefs:
    genDialectDefs(out, records);
    break;
  }

  return false;
}

} // anonymous namespace

int main(int argc, char** argv) {
  InitLLVM X(argc, argv);
  cl::ParseCommandLineOptions(argc, argv);
  return TableGenMain(argv[0], &llvmDialectsTableGenMain);
}
