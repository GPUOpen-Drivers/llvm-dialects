/*
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
 */

#include "llvm-dialects/Dialect/Verifier.h"

#include "llvm-dialects/Dialect/ContextExtension.h"

using namespace llvm;
using namespace llvm_dialects;

namespace {

struct VerifierContextExtension
    : public ContextExtensionImpl<VerifierContextExtension> {
  static Key theKey;

  VerifierContextExtension(LLVMContext &context) : visitor(build(context)) {}

  static Visitor<VerifierState> build(LLVMContext &context) {
    VisitorBuilder<VerifierState> builder;
    auto extensions = getVerifierExtensionPoint().getExtensions(context);
    for (const VerifierExtension *extension : extensions)
      (*extension->build)(builder);
    return builder.build();
  }

  Visitor<VerifierState> visitor;
};

VerifierContextExtension::Key VerifierContextExtension::theKey;

} // anonymous namespace

DialectExtensionPoint<VerifierExtension> &
llvm_dialects::getVerifierExtensionPoint() {
  static DialectExtensionPoint<VerifierExtension> point;
  return point;
}

bool llvm_dialects::verify(Module &module, raw_ostream &out) {
  const auto &extension = VerifierContextExtension::get(module.getContext());

  std::string outstr;
  llvm::raw_string_ostream outstream{outstr};
  bool ok = true;

  for (Function &fn : module.functions()) {
    for (BasicBlock &bb : fn) {
      for (Instruction &inst : bb) {
        VerifierState state{outstream};
        extension.visitor.visit(state, inst);
        if (state.hasError()) {
          out << "Verifier error in: " << inst << '\n' << outstr;
          ok = false;
        }
        assert(outstr.empty() == !state.hasError());
        outstr.clear();
      }
    }
  }
  return ok;
}

PreservedAnalyses
VerifyModulePass::run(Module &module, ModuleAnalysisManager &analysisManager) {
  if (!verify(module, llvm::errs()))
    report_fatal_error("llvm_dialects IR verification failed");
  return PreservedAnalyses::all();
}
