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

#pragma once

#include "llvm-dialects/Dialect/Dialect.h"
#include "llvm-dialects/Dialect/Visitor.h"

#include "llvm/IR/PassManager.h"

namespace llvm {
class Module;
class raw_ostream;
} // namespace llvm

namespace llvm_dialects {

class VerifierState {
public:
  VerifierState(llvm::raw_ostream &out) : m_out(out) {}

  bool hasError() const { return m_error; }
  llvm::raw_ostream &out() { return m_out; }
  void setError() { m_error = true; }

private:
  llvm::raw_ostream &m_out;
  bool m_error = false;
};

struct VerifierExtension {
  void (*build)(VisitorBuilder<VerifierState> &) = nullptr;
};
DialectExtensionPoint<VerifierExtension> &getVerifierExtensionPoint();

bool verify(llvm::Module &module, llvm::raw_ostream &out);

class VerifyModulePass : public llvm::PassInfoMixin<VerifyModulePass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &module,
                              llvm::ModuleAnalysisManager &analysisManager);
};

} // namespace llvm_dialects
