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

#include "llvm-dialects/Dialect/Visitor.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Debug.h"

#define DEBUG_TYPE "llvm-dialects-visitor"

using namespace llvm_dialects;
using namespace llvm;

using llvm_dialects::detail::VisitorBuilderBase;
using llvm_dialects::detail::VisitorBase;

void VisitorBuilderBase::add(const OpDescription &desc, void *extra, VisitorCallback *fn) {
  m_cases.emplace_back(&desc, extra, fn);
}

VisitorBase::VisitorBase(VisitorBuilderBase builder)
    : m_strategy(builder.m_strategy), m_cases(std::move(builder.m_cases)) {
}

void VisitorBase::visit(void *payload, Function &fn) const {
  if (m_strategy == VisitorStrategy::ByInstruction) {
    for (BasicBlock &bb : fn) {
      for (Instruction &inst : bb) {
        for (const auto &[desc, extra, callback] : m_cases) {
          if (desc->matchInstruction(inst))
            callback(extra, payload, &inst);
        }
      }
    }
    return;
  }

  for (Function &decl : fn.getParent()->functions()) {
    if (!decl.isDeclaration())
      continue;

    LLVM_DEBUG(dbgs() << "visit " << decl.getName() << '\n');

    for (const auto &[desc, extra, callback] : m_cases) {
      if (desc->matchDeclaration(decl)) {
        for (Use &use : decl.uses()) {
          if (auto *inst = dyn_cast<Instruction>(use.getUser())) {
            if (inst->getFunction() != &fn)
              continue;
            if (auto *call = dyn_cast<CallInst>(inst)) {
              if (&use == &call->getCalledOperandUse())
                callback(extra, payload, call);
            }
          }
        }
      }
    }
  }
}

void VisitorBase::visit(void *payload, Module &module) const {
  if (m_strategy == VisitorStrategy::ByInstruction) {
    for (Function &fn : module.functions()) {
      if (!fn.isDeclaration())
        visit(payload, fn);
    }
    return;
  }

  for (Function &decl : module.functions()) {
    if (!decl.isDeclaration())
      continue;

    for (const auto &[desc, extra, callback] : m_cases) {
      if (desc->matchDeclaration(decl)) {
        for (Use &use : decl.uses()) {
          if (auto *call = dyn_cast<CallInst>(use.getUser())) {
            if (&use == &call->getCalledOperandUse())
              callback(extra, payload, call);
          }
        }
      }
    }
  }
}
