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

VisitorBuilderBase::~VisitorBuilderBase() {
  if (m_parent) {
    // Build up the projection sequence by walking all the way to the root.
    SmallVector<PayloadProjection> projectionSequence;
    projectionSequence.emplace_back();

    VisitorBuilderBase *ancestor = this;
    for (; ancestor->m_parent; ancestor = ancestor->m_parent) {
      if (ancestor->m_projection) {
        PayloadProjection next;
        next.projection = ancestor->m_projection;
        projectionSequence.push_back(next);
      } else {
        projectionSequence.back().offset += ancestor->m_offsetProjection;
      }
    }

    ssize_t projection = 0;
    if (projectionSequence.size() == 1) {
      projection = projectionSequence[0].offset;
    } else {
      projection = -(1 + ancestor->m_projections.size());
      ancestor->m_projections.append(projectionSequence.rbegin(),
                                     projectionSequence.rend());
    }

    for (auto &theCase : m_cases)
      theCase.projection = projection;

    // Copy all cases to the root.
    ancestor->m_cases.append(m_cases.begin(), m_cases.end());
  }
}

void VisitorBuilderBase::add(const OpDescription &desc, void *extra, VisitorCallback *fn) {
  VisitorCase theCase;
  theCase.description = &desc;
  theCase.callback = fn;
  theCase.callbackData = extra;
  theCase.projection = 0;
  m_cases.emplace_back(theCase);
}

VisitorBase::VisitorBase(VisitorBuilderBase &&builder)
    : m_strategy(builder.m_strategy), m_cases(std::move(builder.m_cases)),
      m_projections(std::move(builder.m_projections)) {
  assert(!builder.m_parent);
}

void VisitorBase::call(const VisitorCase &theCase, void *payload,
                       Instruction &inst) const {
  if (theCase.projection >= 0) {
    payload = (char *)payload + theCase.projection;
  } else {
    for (size_t idx = -theCase.projection - 1;; ++idx) {
      payload = (char *)payload + m_projections[idx].offset;
      if (!m_projections[idx].projection)
        break;
      payload = m_projections[idx].projection(payload);
    }
  }
  theCase.callback(theCase.callbackData, payload, &inst);
}

void VisitorBase::visit(void *payload, Instruction &inst) const {
  for (const auto &theCase : m_cases) {
    if (theCase.description->matchInstruction(inst))
      call(theCase, payload, inst);
  }
}

void VisitorBase::visit(void *payload, Function &fn) const {
  if (m_strategy == VisitorStrategy::ByInstruction) {
    for (BasicBlock &bb : fn) {
      for (Instruction &inst : bb)
        visit(payload, inst);
    }
    return;
  }

  for (Function &decl : fn.getParent()->functions()) {
    if (!decl.isDeclaration())
      continue;

    LLVM_DEBUG(dbgs() << "visit " << decl.getName() << '\n');

    for (const auto &theCase : m_cases) {
      if (theCase.description->matchDeclaration(decl)) {
        for (Use &use : decl.uses()) {
          if (auto *inst = dyn_cast<Instruction>(use.getUser())) {
            if (inst->getFunction() != &fn)
              continue;
            if (auto *callInst = dyn_cast<CallInst>(inst)) {
              if (&use == &callInst->getCalledOperandUse())
                call(theCase, payload, *callInst);
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

    for (const auto &theCase : m_cases) {
      if (theCase.description->matchDeclaration(decl)) {
        for (Use &use : decl.uses()) {
          if (auto *callInst = dyn_cast<CallInst>(use.getUser())) {
            if (&use == &callInst->getCalledOperandUse())
              call(theCase, payload, *callInst);
          }
        }
      }
    }
  }
}
