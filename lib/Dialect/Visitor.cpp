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

#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Debug.h"

#define DEBUG_TYPE "llvm-dialects-visitor"

using namespace llvm_dialects;
using namespace llvm;

using llvm_dialects::detail::VisitorBase;
using llvm_dialects::detail::VisitorBuilderBase;
using llvm_dialects::detail::VisitorCallbackData;
using llvm_dialects::detail::VisitorHandler;
using llvm_dialects::detail::VisitorKey;
using llvm_dialects::detail::VisitorTemplate;

void VisitorTemplate::setStrategy(VisitorStrategy strategy) {
  if (strategy == VisitorStrategy::Default)
    return;

  assert(m_strategy == VisitorStrategy::Default || m_strategy == strategy);

  m_strategy = strategy;
}

void VisitorTemplate::add(VisitorKey key, VisitorCallback *fn,
                          VisitorCallbackData data,
                          VisitorHandler::Projection projection) {
  VisitorHandler handler;
  handler.callback = fn;
  handler.data = data;
  handler.projection = projection;
  m_handlers.emplace_back(handler);

  unsigned handlerIdx = m_handlers.size() - 1;

  if (key.m_kind == VisitorKey::Kind::Intrinsic) {
    m_intrinsicIdMap[key.m_intrinsicId].push_back(handlerIdx);
  } else {
    const OpDescription *description = key.m_description;
    switch (description->getKind()) {
    case OpDescription::Kind::Core:
      for (unsigned opcode : description->getOpcodes())
        m_coreOpcodeMap[opcode].push_back(handlerIdx);
      break;
    case OpDescription::Kind::Intrinsic:
      for (unsigned id : description->getOpcodes())
        m_intrinsicIdMap[id].push_back(handlerIdx);
      break;
    default: {
      auto it = find_if(m_dialectCases, [=](const auto &theCase) {
        return theCase.first == description;
      });
      if (it != m_dialectCases.end()) {
        it->second.push_back(handlerIdx);
      } else {
        SmallVector<unsigned> handlers;
        handlers.push_back(handlerIdx);
        m_dialectCases.emplace_back(description, std::move(handlers));
      }
      break;
    }
    }
  }
}

VisitorBuilderBase::VisitorBuilderBase() : m_template(&m_ownedTemplate) {}

VisitorBuilderBase::VisitorBuilderBase(VisitorBuilderBase *parent,
                                       PayloadProjectionCallback *projection)
    : m_template(parent->m_template) {
  // Extract and update the parent's projection.
  SmallVector<PayloadProjection> sequence;

  if (parent->m_projection.isOffset()) {
    sequence.emplace_back();
    sequence.back().offset = parent->m_projection.getOffset();
  } else {
    for (size_t idx = parent->m_projection.getIndex();; ++idx) {
      sequence.push_back(m_template->m_projections[idx]);
      if (!sequence.back().projection)
        break;
    }
  }

  sequence.back().projection = projection;
  sequence.emplace_back();

  // Setup our projection.
  m_projection.setIndex(m_template->m_projections.size());
  m_template->m_projections.insert(m_template->m_projections.end(),
                                   sequence.begin(), sequence.end());
}

VisitorBuilderBase::VisitorBuilderBase(VisitorBuilderBase *parent,
                                       size_t offset)
    : m_template(parent->m_template) {
  if (offset == 0) {
    m_projection = parent->m_projection;
  } else if (parent->m_projection.isOffset()) {
    m_projection.setOffset(parent->m_projection.getOffset() + offset);
  } else {
    // Extract and update the parent's projection.
    SmallVector<PayloadProjection> sequence;
    for (size_t idx = parent->m_projection.getIndex();; ++idx) {
      sequence.push_back(m_template->m_projections[idx]);
      if (!sequence.back().projection)
        break;
    }
    sequence.back().offset += offset;

    // Setup our projection.
    m_projection.setIndex(m_template->m_projections.size());
    m_template->m_projections.insert(m_template->m_projections.end(),
                                     sequence.begin(), sequence.end());
  }
}

void VisitorBuilderBase::setStrategy(VisitorStrategy strategy) {
  m_template->setStrategy(strategy);
}

void VisitorBuilderBase::add(VisitorKey key, VisitorCallback *fn,
                             VisitorCallbackData data) {
  m_template->add(key, fn, data, m_projection);
}

VisitorBase VisitorBuilderBase::build() {
  assert(m_template == &m_ownedTemplate);
  return std::move(m_ownedTemplate);
}

class VisitorBase::BuildHelper {
public:
  BuildHelper(VisitorBase &self, ArrayRef<VisitorHandler> srcHandlers)
      : m_srcHandlers(srcHandlers), m_dstHandlers(self.m_handlers) {}

  HandlerRange mapHandlers(ArrayRef<unsigned> handlers) {
    auto it = m_handlerMap.find(handlers);
    if (it != m_handlerMap.end())
      return it->second;

    HandlerRange range;
    range.first = m_dstHandlers.size();
    range.second = range.first + handlers.size();

    for (unsigned srcIdx : handlers)
      m_dstHandlers.push_back(m_srcHandlers[srcIdx]);

    m_handlerMap.try_emplace(handlers, range);
    return range;
  }

private:
  ArrayRef<VisitorHandler> m_srcHandlers;
  std::vector<VisitorHandler> &m_dstHandlers;
  DenseMap<ArrayRef<unsigned>, HandlerRange> m_handlerMap;
};

VisitorBase::VisitorBase(VisitorTemplate &&templ)
    : m_strategy(templ.m_strategy),
      m_projections(std::move(templ.m_projections)) {
  if (m_strategy == VisitorStrategy::Default) {
    m_strategy = templ.m_coreOpcodeMap.empty()
                     ? VisitorStrategy::ByFunctionDeclaration
                     : VisitorStrategy::ByInstruction;
  }

  BuildHelper helper(*this, templ.m_handlers);

  m_coreOpcodeMap.reserve(templ.m_coreOpcodeMap.size());
  m_intrinsicIdMap.reserve(templ.m_intrinsicIdMap.size());
  m_dialectCases.reserve(templ.m_dialectCases.size());

  for (const auto &entry : templ.m_coreOpcodeMap) {
    m_coreOpcodeMap.try_emplace(entry.first, helper.mapHandlers(entry.second));
  }
  for (const auto &entry : templ.m_intrinsicIdMap) {
    m_intrinsicIdMap.try_emplace(entry.first, helper.mapHandlers(entry.second));
  }
  for (const auto &entry : templ.m_dialectCases) {
    m_dialectCases.emplace_back(entry.first, helper.mapHandlers(entry.second));
  }
}

void VisitorBase::call(HandlerRange handlers, void *payload,
                       Instruction &inst) const {
  for (unsigned idx = handlers.first; idx != handlers.second; ++idx)
    call(m_handlers[idx], payload, inst);
}

void VisitorBase::call(const VisitorHandler &handler, void *payload,
                       Instruction &inst) const {
  if (handler.projection.isOffset()) {
    payload = (char *)payload + handler.projection.getOffset();
  } else {
    for (size_t idx = handler.projection.getIndex();; ++idx) {
      payload = (char *)payload + m_projections[idx].offset;
      if (!m_projections[idx].projection)
        break;
      payload = m_projections[idx].projection(payload);
    }
  }
  handler.callback(handler.data, payload, &inst);
}

void VisitorBase::visit(void *payload, Instruction &inst) const {
  if (auto *callInst = dyn_cast<CallInst>(&inst)) {
    // Note: Always fall through to case handlers installed for generic
    // CallInst instructions, if there are any.
    if (auto *intrinsicInst = dyn_cast<IntrinsicInst>(callInst)) {
      auto it = m_intrinsicIdMap.find(intrinsicInst->getIntrinsicID());
      if (it != m_intrinsicIdMap.end())
        call(it->second, payload, inst);
    } else {
      for (const auto &theCase : m_dialectCases) {
        if (theCase.first->matchInstruction(inst)) {
          call(theCase.second, payload, inst);
          break;
        }
      }
    }
  }

  auto it = m_coreOpcodeMap.find(inst.getOpcode());
  if (it != m_coreOpcodeMap.end())
    call(it->second, payload, inst);
}

template <typename FilterT>
void VisitorBase::visitByDeclarations(void *payload, llvm::Module &module,
                                      FilterT &&filter) const {
  assert(m_strategy == VisitorStrategy::ByFunctionDeclaration);

  for (Function &decl : module.functions()) {
    if (!decl.isDeclaration())
      continue;

    LLVM_DEBUG(dbgs() << "visit " << decl.getName() << '\n');

    HandlerRange handlers{0, 0};
    if (unsigned intrinsicId = decl.getIntrinsicID()) {
      auto it = m_intrinsicIdMap.find(intrinsicId);
      if (it == m_intrinsicIdMap.end()) {
        // Can't be a dialect op, so skip this declaration entirely.
        continue;
      }
      handlers = it->second;
    }

    if (handlers.second == 0) {
      for (const auto &theCase : m_dialectCases) {
        if (theCase.first->matchDeclaration(decl)) {
          handlers = theCase.second;
          break;
        }
      }
    }

    if (handlers.second == 0) {
      // Neither a matched intrinsic nor a matched dialect op; skip.
      continue;
    }

    for (Use &use : decl.uses()) {
      if (auto *inst = dyn_cast<Instruction>(use.getUser())) {
        if (!filter(*inst))
          continue;
        if (auto *callInst = dyn_cast<CallInst>(inst)) {
          if (&use == &callInst->getCalledOperandUse())
            call(handlers, payload, *callInst);
        }
      }
    }
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

  if (m_strategy == VisitorStrategy::ReversePostOrder) {
    ReversePostOrderTraversal<Function *> rpot(&fn);
    for (BasicBlock *bb : rpot) {
      for (Instruction &inst : *bb)
        visit(payload, inst);
    }
    return;
  }

  visitByDeclarations(payload, *fn.getParent(), [&fn](Instruction &inst) {
    return inst.getFunction() == &fn;
  });
}

void VisitorBase::visit(void *payload, Module &module) const {
  if (m_strategy != VisitorStrategy::ByFunctionDeclaration) {
    for (Function &fn : module.functions()) {
      if (!fn.isDeclaration())
        visit(payload, fn);
    }
    return;
  }

  visitByDeclarations(payload, module, [](Instruction &inst) { return true; });
}
