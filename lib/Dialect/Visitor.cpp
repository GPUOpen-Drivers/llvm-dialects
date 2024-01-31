/*
 * Copyright (c) 2022-2024 Advanced Micro Devices, Inc. All Rights Reserved.
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

void VisitorTemplate::storeHandlersInOpMap(
    const VisitorKey &key, unsigned handlerIdx,
    VisitorCallbackType visitorCallbackTy) {
  const auto HandlerList =
      [&](const OpDescription &opDescription) -> llvm::SmallVector<unsigned> & {
    if (visitorCallbackTy == VisitorCallbackType::PreVisit)
      return m_opMap[opDescription].PreVisitHandlers;

    return m_opMap[opDescription].VisitHandlers;
  };

  if (key.m_kind == VisitorKey::Kind::Intrinsic) {
    HandlerList(OpDescription::fromIntrinsic(key.m_intrinsicId))
        .push_back(handlerIdx);
  } else if (key.m_kind == VisitorKey::Kind::OpDescription) {
    const OpDescription *opDesc = key.m_description;

    if (opDesc->isCoreOp()) {
      for (const unsigned op : opDesc->getOpcodes())
        HandlerList(OpDescription::fromCoreOp(op)).push_back(handlerIdx);
    } else if (opDesc->isIntrinsic()) {
      for (const unsigned op : opDesc->getOpcodes())
        HandlerList(OpDescription::fromIntrinsic(op)).push_back(handlerIdx);
    } else {
      HandlerList(*opDesc).push_back(handlerIdx);
    }
  } else if (key.m_kind == VisitorKey::Kind::OpSet) {
    const OpSet *opSet = key.m_set;

    if (visitorCallbackTy == VisitorCallbackType::PreVisit && opSet->empty()) {
      // This adds a handler for every stored op.
      // Note: should be used with caution.
      for (auto it : m_opMap)
        it.second.PreVisitHandlers.push_back(handlerIdx);

      return;
    }

    for (unsigned opcode : opSet->getCoreOpcodes())
      HandlerList(OpDescription::fromCoreOp(opcode)).push_back(handlerIdx);

    for (unsigned intrinsicID : opSet->getIntrinsicIDs())
      HandlerList(OpDescription::fromIntrinsic(intrinsicID))
          .push_back(handlerIdx);

    for (const auto &dialectOpPair : opSet->getDialectOps())
      HandlerList(OpDescription::fromDialectOp(dialectOpPair.isOverload,
                                               dialectOpPair.mnemonic))
          .push_back(handlerIdx);
  }
}

void VisitorTemplate::add(VisitorKey key, VisitorCallback *fn,
                          VisitorCallbackData data,
                          VisitorHandler::Projection projection,
                          VisitorCallbackType visitorCallbackTy) {
  assert(visitorCallbackTy != VisitorCallbackType::PreVisit || key.m_set);

  VisitorHandler handler;
  handler.callback = fn;
  handler.data = data;
  handler.projection = projection;

  m_handlers.emplace_back(handler);

  const unsigned handlerIdx = m_handlers.size() - 1;

  storeHandlersInOpMap(key, handlerIdx, visitorCallbackTy);
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

void VisitorBuilderBase::addPreVisitCallback(VisitorKey key,
                                             VisitorCallback *fn,
                                             VisitorCallbackData data) {
  m_template->add(key, fn, data, m_projection,
                  VisitorTemplate::VisitorCallbackType::PreVisit);
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
    m_strategy = templ.m_opMap.empty() ? VisitorStrategy::ByFunctionDeclaration
                                       : VisitorStrategy::ByInstruction;
  }

  BuildHelper helper(*this, templ.m_handlers);

  m_opMap.reserve(templ.m_opMap);
  for (auto it : templ.m_opMap) {
    m_opMap[it.first].PreVisitCallbacks =
        helper.mapHandlers(it.second.PreVisitHandlers);
    m_opMap[it.first].VisitCallbacks =
        helper.mapHandlers(it.second.VisitHandlers);
  }
}

void VisitorBase::call(HandlerRange handlers, void *payload,
                       Instruction &inst) const {
  for (unsigned idx = handlers.first; idx != handlers.second; ++idx) {
    VisitorResult result = call(m_handlers[idx], payload, inst);
    if (result == VisitorResult::Stop)
      return;
  }
}

VisitorResult VisitorBase::call(const VisitorHandler &handler, void *payload,
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

  return handler.callback(handler.data, payload, &inst);
}

void VisitorBase::visit(void *payload, Instruction &inst) const {
  auto mappedHandlers = m_opMap.find(inst);
  if (!mappedHandlers)
    return;

  auto &callbacks = *mappedHandlers.val();

  call(callbacks.PreVisitCallbacks, payload, inst);
  call(callbacks.VisitCallbacks, payload, inst);
}

template <typename FilterT>
void VisitorBase::visitByDeclarations(void *payload, llvm::Module &module,
                                      FilterT &&filter) const {
  assert(m_strategy == VisitorStrategy::ByFunctionDeclaration);

  for (Function &decl : module.functions()) {
    if (!decl.isDeclaration())
      continue;

    LLVM_DEBUG(dbgs() << "visit " << decl.getName() << '\n');

    auto mappedHandlers = m_opMap.find(decl);
    if (!mappedHandlers) {
      // Neither a matched intrinsic nor a matched dialect op; skip.
      continue;
    }

    auto &callbacks = *mappedHandlers.val();

    for (Use &use : make_early_inc_range(decl.uses())) {
      if (auto *inst = dyn_cast<Instruction>(use.getUser())) {
        if (!filter(*inst))
          continue;
        if (auto *callInst = dyn_cast<CallInst>(inst)) {
          if (&use == &callInst->getCalledOperandUse()) {
            call(callbacks.PreVisitCallbacks, payload, *callInst);
            call(callbacks.VisitCallbacks, payload, *callInst);
          }
        }
      }
    }
  }
}

void VisitorBase::visit(void *payload, Function &fn) const {
  if (m_strategy == VisitorStrategy::ByInstruction) {
    for (BasicBlock &bb : fn) {
      for (Instruction &inst : make_early_inc_range(bb))
        visit(payload, inst);
    }
    return;
  }

  if (m_strategy == VisitorStrategy::ReversePostOrder) {
    ReversePostOrderTraversal<Function *> rpot(&fn);
    for (BasicBlock *bb : rpot) {
      // Allow callbacks to directly edit the code adding basic blocks
      for (Instruction *inst = &*bb->begin(); inst != nullptr;) {
        auto nextInst = inst->getNextNode();
        visit(payload, *inst);
        inst = nextInst;
      }
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
