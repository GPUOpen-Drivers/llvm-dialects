/*
 ***********************************************************************************************************************
 *
 *  Copyright (c) 2023 Advanced Micro Devices, Inc. All Rights Reserved.
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the "Software"), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in all
 *  copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 *  SOFTWARE.
 *
 **********************************************************************************************************************/

#include "ExampleDialect.h"

#include "llvm-dialects/Dialect/Builder.h"

#include "llvm/IR/Module.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IRPrinter/IRPrintingPasses.h"

using namespace llvm;
using namespace llvm_dialects;

void createFunctionExample(Module &module, const Twine &name) {
  Builder b{module.getContext()};

  Function *fn = Function::Create(FunctionType::get(b.getVoidTy(), false),
                                  GlobalValue::ExternalLinkage, name, module);

  BasicBlock *bb = BasicBlock::Create(module.getContext(), "entry", fn);
  b.SetInsertPoint(bb);

  Value *x1 = b.create<xd::ReadOp>(b.getInt32Ty());
  Value *x2 = b.create<xd::Add32Op>(x1, b.getInt32(42), 7);
  Value *x3 = b.create<xd::CombineOp>(x2, x1);
  b.create<xd::WriteOp>(x3);
  b.CreateRetVoid();
}

std::unique_ptr<Module> createModuleExample(LLVMContext &context) {
  auto module = std::make_unique<Module>("example", context);
  createFunctionExample(*module, "example");
  return module;
}

int main(int argc, char **argv) {
  LLVMContext context;
  auto dialectContext = DialectContext::make<xd::ExampleDialect>(context);

  auto module = createModuleExample(context);

  module->print(llvm::outs(), nullptr, false);

  return 0;
}
