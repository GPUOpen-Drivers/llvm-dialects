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

include "llvm-dialects/Dialect/Dialect.td"

def TestDialect : Dialect {
  let name = "test";
  let cppNamespace = "test";
}

class TestOp<string mnemonic_, list<Trait> traits_>
    : Op<TestDialect, mnemonic_, traits_>;

def DialectOp1 : TestOp<"dialect.op.1",
                       []> {
  let results = (outs);
  let arguments = (ins);

  let summary = "Test operation 1";
}

def DialectOp2 : TestOp<"dialect.op.2",
                       []> {
  let results = (outs);
  let arguments = (ins);

  let summary = "Test operation 2";
}

def DialectOp3 : TestOp<"dialect.op.3",
                       []> {
  let results = (outs);
  let arguments = (ins);

  let summary = "Test operation 3";
}

def DialectOp4 : TestOp<"dialect.op.4", []> {
  let results = (outs value:$r);
  let arguments = (ins value:$v);

  let defaultBuilderHasExplicitResultType = true;

  let summary = "Test operation 4";
}
