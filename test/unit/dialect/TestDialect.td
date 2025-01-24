/*
 ***********************************************************************************************************************
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
 ***********************************************************************************************************************
 */

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

def InstNameConflictTestOp : Op<TestDialect, "try.conflict", [WillReturn]> {
  let results = (outs);
  let arguments = (ins value:$instName, value:$inst__name);

  let summary = "a test case for instName parameter escaping";
  let description = [{
    The builder accepts an additional argument to set the name of the created
    value like IRBuilder methods. This op tries to produce a conflict
  }];
}

def InstNameConflictWithExplRetTestOp : Op<TestDialect, "try.conflict.type.suffix", [WillReturn]> {
  let results = (outs);
  let arguments = (ins value:$instName, value:$inst__name);

  let defaultBuilderHasExplicitResultType = true;

  let summary = "same with explicit return";
  let description = [{
    The builder accepts an additional argument to set the name of the created
    value like IRBuilder methods. This op tries to produce a conflict
  }];
}
