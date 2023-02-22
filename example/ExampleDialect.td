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

def ExampleDialect : Dialect {
  let name = "xd";
  let cppNamespace = "xd";
}

def XdVectorType : DialectType<ExampleDialect, "vector"> {
  let typeArguments = (args type:$element_type, AttrI32:$num_elements);

  let summary = "a custom vector type";
  let description = [{
    Unlike LLVM's built-in vector type, this vector can have arbitrary element
    types (and nesting is possible!).
  }];
}

class ExampleOp<string mnemonic_, list<Trait> traits_>
    : Op<ExampleDialect, mnemonic_, traits_>;

def ReadOp : ExampleOp<"read",
                       [Memory<[(readwrite InaccessibleMem)]>, NoUnwind]> {
  let results = (outs value:$data);
  let arguments = (ins);

  let defaultBuilderHasExplicitResultType = true;

  let summary = "read a piece of data";
  let description = [{
    Longer description of how this operation reads a piece of data.
  }];
}

def WriteOp : ExampleOp<"write",
                        [Memory<[(write InaccessibleMem)]>, NoUnwind,
                         WillReturn]> {
  let results = (outs);
  let arguments = (ins value:$data);

  let summary = "write a piece of data";
  let description = [{
    Longer description of how this operation writes a piece of data.
  }];
}

def CombineOp : ExampleOp<"combine",
                          [Memory<[]>, NoUnwind, WillReturn]> {
  let results = (outs value:$result);
  let arguments = (ins (eq $result):$lhs, (eq $result):$rhs);

  let summary = "combine two pieces of data";
  let description = [{
    Longer description of... well, you know by now how this goes.
  }];
}

def Add32Op : ExampleOp<"add32", [Memory<[]>, NoUnwind, WillReturn]> {
  let results = (outs I32:$result);
  let arguments = (ins I32:$lhs, I32:$rhs, AttrI32:$extra);

  let summary = "add two numbers, and a little extra";
  let description = [{
    For those times when you want a little extra, this operation adds two
    numbers and puts a constant on top.
  }];
}

def SizeOfOp : ExampleOp<"sizeof", [Memory<[]>, NoUnwind, WillReturn]> {
  let results = (outs I64:$result);
  let arguments = (ins type:$sizeof_type);

  let summary = "size of a given type";
  let description = [{
    Returns the store size of the given type in bytes.
  }];
}

def ExtractElementOp : ExampleOp<"extractelement", [Memory<[]>, NoUnwind,
                                                    WillReturn]> {
  let results = (outs value:$result);
  let arguments = (ins (XdVectorType $result, any):$vector, I32:$index);

  let summary = "extract an element from a vector";
  let description = [{
    Return the element of `vector` with the given `index`.
  }];
}

def InsertElementOp : ExampleOp<"insertelement", [Memory<[]>, NoUnwind,
                                                  WillReturn]> {
  let results = (outs value:$result);
  let arguments = (ins value:$vector, value:$value, I32:$index);

  let verifier = [
    (XdVectorType $result, $value, any),
    (eq $result, $vector),
  ];

  let summary = "insert an element into a vector";
  let description = [{
    Insert the given `value` into the given `vector` at the given `index` and
    returns the result.
  }];
}

def FromFixedVectorOp : ExampleOp<"fromfixedvector", [Memory<[]>, NoUnwind,
                                                      WillReturn]> {
  let results = (outs (XdVectorType $scalar_type, $num_elements):$result);
  let arguments = (ins (FixedVectorType $scalar_type, $num_elements):$source);

  let summary = "convert <n x T> to our custom vector type";
  let description = [{
    Demonstrate a more complex unification case.
  }];
}

def ITruncOp : ExampleOp<"itrunc", [Memory<[]>, NoUnwind, WillReturn]> {
  let results = (outs (IntegerType $result_width):$result);
  let arguments = (ins (IntegerType $source_width):$source);

  let verifier = [
    (le $result_width, $source_width),
  ];

  let defaultBuilderHasExplicitResultType = true;

  let summary = "custom trunc operation that only works on integers";
  let description = [{
    Demonstrates the use of a predicate in an unvalued position.
  }];
}

def IExtOp : ExampleOp<"iext", [Memory<[]>, NoUnwind, WillReturn]> {
  let results = (outs (IntegerType $result_width):$result);
  let arguments = (ins (IntegerType (le $result_width)):$source);

  let defaultBuilderHasExplicitResultType = true;

  let summary = "custom ext operation that only works on integers";
  let description = [{
    Demonstrates the use of the same unevaluatable `le` predicate in a valued
    position.
  }];
}