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

def ExampleDialect : Dialect {
  let name = "xd.ir";
  let cppNamespace = "xd::cpp";
}

defm AttrVectorKind : AttrEnum<"VectorKind">;

def VectorKindLittleEndian : CppConstant<"xd::cpp::VectorKind::LittleEndian">;
def VectorKindBigEndian : CppConstant<"xd::cpp::VectorKind::BigEndian">;
def VectorKindMiddleEndian : CppConstant<"xd::cpp::VectorKind::MiddleEndian">;

def ImmutableAttrI1 : IntegerAttr<"bool"> {
  let isImmutable = true;
}

def : AttrLlvmType<ImmutableAttrI1, I1>;

def isReasonableVectorKind : TgPredicate<
    (args AttrVectorKind:$kind),
    (eq $kind, (or VectorKindLittleEndian, VectorKindBigEndian))>;

def XdVectorType : DialectType<ExampleDialect, "vector"> {
  let typeArguments = (args AttrVectorKind:$kind, type:$element_type,
                            AttrI32:$num_elements);

  let summary = "a custom vector type";
  let description = [{
    Unlike LLVM's built-in vector type, this vector can have arbitrary element
    types (and nesting is possible!).
  }];
}

def XdHandleType : DialectType<ExampleDialect, "handle"> {
  let typeArguments = (args);

  let defaultGetterHasExplicitContextArgument = true;

  let summary = "an opaque handle type";
  let description = [{
    Test that a type without arguments works correctly.
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

def WriteVarArgOp : ExampleOp<"write.vararg",
                        [Memory<[(write InaccessibleMem)]>, NoUnwind,
                         WillReturn]> {
  let results = (outs);
  let arguments = (ins value:$data, varargs:$args);

  let summary = "write one or more data elements";
  let description = [{
    Longer description of how this operation writes pieces of data.
  }];
}

def SetReadOp : ExampleOp<"set.read",
                       [Memory<[(readwrite InaccessibleMem)]>, NoUnwind]> {
  let results = (outs value:$data);
  let arguments = (ins);

  let defaultBuilderHasExplicitResultType = true;

  let summary = "read a piece of data";
  let description = [{
    Longer description of how this operation reads a piece of data.
  }];
}

def SetWriteOp : ExampleOp<"set.write",
                        [Memory<[(write InaccessibleMem)]>, NoUnwind,
                         WillReturn]> {
  let results = (outs);
  let arguments = (ins value:$data);

  let summary = "write a data element";
  let description = [{
    Longer description of how this operation writes pieces of data.
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

  let value_traits = [
    (NoCapture $sizeof_type),
  ];

  let summary = "size of a given type";
  let description = [{
    Returns the store size of the given type in bytes.
  }];
}

def ExtractElementOp : ExampleOp<"extractelement", [Memory<[]>, NoUnwind,
                                                    WillReturn]> {
  let results = (outs value:$result);
  let arguments = (ins (XdVectorType any, $result, any):$vector, I32:$index);

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
    (XdVectorType $result, any, $value, any),
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
  let results = (outs (XdVectorType $kind, $scalar_type, $num_elements):$result);
  let arguments = (ins (FixedVectorType $scalar_type, $num_elements):$source);

  let defaultBuilderHasExplicitResultType = true;

  let verifier = [
    (isReasonableVectorKind $kind),
  ];

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

def StreamReduceOp : OpClass<ExampleDialect> {
  let arguments = (ins Ptr:$ptr, I64:$count, value:$initial);

  let value_traits = [
    (NoCapture $ptr)
  ];

  let summary = "family of operations that reduce some array in memory";
  let description = [{
    Illustrate the use of the OpClass feature.
  }];
}

class StreamReduceTemplate<string op>
    : ExampleOp<"stream." # op, [Memory<[(read)]>, NoUnwind, WillReturn]> {
  let superclass = StreamReduceOp;

  let results = (outs (eq $initial):$result);
  let arguments = (ins superclass);

  let summary = "perform the " # op # " operation streaming from memory";
  let description = [{
    Illustrate the use of the OpClass feature.
  }];
}

def StreamAddOp : StreamReduceTemplate<"add">;
def StreamMaxOp : StreamReduceTemplate<"max">;
def StreamMinOp : StreamReduceTemplate<"min">;

def HandleGetOp : ExampleOp<"handle.get", [Memory<[]>, NoUnwind, WillReturn]> {
  let results = (outs XdHandleType:$result);
  let arguments = (ins);

  let summary = "obtain a handle";
  let description = [{
    Use a dialect type without type arguments.
  }];
}

def InstNameConflictOp : Op<ExampleDialect, "inst.name.conflict", [WillReturn]> {
  let results = (outs I32:$result);
  let arguments = (ins value:$instName);

  let summary = "demonstrate how name conflict will be avoided";
  let description = [{
    The builder accepts an additional argument to set the name of the created
    value like IRBuilder methods. This op produces a conflict so the parameter
    will be renamed.
  }];
}

def InstNameConflictDoubleOp : Op<ExampleDialect, "inst.name.conflict.double", [WillReturn]> {
  let results = (outs I32:$result);
  let arguments = (ins value:$instName, value:$instName_0);

  let summary = "demonstrate how name conflict will be avoided";
  let description = [{
    Like InstNameConflictOp but this has a second parameter named like the
    dialect compiler's first choice
  }];
}

def InstNameConflictVarargsOp : Op<ExampleDialect, "inst.name.conflict.varargs", [WillReturn]> {
  let results = (outs I32:$result);
  let arguments = (ins varargs:$instName_0);

  let summary = "demonstrate how name conflict will be avoided";
  let description = [{
    Like InstNameConflictOp but with varargs
  }];
}

def ImmutableOp : Op<ExampleDialect, "immutable.op", [WillReturn]> {
  let results = (outs);
  let arguments = (ins ImmutableAttrI1:$val);

  let summary = "demonstrate how an argument will not get a setter method";
  let description = [{
    Make an argument immutable
  }];
}

def StringAttrOp : Op<ExampleDialect, "string.attr.op", [WillReturn]> {
  let results = (outs);
  let arguments = (ins ImmutableStringAttr:$val);

  let summary = "demonstrate an argument that takes in a StringRef";
  let description = [{
    The argument should not have a setter method
  }];
}

def NoSummaryOp : Op<ExampleDialect, "no.summary.op", [WillReturn]> {
  let results = (outs);
  let arguments = (ins);

  let description = [{
    Some description
  }];
}

def NoDescriptionOp : Op<ExampleDialect, "no.description.op", [WillReturn]> {
  let results = (outs);
  let arguments = (ins);

  let summary = "Some summary";
}

def BufferCompareOp : Op<ExampleDialect, "buffer.compare.op", [WillReturn, NoUnwind]> {
  let results = (outs I32:$ret);
  let arguments = (ins Ptr:$lhs, Ptr:$rhs);

  let value_traits = [
    (NoCapture $lhs),
    (NoCapture $rhs),
    (NoUndef $lhs),
    (NoUndef $ret),
  ];

  let summary = "demonstrate how multiple parameter attributes are added";
  let description = [{
    Both arguments get a parameter attribute, as well as return value
  }];
}
