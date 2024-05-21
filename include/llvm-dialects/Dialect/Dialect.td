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

def args;
def ins;
def outs;

class MetaType;

def type : MetaType;
def value;
def varargs;

// ============================================================================
/// Dialect
// ============================================================================

/// Derive from this class to define a dialect.
///
/// Dialects group operations and type information. This is in analogy to MLIR.
class Dialect {
  /// Short name of the dialect. Operations are prefixed with this name by
  /// default.
  string name = ?;

  /// The C++ namespace in which classes of the dialect are defined.
  string cppNamespace = ?;
}


// ============================================================================
/// Attributes
///
/// Operations and types can have attributes, which one can think of as constant
/// arguments.
///
/// The default representation of an operation with attributes uses function
/// call arguments that are always llvm::Constants.
// ============================================================================

class Attr<string cppType_> : MetaType {
  string cppType = cppType_;

  // $0 is the C++ value, $1 is the LLVM type.
  string toLlvmValue = ?;

  // $0 is the LLVM value.
  string fromLlvmValue = ?;

  // For use in dialect types. $0 is the C++ value
  string toUnsigned = "$0";

  // For use in dialect types. $0 is the "type-erased" unsigned value
  string fromUnsigned = "$0";

  // A check statement that is issued before using the C++ value in builders.
  // $0 is the C++ value.
  string check = "";
  
  // Overriding prevents generating a setter method. Attributes are mutable by default.
  bit isImmutable = false;
}

class IntegerAttr<string cppType_> : Attr<cppType_> {
  let toLlvmValue = [{ ::llvm::ConstantInt::get($1, $0) }];
  let fromLlvmValue = [{ ::llvm::cast<::llvm::ConstantInt>($0)->getZExtValue() }];
}

def AttrI1 : IntegerAttr<"bool">;
def AttrI8 : IntegerAttr<"uint8_t">;
def AttrI16 : IntegerAttr<"uint16_t">;
def AttrI32 : IntegerAttr<"uint32_t">;
def AttrI64 : IntegerAttr<"uint64_t">;

// ============================================================================
/// Types and (type) constraints
///
/// TableGen representation of LLVM IR types, including both builtin types
/// (such as i32, pointers) and dialect-defined types.
// ============================================================================

class Constraint;

class Predicate : Constraint {
  /// Arguments of this predicate. The first argument is considered the "self"
  /// argument for when the predicate is used in a valued position.
  dag arguments = ?;
}

class CppPredicateMixin {
  /// C++ expression to evaluate the predicate, substituting arguments by name.
  /// All arguments (both capturable and uncapturable) can be (and usually are)
  /// used in this expression.
  string evaluate = ?;

  /// C++ expression to check the predicate. Only the "self" (first) argument
  /// and any uncapturable arguments can be referenced.
  string check = ?;

  /// C++ expressions to capture other arguments from the "self" (first)
  /// argument, assuming that the `check` expression is true.
  ///
  /// This list can be empty to indicate that no argument can be captured.
  /// Otherwise, its length must match the number of non-self arguments.
  list<string> capture = ?;
}

def any : Predicate, CppPredicateMixin, MetaType {
  let arguments = (args any:$self);
  let evaluate = ""; // cannot be evaluated
  let check = "true";
  let capture = [];
}

def eq : Predicate, CppPredicateMixin {
  let arguments = (args any:$lhs, any:$rhs);
  let evaluate = "$rhs";
  let check = "true"; // the check happens implicitly via the capture logic
  let capture = ["$lhs"];
}

class TgPredicateMixin {
  /// Expression that a predicate application is substituted by.
  dag expression = ?;
}

class TgPredicate<dag arguments_, dag expression_>
    : Predicate, TgPredicateMixin {
  let arguments = arguments_;
  let expression = expression_;
}

class TgConstant<dag expression_>
    : TgPredicate<(args any:$self), (eq $self, expression_)>;

class Type;

class BuiltinType : Type, Predicate, CppPredicateMixin;

def VoidTy : BuiltinType {
  let arguments = (args type:$self);
  let evaluate = "::llvm::Type::getVoidTy($_context)";
  let check = "$self->isVoidTy()";
  let capture = [];
}

def IntegerType : BuiltinType {
  let arguments = (args type:$self, AttrI32:$num_bits);
  let evaluate = "::llvm::IntegerType::get($_context, $num_bits)";
  let check = "$self->isIntegerTy()";
  let capture = ["$self->getIntegerBitWidth()"];
}

def I1 : TgConstant<(IntegerType 1)>, Type;
def I8 : TgConstant<(IntegerType 8)>, Type;
def I16 : TgConstant<(IntegerType 16)>, Type;
def I32 : TgConstant<(IntegerType 32)>, Type;
def I64 : TgConstant<(IntegerType 64)>, Type;

def PointerType : BuiltinType {
  let arguments = (args type:$self, AttrI32:$address_space);
  // Supports both typed and opaque pointers.
  let evaluate = "::llvm::PointerType::get(::llvm::Type::getInt8Ty($_context), $address_space)";
  let check = "$self->isPointerTy()";
  let capture = ["$self->getPointerAddressSpace()"];
}

def Ptr : TgConstant<(PointerType 0)>, Type;

// LLVM IR fixed vector type (<n x T>).
def FixedVectorType : BuiltinType {
  let arguments = (args type:$self, type:$element_type, AttrI32:$num_elements);
  let evaluate = "::llvm::FixedVectorType::get($element_type, $num_elements)";
  let check = "::llvm::isa<::llvm::FixedVectorType>($self)";
  let capture = [
    "::llvm::cast<::llvm::FixedVectorType>($self)->getElementType()",
    "::llvm::cast<::llvm::FixedVectorType>($self)->getNumElements()",
  ];
}

// LLVM IR array type
def ArrayType : BuiltinType {
  let arguments = (args type:$self, type:$element_type, AttrI32:$num_elements);
  let evaluate = "::llvm::ArrayType::get($element_type, $num_elements)";
  let check = "::llvm::isa<::llvm::ArrayType>($self)";
  let capture = [
    "::llvm::cast<::llvm::ArrayType>($self)->getElementType()",
    "::llvm::cast<::llvm::ArrayType>($self)->getNumElements()",
  ];
}

class SpecialBuiltinType<string name> : BuiltinType {
  let arguments = (args type:$self);
  let evaluate = "::llvm::Type::get" # name # "Ty($_context)";
  let check = "$self->is" # name # "Ty()";
  let capture = [];
}

def F64 : SpecialBuiltinType<"Double">;
def F32 : SpecialBuiltinType<"Float">;
def F16 : SpecialBuiltinType<"Half">;

/// All types that are defined by a dialect are derived from this class.
class DialectType<Dialect dialect_, string mnemonic_> : Type, Predicate {
  dag typeArguments = ?;
  let arguments = !con((args type:$self), typeArguments);

  Dialect dialect = dialect_;
  string mnemonic = mnemonic_;

  /// Whether the Type::get method has an explicit LLVMContext reference as the
  /// first argument.
  bit defaultGetterHasExplicitContextArgument = false;

  string summary = ?;
  string description = ?;
}

def and;
def or;

// ============================================================================
/// Misc. predicates
// ============================================================================

class CppConstant<string cppExpr_> : Predicate, CppPredicateMixin {
  let arguments = (args any:$self);
  let evaluate = cppExpr_;
  let check = "$self == " # cppExpr_;
  let capture = [];
}

def ScalarOrFixedVector : TgPredicate<
    (args type:$self, type:$scalar_type),
    (or (eq $self, $scalar_type),
        (FixedVectorType $self, $scalar_type, any))>;

class Comparison<string cppComparison> : Predicate, CppPredicateMixin {
  let arguments = (args any:$lhs, any:$rhs);
  let evaluate = "";
  let check = "$lhs " # cppComparison # " $rhs";
  let capture = [];
}

def ge : Comparison<">=">;
def gt : Comparison<">">;
def le : Comparison<"<=">;
def lt : Comparison<"<">;

// ============================================================================
/// Tie attributes to LLVM types
// ============================================================================

// Link an Attr to its corresponding LLVM type.
//
// This link is achieved with a separate def to break a circular dependency in
// how IntegerType is defined.
class AttrLlvmType<Attr attr_, Type llvmType_> {
  Attr attr = attr_;
  Type llvmType = llvmType_;
}

def : AttrLlvmType<AttrI1, I1>;
def : AttrLlvmType<AttrI8, I8>;
def : AttrLlvmType<AttrI16, I16>;
def : AttrLlvmType<AttrI32, I32>;
def : AttrLlvmType<AttrI64, I64>;

def ImmutableStringAttr : Attr<"::llvm::StringRef"> {
  let toLlvmValue = [{ $_builder.CreateGlobalString($0) }];
  let fromLlvmValue = [{ ::llvm::cast<::llvm::ConstantDataArray>(::llvm::cast<::llvm::GlobalVariable>($0)->getInitializer())->getAsString() }];
  let isImmutable = true;
}
 
// Global string variables are essentially pointers in addrspace(0).
def : AttrLlvmType<ImmutableStringAttr, Ptr>;

// ============================================================================
/// More general attributes
// ============================================================================

// Define a custom enum attribute that can be used in operations and dialect
// types
//
// An overload of operator<< for llvm::raw_ostream must be provided in the same
// namespace that defines the enum type, or automatically generated verifier
// code may fail to compile.
multiclass AttrEnum<string cppType_> {
  def NAME : Attr<cppType_> {
    let toLlvmValue = "::llvm::ConstantInt::get($1, static_cast<unsigned>($0))";
    let fromLlvmValue = "static_cast<" # cppType_ # ">(::llvm::cast<::llvm::ConstantInt>($0)->getZExtValue())";
    let toUnsigned = "static_cast<unsigned>($0)";
    let fromUnsigned = "static_cast<" # cppType_ # ">($0)";
    let check = "static_assert(sizeof($0) <= sizeof(unsigned));";
  }

  def : AttrLlvmType<!cast<Attr>(NAME), I32>;
}

// ============================================================================
/// Traits
///
/// The traits of an operation encode additional information about the operation, typically
/// information that is useful for program transforms and optimization such as whether the
/// operation writes to memory.
///
/// Traits generally map to llvm::Attributes.
// ============================================================================

class Trait;

class LlvmEnumAttributeTrait<string llvmEnum_> : Trait {
  string llvmEnum = llvmEnum_;
}

def NoUnwind : LlvmEnumAttributeTrait<"NoUnwind">;
def WillReturn : LlvmEnumAttributeTrait<"WillReturn">;
def NoReturn : LlvmEnumAttributeTrait<"NoReturn">;
def NoRecurse : LlvmEnumAttributeTrait<"NoRecurse">;
def NoSync : LlvmEnumAttributeTrait<"NoSync">;
def NoFree : LlvmEnumAttributeTrait<"NoFree">;
def MustProgress : LlvmEnumAttributeTrait<"MustProgress">;
def NoCallback : LlvmEnumAttributeTrait<"NoCallback">;
def NoDuplicate : LlvmEnumAttributeTrait<"NoDuplicate">;
def NoBuiltin : LlvmEnumAttributeTrait<"NoBuiltin">;
def Builtin : LlvmEnumAttributeTrait<"Builtin">;
def InlineHint : LlvmEnumAttributeTrait<"InlineHint">;
def AlwaysInline : LlvmEnumAttributeTrait<"AlwaysInline">;
def Cold : LlvmEnumAttributeTrait<"Cold">;
def Hot : LlvmEnumAttributeTrait<"Hot">;
def Convergent : LlvmEnumAttributeTrait<"Convergent">;

/// Represent the LLVM `memory(...)` attribute as the OR (or union) of memory
/// effects. An empty effects list means the operation does not access memory
/// at all. If this trait is missing, it means that there is no bound on the
/// memory accesses performed by the operation.
///
/// Each memory effect is given as a DAG `($modref location, ...)`. If the list
/// of memory locations is empty, it means all memory may be accessed in this
/// way.
///
/// Example: `Memory<[(ref), (mod ArgMem, InaccessibleMem)]>` means the
/// operation may read from any kind of memory and write to argument and
/// inaccessible memory.
class Memory<list<dag> effects_> : Trait {
  list<dag> effects = effects_;
}

def read;
def write;
def readwrite;

class LlvmMemoryLocation<string name_> {
  string name = name_;
}

def ArgMem : LlvmMemoryLocation<"ArgMem">;
def InaccessibleMem : LlvmMemoryLocation<"InaccessibleMem">;
def Other : LlvmMemoryLocation<"Other">;

// ============================================================================
/// Operation classes
///
/// Operations can have an OpClass as a super class (which is *not* a TableGen
/// super class). This is used to conveniently represent a set of operations
/// that have a common prefix of arguments: there is a C++ class named after the
/// OpClass record, and all operations that have this OpClass as a superclass
/// have their C++ class derived from it.
// ============================================================================

class OpClassBase;

def NoSuperClass : OpClassBase;

class OpClass<Dialect dialect_> : OpClassBase {
  Dialect dialect = dialect_;
  OpClassBase superclass = NoSuperClass;

  dag arguments = ?;

  string summary = ?;
  string description = ?;
}


// ============================================================================
/// Operations
// ============================================================================

class Op<Dialect dialect_, string mnemonic_, list<Trait> traits_> {
  Dialect dialect = dialect_;
  OpClassBase superclass = NoSuperClass;
  string mnemonic = mnemonic_;
  list<Trait> traits = traits_;

  dag arguments = ?;
  dag results = ?;

  list<dag> verifier = [];

  string summary = ?;
  string description = ?;

  /// Whether the default builder method has an explicit result type as its
  /// first argument.
  ///
  /// If this is false, the result type must be either constant or computable
  /// from the argument types using the available constraints.
  bit defaultBuilderHasExplicitResultType = false;
}
