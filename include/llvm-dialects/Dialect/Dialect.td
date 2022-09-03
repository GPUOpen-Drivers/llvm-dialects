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

def ins;
def outs;

def seq;

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
/// Types and (type) constraints
///
/// TableGen representation of LLVM IR types, including both builtin types (such as i32, pointers)
/// and dialect-defined types.
///
/// TODO Type constraints
// ============================================================================

class Constraint {
  // An optional piece of code that is used to filter all argument values of
  // this type in builder methods. $_self is the argument value.
  string builderArgumentFilter = "";
}

class Type : Constraint;

class BuiltinType : Type {
  // C++ expression to obtain an llvm::Type* instance.
  string getter = ?;
}

def VoidTy : BuiltinType {
  let getter = "::llvm::Type::getVoidTy($_context)";
}

/// LLVM IR integer type.
class IntegerType<int numBits_> : BuiltinType {
  int numBits = numBits_;

  let getter = "::llvm::Type::getInt" # numBits_ # "Ty($_context)";
}

def I1 : IntegerType<1>;
def I8 : IntegerType<8>;
def I16 : IntegerType<16>;
def I32 : IntegerType<32>;
def I64 : IntegerType<64>;

// LLVM IR pointer type.
class PointerType<int addressSpace_> : BuiltinType {
  int addressSpace = addressSpace_;

  let getter = "::llvm::PointerType::get($_context, " # addressSpace_ # ")";

  let builderArgumentFilter = [{
    if (!$_self->getType()->isOpaquePointerTy())
      $_self = $_builder.CreateBitCast($_self, }] # getter # [{ );
  }];
}

// LLVM IR fixed vector type (<n x T>).
class FixedVectorType<int numElements_, BuiltinType elementType_> : BuiltinType {
  int numElements = numElements_;
  BuiltinType elementType = elementType_;

  let getter = "::llvm::FixedVectorType::get(" # elementType.getter # ", "
                   # numElements # ")";
}

/// All types that are defined by a dialect are derived from this class.
class DialectType<Dialect dialect_, string mnemonic_> : Type {
  Dialect dialect = dialect_;

  string mnemonic = mnemonic_;
}

/// A generic predicate over arguments of an operation, implemented as a
/// snippet of C++.
///
/// Unary predicates can be used in argument and result position, e.g.
/// @code
///   def MyUnaryPred : BaseCPred<...>;
///   def MyOp : Op<...> {
///     let arguments = (ins MyUnaryPred:$argname);
///     ...
///   }
/// @endcode
///
/// All predicates can be used in verifier rules, e.g.:
/// @code
///   def MyArithmeticOp : Op<MyDialect, "my.arithmetic", [],
///                           [(SameTypes $lhs, $rhs, $result)]> {
///     let arguments = (ins $lhs, $rhs);
///     let results = (outs $result);
///   }
/// @endcode
class BaseCPred<dag arguments_, code pred> : Constraint {
  /// The arguments provided to the predicate.
  dag arguments = arguments_;

  /// The C++ expression into which the arguments are substituted to test the
  /// predicate.
  code predExpr = pred;
}

class CPred<code pred> : BaseCPred<(ins $_self), pred>;

def AnyType : CPred<"true">;

def AnyPointerType : CPred<"$_self->isPointerTy()"> {
  let builderArgumentFilter = [{
    if (!$_self->getType()->isOpaquePointerTy()) {
      unsigned as = cast<::llvm::PointerType>($_self)->getAddressSpace();
      $_self = $_builder.CreateBitCast($_self,
                                       ::llvm::PointerType::get($_context, as));
    }
  }];
}

/// Variadic predicate that is true if all its arguments have the same
/// LLVM type.
def SameTypes : BaseCPred<(ins seq:$types), "::llvm_dialects::areTypesEqual($types)">;

def and;
def or;
def not;

// ============================================================================
/// Attributes
///
/// Operations can have attributes, which one can think of as constant arguments. The default
/// representation of an operation with attributes uses function call arguments that are always
/// llvm::Constants.
// ============================================================================

class Attr<string cppType_, Type llvmType_> : Constraint {
  string cppType = cppType_;
  Type llvmType = llvmType_;

  // $0 is the C++ value, $1 is the LLVM type.
  string toLlvmValue = ?;

  // $0 is the LLVM value.
  string fromLlvmValue = ?;
}

class IntegerAttr<string cppType_, Type llvmType_> : Attr<cppType_, llvmType_> {
  let toLlvmValue = [{ ::llvm::ConstantInt::get($1, $0) }];
  let fromLlvmValue = [{ ::llvm::cast<::llvm::ConstantInt>($0)->getZExtValue() }];
}

def AttrI1 : IntegerAttr<"bool", I1>;
def AttrI8 : IntegerAttr<"uint8_t", I8>;
def AttrI16 : IntegerAttr<"uint16_t", I16>;
def AttrI32 : IntegerAttr<"uint32_t", I32>;
def AttrI64 : IntegerAttr<"uint64_t", I64>;


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

class LlvmAttributeTrait<string llvmEnum_> : Trait {
  string llvmEnum = llvmEnum_;
}

def NoUnwind : LlvmAttributeTrait<"NoUnwind">;
def ReadNone : LlvmAttributeTrait<"ReadNone">;
def WillReturn : LlvmAttributeTrait<"WillReturn">;


// ============================================================================
/// Operations
// ============================================================================

class Op<Dialect dialect_, string mnemonic_, list<Trait> traits_> {
  Dialect dialect = dialect_;
  string mnemonic = mnemonic_;
  list<Trait> traits = traits_;

  dag arguments = ?;
  dag results = ?;

  list<dag> verifier = [];

  string summary = ?;
  string description = ?;
}
