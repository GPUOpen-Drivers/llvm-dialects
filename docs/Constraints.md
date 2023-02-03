# TableGen Constraints

The TableGen DSL used by llvm-dialects leans heavily on the concept of
_constraints_ to describe types and operations. Constraints form
_constraint systems_ that can range from very simple to very complex and
inter-related.

For example, a binary operation might be defined as:
```tablegen
  let results = (outs I32:$result);
  let arguments = (ins I32:$lhs, I32:$rhs);
```
The implicitly defined constraint system has three variables: `$result`, `$lhs`,
and `$rhs`. The unary _predicate_ `I32` is used in _valued positions_, which are
positions with an associated _self_ value. The self value is used as the
implicit argument when checking the predicate.

> The variables of constraint systems most commonly correspond to IR values
> like operation inputs and outputs. The values of the variables in the
> constraint system are of type `type`. That is, the corresponding C++ variables
> are of type `llvm::Type *`.
>
> Try not to get confused by that: The constraint language only deals in values
> that are guaranteed to be known at the time when the compiler runs, such as
> IR types.

A more general binary operation may be defined as:
```tablegen
  let results = (outs (ScalarOrFixedVector IntegerType):$result);
  let arguments = (ins (eq $result):$lhs, (eq $result):$rhs);
```
This operation has a result of integer type or of fixed vector type with integer
elements, and arguments of the same type.

The implied constraint system uses more complex predicates, like `eq`,
`ScalarOrFixedVector`, and `IntegerType`.

## Predicates

Constraints are built up from _predicates_. Predicates have one or more
arguments.

The first argument is commonly thought of as a _self_ argument. Predicates
commonly appear in _valued_ positions, i.e. positions that correspond to the
value of some (possibly unnamed) constraint system variable. In that case,
the value of the position is substituted for the _self_ argument.

> Example: The `I32` predicate has one argument, which is the type value that
> must be `i32` for the predicate to be satisfied. Recall the example:
> ```tablegen
>  let results = (outs I32:$result);
>  let arguments = (ins I32:$lhs, I32:$rhs);
> ```
> the predicate `I32` appears in three positions, all of them are valued with
> named variables `$result`, `$lhs`, and `$rhs`, respectively.
>
> The operation definition could be rewritten equivalently as:
> ```tablegen
>  let results = (outs value:$result);
>  let arguments = (ins value:$lhs, value:$rhs);
>
>  let verifier = [
>    (I32 $result),
>    (I32 $lhs),
>    (I32 $rhs),
>  ];
> ```
> In this rewritten form, `I32` appears only in non-valued positions, so the
> self argument is given explicitly.

Predicate _applications_ are written in one of two forms:

* As DAGs, where arguments are given explicitly: `(predicate arg...)`
* Using only the predicate itself: `predicate`

The second form can only be used in valued positions, since a self argument is
required. All non-self arguments are treated as `any`.

## Constraints

Constraints come in two forms:

* (Nested) predicate applications
* Logical connectives `or` and `and`

Variables can be named or "captured" in valued positions using the `:$name`
part of TableGen DAG syntax.

> Example: This snippet is part of a definition of an operation that takes an
> integer value as input and produces a vector of bits whose length is the bit
> width of the input.
> ```tablegen
>   let results = (outs (FixedVectorType I1, $num_bits):$result);
>   let arguments = (ins (IntegerType $num_bits):$source);
> ```
> The variable `$num_bits` is captured in two different places. The code
> generation infrastructure uses this fact to:
>
>  * Generate a default builder method that deduces the result type from the
>    source type.
>  * Create IR verifier code that verifies that the number of elements in the
>    result vector is equal to the number of bits in the source operand.
>

## Logical `and`

Multiple constraints can be combined via `(and constraint1, constraint2, ...)`.
If an `and` expression appears in a valued position, then the positions of the
child constraints are equally valued.

## Logical `or`

Constraints can also be combined as `(or constraint1, constraint2, ...)`. If an
`or` expression appears in a valued position, then the positions of the child
constraints are equally valued.

An important limitation of logical `or` is that it does not allow capturing of
variable values.

> Example: It may be tempting to try to define an analog of the LLVM IR `trunc`
> instruction as
> ```tablegen
>   let results = (outs (ScalarOrFixedVector (IntegerType $result_bits):$result);
>   let arguments = (ins (ScalarOrFixedVector (IntegerType $source_bits):$source);
>
>   let verifier = [
>     (or (and (FixedVectorType $result, $num_elements), (FixedVectorType $source, $num_elements)),
>         (and (IntegerType $result), (IntegerType $source))),
>     (le $result_bits, $source_bits),
>   ];
> ```
> However, this fails. `ScalarOrFixedVector` is defined as a `TgPredicate`
> using an `or` expression, and so `$result_bits` and `$source_bits` cannot be
> captured, which prevents code generation for the verifier constraint.
>
> An additional conceptual problem is that `$num_elements` appears only in one
> of the child constraints of the `or` in the verifier list.

This limitation is on purpose. The interaction of variable capture with the
logical `or` would lead to a form of non-determinism inherent in the formulation
of constraint systems that we want to avoid.

> Note: A possible future extension of the language that avoids this
> non-determinism is a constraint expression that conditionally enables
> sub-constraints based on non-capturing guard constraints. We may be able to
> define an analog of LLVM IR `trunc` as:
> ```tablegen
>   let results = (outs value:$result);
>   let arguments = (ins value:$source);
>
>   let verifier = [
>     (cond
>       (FixedVectorType $result), (and (FixedVectorType $result, $result_scalar, $num_elements),
>                                       (FixedVectorType $source, $source_scalar, $num_elements)),
>       true, (and (eq $result, $result_scalar),
>                  (eq $source, $source_scalar))),
>     (IntegerType $result_scalar, $result_bits),
>     (IntegerType $source_scalar, $source_bits),
>     (le $result_bits, $source_bits),
>   ];
> ```
> The guard constraints can be evaluated in-order without having to capture from
> them.
>
> Both conditional constraints have `$result_scalar` and `$source_scalar` in
> capturable positions, so the overall `cond` expression is able to capture both
> variables.
>
> There is still the issue of `$num_elements` appearing only conditionally.
> Another possible future extension is a constraint expression that introduces
> a new scope, so we could write:
> ```tablegen
>   let verifier = [
>     (cond
>       (FixedVectorType $result), (scope (args AttrI32:$num_elements),
>                                    (FixedVectorType $result, $result_scalar, $num_elements),
>                                    (FixedVectorType $source, $source_scalar, $num_elements)),
>       true, (and (eq $result, $result_scalar),
>                  (eq $source, $source_scalar))),
>     ...
>   ];
> ```

## Custom predicate definitions

Predicates can be defined in one of two ways:

* TableGen expressions
* C++ expressions

The easiest way is via a TableGen expression. Applications of the custom
predicate are simply replaced by the given expression, with arguments
substituted appropriately.

> Example: The `ScalarOrFixedVector` predicate is defined as
> ```tablegen
>   def ScalarOrFixedVector : TgPredicate<
>       (args type:$self, type:$scalar_type),
>       (or (eq $self, $scalar_type),
>           (FixedVectorType $self, $scalar_type, any))>;
> ```

Predicates can also be defined by C++ expressions that describe how to check,
capture from, or evaluate the predicate.

_Evaluating_ a (C++) predicate means computing its self argument from the other
arguments. C++ predicates don't have to be evaluatable.

_Capturing from_ a (C++) predicate means computing one of the non-self arguments
given the value of the self argument. Non-self arguments don't necessarily have
to be capturable.

It is generally assumed that evaluating a predicate is more expensive than
capturing from it. This assumption is motivated by the fact that it is true for
type predicates.

> Example: The comparison predicates (`le`, `lt`, `ge`, `gt`) are neither
> evaluatable nor can they capture their right-hand side.
>
> By contrast, the equality predicate `eq` is both evaluatable (the left-hand
> side can be obtained given the right-hand side) and it can capture its
> non-self argument (the right-hand side can be obtained given the left-hand
> side).

_Checking_ a (C++) predicate means checking whether the predicate is true given
all non-capturable arguments (including the self argument). Every C++ predicate
must be checkable.

> Example: Checking the `IntegerType` predicate means checking that the self
> argument is an integer type, using the C++ expression
> `$self->isIntegerType()`.
>
> An overall application of this predicate such as `(IntegerType $self, 32)`
> can be checked by first checking the predicate itself and then capturing the
> capturable argument and checking it against the given constraint (in this
> case, checking it against `32` which just means checking that the captured
> number of bits is equal to 32).
>
> That said, since this predicate application is also evaluatable, the verifier
> generator prefers to evaluate `$self` based on this expression and then use
> an equality comparison to check whether the evaluation result is identical to
> the value captured from elsewhere.
