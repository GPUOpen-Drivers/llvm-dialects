; RUN: llvm-dialects-example -visit %s | FileCheck --check-prefixes=DEFAULT %s

; DEFAULT: visiting ReadOp: %v = call i32 @xd.ir.read__i32()
; DEFAULT-NEXT: visiting UnaryInstruction (pre): %w = load i32, ptr %p
; DEFAULT-NEXT: visiting UnaryInstruction (pre): %q = load i32, ptr %p1
; DEFAULT-NEXT: visiting BinaryOperator: %v1 = add i32 %v, %w
; DEFAULT-NEXT: visiting umax intrinsic: %v2 = call i32 @llvm.umax.i32(i32 %v1, i32 %q)
; DEFAULT-NEXT: visiting WriteOp: call void (...) @xd.ir.write(i8 %t)
; DEFAULT-NEXT: visiting SetReadOp: %v.0 = call i1 @xd.ir.set.read__i1()
; DEFAULT-NEXT: visiting SetReadOp: %v.1 = call i32 @xd.ir.set.read__i32()
; DEFAULT-NEXT: visiting SetReadOp (set): %v.1 = call i32 @xd.ir.set.read__i32()
; DEFAULT-NEXT: visiting UnaryInstruction (pre): %v.2 = trunc i32 %v.1 to i8
; DEFAULT-NEXT: visiting UnaryInstruction: %v.2 = trunc i32 %v.1 to i8
; DEFAULT-NEXT: visiting SetWriteOp (set): call void (...) @xd.ir.set.write(i8 %v.2)
; DEFAULT-NEXT: visiting WriteVarArgOp: call void (...) @xd.ir.write.vararg(i8 %t, i32 %v2, i32 %q)
; DEFAULT-NEXT:   %v2 =
; DEFAULT-NEXT:   %q =
; DEFAULT-NEXT: visiting umin (set): %vm = call i32 @llvm.umin.i32(i32 %v1, i32 %q) 
; DEFAULT-NEXT: visiting StringAttrOp: Hello world!
; DEFAULT-NEXT: visiting CallInst:   %0 = call i32 @op.func(i32 %v1, i32 %q)
; DEFAULT-NEXT: visiting CallBrInst:   callbr void @callee()
; DEFAULT-NEXT: to label %continueBB [label %label1BB, label %label2BB]
; DEFAULT-NEXT: visiting Ret (set): ret void
; DEFAULT-NEXT: visiting ReturnInst: ret void
; DEFAULT-NEXT: inner.counter = 1

@0 = private unnamed_addr constant [13 x i8] c"Hello world!\00", align 1

define void @test1(ptr %p) {
entry:
  %v = call i32 @xd.ir.read__i32()
  %w = load i32, ptr %p
  %p1 = getelementptr i32, ptr %p, i32 1
  %q = load i32, ptr %p1
  %v1 = add i32 %v, %w
  %v2 = call i32 @llvm.umax.i32(i32 %v1, i32 %q)
  %t = call i8 (...) @xd.ir.itrunc__i8(i32 %v2)
  call void (...) @xd.ir.write(i8 %t)
  %v.0 = call i1 @xd.ir.set.read__i1()
  %v.1 = call i32 @xd.ir.set.read__i32()
  %v.2 = trunc i32 %v.1 to i8
  call void (...) @xd.ir.set.write(i8 %v.2)
  call void (...) @xd.ir.write.vararg(i8 %t, i32 %v2, i32 %q)
  %vm = call i32 @llvm.umin.i32(i32 %v1, i32 %q)
  call void @xd.ir.string.attr.op(ptr @0)
  call i32 @op.func(i32 %v1, i32 %q)
  callbr void @callee() to label %continueBB [label %label1BB, label %label2BB]
  ret void

continueBB:
  br label %entry

label1BB:
  br label %entry

label2BB:
  br label %entry
}

declare void @callee()
declare i32 @xd.ir.read__i32()
declare i1 @xd.ir.set.read__i1()
declare i32 @xd.ir.set.read__i32()
declare void @xd.ir.write(...)
declare void @xd.ir.set.write(...)
declare void @xd.ir.write.vararg(...)
declare i8 @xd.ir.itrunc__i8(...)
declare void @xd.ir.string.attr.op(ptr)
declare i32 @llvm.umax.i32(i32, i32)
declare i32 @llvm.umin.i32(i32, i32)
declare void @op.func(i32, i32)
