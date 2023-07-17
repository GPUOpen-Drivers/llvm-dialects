; RUN: llvm-dialects-example -visit %s | FileCheck --check-prefixes=DEFAULT %s

; DEFAULT: visiting ReadOp: %v = call i32 @xd.read.i32()
; DEFAULT-NEXT: visiting UnaryInstruction: %w = load i32, ptr %p
; DEFAULT-NEXT: visiting UnaryInstruction: %q = load i32, ptr %p1
; DEFAULT-NEXT: visiting BinaryOperator: %v1 = add i32 %v, %w
; DEFAULT-NEXT: visiting umax intrinsic: %v2 = call i32 @llvm.umax.i32(i32 %v1, i32 %q)
; DEFAULT-NEXT: visiting WriteOp: call void (...) @xd.write(i8 %t)
; DEFAULT-NEXT: visiting SetReadOp (set): %v.1 = call i32 @xd.set.read.i32()
; DEFAULT-NEXT: visiting UnaryInstruction: %v.2 = trunc i32 %v.1 to i8
; DEFAULT-NEXT: visiting SetWriteOp (set): call void (...) @xd.set.write(i8 %v.2)
; DEFAULT-NEXT: visiting WriteVarArgOp: call void (...) @xd.write.vararg(i8 %t, i32 %v2, i32 %q)
; DEFAULT-NEXT:   %v2 =
; DEFAULT-NEXT:   %q =
; DEFAULT-NEXT: visiting umin (set): %vm = call i32 @llvm.umin.i32(i32 %v1, i32 %q) 
; DEFAULT-NEXT: visiting Ret (set): ret void
; DEFAULT-NEXT: visiting ReturnInst: ret void
; DEFAULT-NEXT: inner.counter = 1

define void @test1(ptr %p) {
entry:
  %v = call i32 @xd.read.i32()
  %w = load i32, ptr %p
  %p1 = getelementptr i32, ptr %p, i32 1
  %q = load i32, ptr %p1
  %v1 = add i32 %v, %w
  %v2 = call i32 @llvm.umax.i32(i32 %v1, i32 %q)
  %t = call i8 (...) @xd.itrunc.i8(i32 %v2)
  call void (...) @xd.write(i8 %t)
  %v.1 = call i32 @xd.set.read.i32()
  %v.2 = trunc i32 %v.1 to i8
  call void (...) @xd.set.write(i8 %v.2)
  call void (...) @xd.write.vararg(i8 %t, i32 %v2, i32 %q)
  %vm = call i32 @llvm.umin.i32(i32 %v1, i32 %q)
  ret void
}

declare i32 @xd.read.i32()
declare i32 @xd.set.read.i32()
declare void @xd.write(...)
declare void @xd.set.write(...)
declare void @xd.write.vararg(...)
declare i8 @xd.itrunc.i8(...)

declare i32 @llvm.umax.i32(i32, i32)
declare i32 @llvm.umin.i32(i32, i32)
