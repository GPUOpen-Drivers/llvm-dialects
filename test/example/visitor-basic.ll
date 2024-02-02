; RUN: llvm-dialects-example -visit %s | FileCheck --check-prefixes=DEFAULT %s

; DEFAULT: Will visit ReadOp next: %v = call i32 @xd.read__i32()
; DEFAULT-NEXT: visiting ReadOp: %v = call i32 @xd.read__i32()
; DEFAULT-NEXT: visiting UnaryInstruction (pre): %w = load i32, ptr %p
; DEFAULT-NEXT: visiting UnaryInstruction (pre): %q = load i32, ptr %p1
; DEFAULT-NEXT: visiting BinaryOperator: %v1 = add i32 %v, %w
; DEFAULT-NEXT: Pre-visiting intrinsic instruction: %v2 = call i32 @llvm.umax.i32(i32 %v1, i32 %q)
; DEFAULT-NEXT: visiting umax intrinsic: %v2 = call i32 @llvm.umax.i32(i32 %v1, i32 %q)
; DEFAULT-NEXT: Counter before visiting ITruncOp: 0
; DEFAULT-NEXT: Counter after visiting ITruncOp: 1
; DEFAULT-NEXT: Will visit WriteOp next: call void (...) @xd.write(i8 %t)
; DEFAULT-NEXT: visiting WriteOp: call void (...) @xd.write(i8 %t)
; DEFAULT-NEXT: visiting SetReadOp: %v.0 = call i1 @xd.set.read__i1()
; DEFAULT-NEXT: visiting SetReadOp: %v.1 = call i32 @xd.set.read__i32()
; DEFAULT-NEXT: visiting SetReadOp (set): %v.1 = call i32 @xd.set.read__i32()
; DEFAULT-NEXT: visiting UnaryInstruction (pre): %v.2 = trunc i32 %v.1 to i8
; DEFAULT-NEXT: visiting UnaryInstruction: %v.2 = trunc i32 %v.1 to i8
; DEFAULT-NEXT: visiting SetWriteOp (set): call void (...) @xd.set.write(i8 %v.2)
; DEFAULT-NEXT: visiting WriteVarArgOp: call void (...) @xd.write.vararg(i8 %t, i32 %v2, i32 %q)
; DEFAULT-NEXT:   %v2 =
; DEFAULT-NEXT:   %q =
; DEFAULT-NEXT: Pre-visiting intrinsic instruction: %vm = call i32 @llvm.umin.i32(i32 %v1, i32 %q) 
; DEFAULT-NEXT: visiting umin (set): %vm = call i32 @llvm.umin.i32(i32 %v1, i32 %q) 
; DEFAULT-NEXT: visiting Ret (set): ret void
; DEFAULT-NEXT: visiting ReturnInst: ret void
; DEFAULT-NEXT: inner.counter = 1

define void @test1(ptr %p) {
entry:
  %v = call i32 @xd.read__i32()
  %w = load i32, ptr %p
  %p1 = getelementptr i32, ptr %p, i32 1
  %q = load i32, ptr %p1
  %v1 = add i32 %v, %w
  %v2 = call i32 @llvm.umax.i32(i32 %v1, i32 %q)
  %t = call i8 (...) @xd.itrunc__i8(i32 %v2)
  call void (...) @xd.write(i8 %t)
  %v.0 = call i1 @xd.set.read__i1()
  %v.1 = call i32 @xd.set.read__i32()
  %v.2 = trunc i32 %v.1 to i8
  call void (...) @xd.set.write(i8 %v.2)
  call void (...) @xd.write.vararg(i8 %t, i32 %v2, i32 %q)
  %vm = call i32 @llvm.umin.i32(i32 %v1, i32 %q)
  ret void
}

declare i32 @xd.read__i32()
declare i1 @xd.set.read__i1()
declare i32 @xd.set.read__i32()
declare void @xd.write(...)
declare void @xd.set.write(...)
declare void @xd.write.vararg(...)
declare i8 @xd.itrunc__i8(...)

declare i32 @llvm.umax.i32(i32, i32)
declare i32 @llvm.umin.i32(i32, i32)
