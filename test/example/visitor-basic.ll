; RUN: llvm-dialects-example -visit %s | FileCheck --check-prefixes=DEFAULT %s

; DEFAULT-DAG: visiting ReadOp: %v = call i32 @xd.read.i32()
; DEFAULT-DAG: visiting WriteOp: call void (...) @xd.write(i8 %t)
; DEFAULT: inner.counter = 1

define void @test1(ptr %p) {
entry:
  %v = call i32 @xd.read.i32()
  %t = call i8 (...) @xd.itrunc.i8(i32 %v)
  call void (...) @xd.write(i8 %t)
  ret void
}

declare i32 @xd.read.i32()
declare void @xd.write(...)
declare i8 @xd.itrunc.i8(...)
