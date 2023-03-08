; RUN: llvm-dialects-example -visit %s | FileCheck --check-prefixes=DEFAULT %s
; RUN: llvm-dialects-example -visit -rpot %s | FileCheck --check-prefixes=RPOT %s

; DEFAULT: visiting WriteOp: call void (...) @xd.write(i32 0)
; DEFAULT: visiting WriteOp: call void (...) @xd.write(i32 2)
; DEFAULT: visiting WriteOp: call void (...) @xd.write(i32 1)

; RPOT: visiting WriteOp: call void (...) @xd.write(i32 0)
; RPOT: visiting WriteOp: call void (...) @xd.write(i32 1)
; RPOT: visiting WriteOp: call void (...) @xd.write(i32 2)

define void @test1(ptr %p) {
entry:
  call void (...) @xd.write(i32 0)
  br label %a

b:
  call void (...) @xd.write(i32 2)
  ret void

a:
  call void (...) @xd.write(i32 1)
  br label %b
}

declare void @xd.write(...)
