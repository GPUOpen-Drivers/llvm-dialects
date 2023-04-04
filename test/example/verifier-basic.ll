; RUN: not llvm-dialects-example -verify %s | FileCheck --check-prefixes=CHECK %s

define void @test1(ptr %p) {
entry:
; CHECK-LABEL: Verifier error in: %sizeof1 =
; CHECK:  unexpected value of $result:
; CHECK:    expected:  i64
; CHECK:    actual:    i32
  %sizeof1 = call i32 (...) @xd.sizeof(<10 x i32> poison)

; CHECK-LABEL: Verifier error in:   %trunc1 =
; CHECK:  failed check for (le ?:$result_width, ?:$source_width)
; CHECK:  with $result_width = 64
; CHECK:  with $source_width = 32
  %trunc1 = call i64 (...) @xd.itrunc.i64(i32 %sizeof1)

; CHECK-LABEL: Verifier error in:   %fromfixedvector1 =
; CHECK:  unexpected value of $scalar_type:
; CHECK:    expected:  i32
; CHECK:    actual:    i64
  %fromfixedvector1 = call target("xd.vector", i32, 0, 2) (...) @xd.fromfixedvector.txd.vector_i32_0_2t(<2 x i64> poison)

; CHECK-LABEL: Verifier error in:   %fromfixedvector2 =
; CHECK:  unexpected value of $num_elements:
; CHECK:    expected:  2
; CHECK:    actual:    4
  %fromfixedvector2 = call target("xd.vector", i32, 0, 2) (...) @xd.fromfixedvector.txd.vector_i32_0_2t(<4 x i32> poison)

; CHECK-LABEL: Verifier error in:   %stream.max1 =
; CHECK:  unexpected value of $result:
; CHECK:    expected:  i16
; CHECK:    actual:    <2 x i16>
  %stream.max1 = call <2 x i16> (...) @xd.stream.max.v2i16(ptr %p, i64 %trunc1, i16 0)

; CHECK-LABEL: Verifier error in:   %stream.min1 =
; CHECK:  wrong number of arguments: 2, expected 3
  %stream.min1 = call i8 (...) @xd.stream.min.i8(ptr %p, i64 14)
  ret void
}

declare i32 @xd.sizeof(...)
declare i64 @xd.itrunc.i64(...)
declare target("xd.vector", i32, 0, 2) @xd.fromfixedvector.txd.vector_i32_0_2t(...)
declare <2 x i16> @xd.stream.max.v2i16(...)
declare i8 @xd.stream.min.i8(...)
