; RUN: split-file %s %t
; RUN: not llvm-dialects-example -verify %t/bad-parameters.ll 2>&1 | FileCheck --check-prefixes=CHECK %t/bad-parameters.ll
; RUN: not llvm-dialects-example -verify %t/bad-type-info.ll 2>&1 | FileCheck --check-prefixes=CHECK %t/bad-type-info.ll

;--- bad-parameters.ll

; CHECK: [[@LINE+4]]:35: error: target type failed validation:
; CHECK:   wrong number of int parameters
; CHECK:     expected: 2
; CHECK:       actual: 3
declare void @test_bad_parameters(target("xd.vector", i32, 1, 4, 5))

;--- bad-type-info.ll

; CHECK: [[@LINE+1]]:1: error: target type has wrong layout type
type target("xd.vector", i32, 1, 2) {
  layout: type <4 x i32>,
}
