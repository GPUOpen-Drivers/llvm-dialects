;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; Copyright (c) 2024 Advanced Micro Devices, Inc. All Rights Reserved.
 ;
 ; Licensed under the Apache License, Version 2.0 (the "License");
 ; you may not use this file except in compliance with the License.
 ; You may obtain a copy of the License at
 ;
 ;     http://www.apache.org/licenses/LICENSE-2.0
 ;
 ; Unless required by applicable law or agreed to in writing, software
 ; distributed under the License is distributed on an "AS IS" BASIS,
 ; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 ; See the License for the specific language governing permissions and
 ; limitations under the License.
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;


; RUN: llvm-dialects-example -visit %s | FileCheck --check-prefixes=DEFAULT %s
; RUN: llvm-dialects-example -visit -rpot %s | FileCheck --check-prefixes=RPOT %s

; DEFAULT: visiting WriteOp: call void (...) @xd.ir.write(i32 0)
; DEFAULT: visiting WriteOp: call void (...) @xd.ir.write(i32 2)
; DEFAULT: visiting WriteOp: call void (...) @xd.ir.write(i32 1)

; RPOT: visiting WriteOp: call void (...) @xd.ir.write(i32 0)
; RPOT: visiting WriteOp: call void (...) @xd.ir.write(i32 1)
; RPOT: visiting WriteOp: call void (...) @xd.ir.write(i32 2)

define void @test1(ptr %p) {
entry:
  call void (...) @xd.ir.write(i32 0)
  br label %a

b:
  call void (...) @xd.ir.write(i32 2)
  ret void

a:
  call void (...) @xd.ir.write(i32 1)
  br label %b
}

declare void @xd.ir.write(...)
