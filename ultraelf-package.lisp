;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :ultraelf
  (:use :cl)
  (:shadow :type)
  (:export
    :print-hex
    :emit
    :emit-hex
    :code-format
    :operands
    :is-reg
    :reg-size
    :register-name
    :is-x86-register
    :is-old-reg
    :is-new-reg
    :is-mmx-reg
    :is-xmm-reg
    :is-ymm-reg
    :is-zmm-reg
    :is-memory-addressing
    :is-register-indirect
    :displacement-size
    :needs-sib
    :modrm.mod
    :modrm.r/m
    :r/m
    :works-with-rex
    :needs-nex
    :rex.r
    :rex.b
    :x64-asm-instruction
    :al  :cl  :dl   :bl
    :ah  :ch  :dh   :bh
    :spl :bpl :sil  :dil
    :r8b :r9b :r10b :r11b :r12b :r13b :r14b :r15b
    :ax  :cx  :dx   :bx
    :sp  :bp  :si   :di
    :r8w :r9w :r10w :r11w :r12w :r13w :r14w :r15w
    :eax :ecx :edx  :ebx
    :esp :ebp :esi  :edi
    :r8d :r9d :r10d :r11d :r12d :r13d :r14d :r15d
    :rax :rcx :rdx  :rbx
    :rsp :rbp :rsi  :rdi
    :r8  :r9  :r10  :r11 :r12 :r13 :r14 :r15
    :\[rax\] :\[rcx\] :\[rdx\] :\[rbx\]
    :\[rsp\] :\[rbp\] :\[rsi\] :\[rdi\]
    :\[r8\]  :\[r9\]  :\[r10]  :\[r11]  :\[r12] :\[r13] :\[r14] :\[r15\]
    :mm0  :mm1  :mm2   :mm3   :mm4   :mm5   :mm6   :mm7
    :xmm0 :xmm1 :xmm2  :xmm3  :xmm4  :xmm5  :xmm6  :xmm7
    :xmm8 :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15
    :ymm0 :ymm1 :ymm2  :ymm3  :ymm4  :ymm5  :ymm6  :ymm7
    :ymm8 :ymm9 :ymm10 :ymm11 :ymm12 :ymm13 :ymm14 :ymm15
    :zmm0 :zmm1 :zmm2  :zmm3  :zmm4  :zmm5  :zmm6  :zmm7
    :zmm8 :zmm9 :zmm10 :zmm11 :zmm12 :zmm13 :zmm14 :zmm15))
