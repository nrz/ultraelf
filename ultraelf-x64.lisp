;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :x64
  (:use :essentials)
  (:import-from
    :ultraelf
    ;; printing functions.
    :print-hex
    ;; list-handling functions.
    :get-list
    ;; emit-code functions.
    :emit
    :emit-hex
    :emit-modrm
    :emit-modrm-byte
    :assemble
    :assemble-alternatives
    :check-args
    :get-all-encodings-for-syntax-tree
    ;; instruction class names.
    :x64-asm-instruction
    ;; instruction classes' slots.
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
    :allowed-targets
    :code-format
    :operands
    :needs-sib
    :r/m
    :works-with-rex
    :needs-rex
    :rex.r
    :rex.b
    ;; instruction classes' methods.
    :modrm.mod
    :modrm.r/m
    ;; x86 registers common to all x86.
    :al  :cl  :dl   :bl
    :ah  :ch  :dh   :bh
    :ax  :cx  :dx   :bx
    :sp  :bp  :si   :di
    ;; x32/x64 registers.
    :eax :ecx :edx  :ebx
    :esp :ebp :esi  :edi
    ;; x64 registers.
    :rax :rcx :rdx  :rbx
    :rsp :rbp :rsi  :rdi
    :r8  :r9  :r10  :r11 :r12 :r13 :r14 :r15
    :spl :bpl :sil  :dil
    :r8b :r9b :r10b :r11b :r12b :r13b :r14b :r15b
    :r8w :r9w :r10w :r11w :r12w :r13w :r14w :r15w
    :r8d :r9d :r10d :r11d :r12d :r13d :r14d :r15d
    ;; x64 register indirects.
    :\[rax\] :\[rcx\] :\[rdx\] :\[rbx\]
    :\[rsp\] :\[rbp\] :\[rsi\] :\[rdi\]
    :\[r8\]  :\[r9\]  :\[r10]  :\[r11]  :\[r12] :\[r13] :\[r14] :\[r15\]
    ;; MMX registers.
    :mm0  :mm1  :mm2   :mm3   :mm4   :mm5   :mm6   :mm7
    ;; XMM registers.
    :xmm0 :xmm1 :xmm2  :xmm3  :xmm4  :xmm5  :xmm6  :xmm7
    :xmm8 :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15
    ;; YMM registers.
    :ymm0 :ymm1 :ymm2  :ymm3  :ymm4  :ymm5  :ymm6  :ymm7
    :ymm8 :ymm9 :ymm10 :ymm11 :ymm12 :ymm13 :ymm14 :ymm15
    ;; ZMM registers.
    :zmm0 :zmm1 :zmm2  :zmm3  :zmm4  :zmm5  :zmm6  :zmm7
    :zmm8 :zmm9 :zmm10 :zmm11 :zmm12 :zmm13 :zmm14 :zmm15))
