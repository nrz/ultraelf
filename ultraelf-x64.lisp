;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :x64
  (:use :x86-modern :essentials)
  (:import-from
    :ultraelf
    ;; addressing form classes' slots.
    :is-old-reg
    :is-new-reg
    :works-with-rex
    :needs-rex
    :rex.r
    :rex.b
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
    ;; XMM registers.
    :xmm8 :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15
    ;; YMM registers.
    :ymm8 :ymm9 :ymm10 :ymm11 :ymm12 :ymm13 :ymm14 :ymm15
    ;; ZMM registers.
    :zmm8 :zmm9 :zmm10 :zmm11 :zmm12 :zmm13 :zmm14 :zmm15
    ;; instruction class names.
    :x64-asm-instruction))
