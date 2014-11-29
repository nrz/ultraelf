;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :x32
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
    :x32-asm-instruction
    ;; instruction classes' slots.
    :is-reg
    :reg-size
    :register-name
    :is-x86-register
    :is-old-reg
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
    ;; instruction classes' methods.
    :modrm.mod
    :modrm.r/m
    ;; x86 registers common to all x86.
    :al  :cl  :dl   :bl
    :ah  :ch  :dh   :bh
    :ax  :cx  :dx   :bx
    :sp  :bp  :si   :di
    ;; x32 registers.
    :eax :ecx :edx  :ebx
    :esp :ebp :esi  :edi
    ;; TODO: x32 register indirects.
    ;; :\[eax\] :\[ecx\] :\[edx\] :\[ebx\]
    ;; :\[esp\] :\[ebp\] :\[esi\] :\[edi\]
    ;; MMX registers.
    :mm0  :mm1  :mm2   :mm3   :mm4   :mm5   :mm6   :mm7
    ;; XMM registers.
    :xmm0 :xmm1 :xmm2  :xmm3  :xmm4  :xmm5  :xmm6  :xmm7
    ;; YMM registers.
    :ymm0 :ymm1 :ymm2  :ymm3  :ymm4  :ymm5  :ymm6  :ymm7
    ;; ZMM registers.
    :zmm0 :zmm1 :zmm2  :zmm3  :zmm4  :zmm5  :zmm6  :zmm7))
