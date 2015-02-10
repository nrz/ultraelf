;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :ultraelf
  (:use :cl)
  (:shadow :type)
  (:export
    ;; symbols imported by x86 common to x16, x32 and x64 begin here.
    ;; global variables.
    :*global-offset* :$
    ;; printing functions.
    :print-hex
    ;; list-handling functions.
    :get-list
    ;; emit-code functions.
    :emit
    :emit-hex
    :emit-modrm
    :emit-modrm-byte
    :emit-little-endian-number-in-n-bytes
    :emit-sign-extended-byte-for-n-bytes
    :create-syntax-tree
    :emit-binary-code-for-one-instruction
    :emit-binary-code-list
    :emit-binary-code
    :emit-binary-code-and-print-hex
    :assemble
    :assemble-and-print-hex
    :assemble-alternatives
    :assemble-alternatives-and-print-hex
    :check-args
    :get-all-encodings-for-syntax-tree
    :get-all-encodings-for-syntax-tree-and-print-hex
    :get-msg-bit
    ;; symbols.
    :convert-string-to-symbol-if-symbol-exists
    ;; instruction classes' slots.
    :req-operands
    :code-format
    :arch-flags
    ;; addressing form classes' slots.
    :allowed-targets
    :is-reg
    :reg-size
    :reg-name
    :is-x86-reg
    :is-memory-addressing
    :is-reg-indirect
    :is-immediate
    :value
    :fits-in-unsigned-byte
    :fits-in-unsigned-word
    :fits-in-signed-byte
    :fits-in-signed-word
    :displacement-size
    :needs-sib
    :r/m
    ;; addressing form classes' methods.
    :modrm-mod
    :modrm-reg
    :modrm-r/m
    ;; x86 registers common to all x86.
    :al :cl :dl :bl
    :ah :ch :dh :bh
    :ax :cx :dx :bx
    :sp :bp :si :di
    ;; symbols imported by x86 common to x16, x32 and x64 end here.
    ;; symbols imported by x86-modern (common tox32 and x64) begin here.
    ;; x32/x64 registers.
    :eax :ecx :edx  :ebx
    :esp :ebp :esi  :edi
    ;; instruction classes' slots.
    :is-mmx-reg
    :is-xmm-reg
    :is-ymm-reg
    :is-zmm-reg
    ;; MMX registers.
    :mm0  :mm1  :mm2   :mm3   :mm4   :mm5   :mm6   :mm7
    ;; XMM registers.
    :xmm0 :xmm1 :xmm2  :xmm3  :xmm4  :xmm5  :xmm6  :xmm7
    ;; YMM registers.
    :ymm0 :ymm1 :ymm2  :ymm3  :ymm4  :ymm5  :ymm6  :ymm7
    ;; ZMM registers.
    :zmm0 :zmm1 :zmm2  :zmm3  :zmm4  :zmm5  :zmm6  :zmm7
    ;; symbols imported by x86-modern (common tox32 and x64) end here.
    ;; symbols available only to x64 begin here.
    ;; addressing form classes' slots.
    :is-old-reg
    :is-new-reg
    :works-with-rex
    :needs-rex
    :rex-w
    :rex-r
    :rex-x
    :rex-b
    ;; TODO: x16 register indirects. availabte only to x16.
    ;; :\[bx\] :\[bp\] :\[si\] :\[di\] ...
    ;; TODO: x32 register indirects. availabte only to x32.
    ;; :\[eax\] :\[ecx\] :\[edx\] :\[ebx\]
    ;; :\[esp\] :\[ebp\] :\[esi\] :\[edi\]
    ;; x64 registers.
    :rax :rcx :rdx  :rbx
    :rsp :rbp :rsi  :rdi
    :r8  :r9  :r10  :r11 :r12 :r13 :r14 :r15
    :spl :bpl :sil  :dil
    :r8b :r9b :r10b :r11b :r12b :r13b :r14b :r15b
    :r8w :r9w :r10w :r11w :r12w :r13w :r14w :r15w
    :r8d :r9d :r10d :r11d :r12d :r13d :r14d :r15d
    ;; x64 register indirects. availabte only to x64.
    :\[rax\] :\[rcx\] :\[rdx\] :\[rbx\]
    :\[rsp\] :\[rbp\] :\[rsi\] :\[rdi\]
    :\[r8\]  :\[r9\]  :\[r10]  :\[r11]  :\[r12] :\[r13] :\[r14] :\[r15\]
    ;; XMM registers. availabte only to x64.
    :xmm8 :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15
    ;; YMM registers. availabte only to x64.
    :ymm8 :ymm9 :ymm10 :ymm11 :ymm12 :ymm13 :ymm14 :ymm15
    ;; ZMM registers. availabte only to x64.
    :zmm8 :zmm9 :zmm10 :zmm11 :zmm12 :zmm13 :zmm14 :zmm15
    ;; symbols available only to x64 end here.
    ;; instruction class names.
    :x16-asm-instruction   ; available only to x16.
    :x32-asm-instruction   ; available only to x32.
    :x64-asm-instruction   ; available only to x64.
    :arm-asm-instruction)) ; available only to arm.
