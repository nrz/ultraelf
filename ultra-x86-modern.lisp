;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :x86-modern
  (:use :x86)
  (:import-from
    :ultraelf
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
    :zmm0 :zmm1 :zmm2  :zmm3  :zmm4  :zmm5  :zmm6  :zmm7))

(in-package :x86-modern)
(cl-user::do-symbols (x86-modern-sym (cl-user::find-package :x86-modern)) (cl-user::export x86-modern-sym))
