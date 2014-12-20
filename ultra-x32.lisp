;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :x32
  (:use :x86-modern :essentials)
  (:import-from
    :cl
    :nil)
  (:import-from
    :ultraelf
    ;; TODO: x32 register indirects.
    ;; :\[eax\] :\[ecx\] :\[edx\] :\[ebx\]
    ;; :\[esp\] :\[ebp\] :\[esi\] :\[edi\]
    ;; instruction class names.
    :x32-asm-instruction)
  (:export
    :emit-with-format-and-operands-x32))
