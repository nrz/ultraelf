;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :x16
  (:use :x86 :little-endian :essentials)
  (:import-from
    :cl
    :nil)
  (:import-from
    :ultraelf
    ;; TODO: x16 register indirects.
    ;; :\[bx\] :\[bp\] :\[si\] :\[di\] ...
    ;; instruction class names.
    :x16-asm-instruction)
  (:export
    :emit-with-format-and-operands-x16))
