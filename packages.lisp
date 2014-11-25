;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :ultraelf
  (:use :cl)
  (:shadow :type)
  (:export :asm-instruction
           :emit
           :emit-hex
           :arm-asm-instruction
           :name
           :operands
           :code-string
           :arch-flags
           :x16-asm-instruction
           :x32-asm-instruction
           :x64-asm-instruction))
