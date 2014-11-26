;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :ultraelf
  (:use :ultraelf)
  (:export
    :asm-instruction
    :emit
    :emit-hex
    :name
    :operands
    :code-format
    :arch-flags
    :arch-flags
    :is-x86-architecture
    :is-x16-architecture
    :is-x32-architecture
    :is-x64-architecture
    :is-arm-architecture
    :is-asm-instruction
    :is-x86-instruction
    :is-x16-instruction
    :is-x32-instruction
    :is-x64-instruction
    :is-arm-instruction
    :modifies-flags
    :flags-affected
    :flags-zeroed
    :flags-set
    :flags-undefined
    :depends-on-flags
    :flags-depends-on
    :depends-on-stack
    :op-code
    :alt-code))
