;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defclass x86-asm-instruction (x86-architecture asm-instruction)
  ((is-x86-asm-instruction
     :reader is-x86-asm-instruction
     :allocation :class
     :initform t)))

(defclass x16-asm-instruction (x16-architecture asm-instruction)
  ((is-x16-asm-instruction
     :reader is-x16-asm-instruction
     :allocation :class
     :initform t)))

(defclass x32-asm-instruction (x32-architecture asm-instruction)
  ((is-x32-asm-instruction
     :reader is-x32-asm-instruction
     :allocation :class
     :initform t)))

(defclass x64-asm-instruction (x64-architecture asm-instruction)
  ((is-x64-asm-instruction
     :reader is-x64-asm-instruction
     :allocation :class
     :initform t)))
