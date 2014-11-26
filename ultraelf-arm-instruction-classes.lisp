;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :arm)

(defclass arm-architecture (architecture) ; ARM architecture
  ((is-arm-architecture
     :reader is-arm-architecture
     :allocation :class
     :initarg :is-arm-architecture
     :initform t)))

(defclass arm-asm-instruction (arm-architecture asm-instruction)
  ((is-arm-asm-instruction
     :reader is-arm-asm-instruction
     :initform t)))
