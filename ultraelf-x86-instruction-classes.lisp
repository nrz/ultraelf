;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defclass x86-architecture (architecture)
  ((is-x86-architecture
     :reader is-x86-architecture
     :allocation :class
     :initarg :is-x86-architecture
     :initform t)))

(defclass x16-architecture (x86-architecture) ; 16-bit x86 architecture
  ((is-x16-architecture
     :reader is-x16-architecture
     :allocation :class
     :initarg :is-x16-architecture
     :initform t)))

(defclass x32-architecture (x86-architecture) ; 32-bit x86 architecture
  ((is-x32-architecture
     :reader is-x32-architecture
     :allocation :class
     :initarg :is-x32-architecture
     :initform t)))

(defclass x64-architecture (x86-architecture) ; 64-bit x86-64 architecture
  ((is-x64-architecture
     :reader is-x64-architecture
     :allocation :class
     :initarg :is-x64-architecture
     :initform t)))

(defclass arm-architecture (architecture) ; ARM architecture
  ((is-arm-architecture
     :reader is-arm-architecture
     :allocation :class
     :initarg :is-arm-architecture
     :initform t)))

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
