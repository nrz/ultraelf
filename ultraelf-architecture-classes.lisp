;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

;; an object will be created for each instruction based on the instruction data:
;; x86/x86-64: `insns.dat` from NASM source.
;; ARM:        `ARMTABLE.INC` from FASMARM source.

(defclass architecture ()
  ((is-x86-architecture
     :reader is-x86-architecture
     :initarg :is-x86-architecture
     :initform nil)
   (is-x16-architecture
     :reader is-x16-architecture
     :initarg :is-x16-architecture
     :initform nil)
   (is-x32-architecture
     :reader is-x32-architecture
     :initarg :is-x32-architecture
     :initform nil)
   (is-x64-architecture
     :reader is-x64-architecture
     :initarg :is-x64-architecture
     :initform nil)
   (is-arm-architecture
     :reader is-arm-architecture
     :initarg :is-arm-architecture
     :initform nil)))

(defclass arm-architecture (architecture) ; ARM architecture
  ((is-arm-architecture
     :reader is-arm-architecture
     :allocation :class
     :initarg :is-arm-architecture
     :initform t)))

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
