;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :little-endian
  (:use :essentials)
  (:export
    :emit-number-in-n-bytes
    :emit-sign-extended-dword-for-n-bytes))
