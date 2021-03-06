;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :little-endian)

(defun emit-number-in-n-bytes (my-number n-bytes)
  (ultraelf:emit-little-endian-number-in-n-bytes my-number n-bytes))

(defun emit-sign-extended-dword-for-n-bytes (my-number n-bytes)
  (ultraelf:emit-little-endian-sign-extended-dword-for-n-bytes my-number n-bytes))
