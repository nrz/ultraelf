;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :big-endian)

(defun emit-number-in-n-bytes (my-number n-bytes)
  (nreverse (ultraelf:emit-little-endian-number-in-n-bytes my-number n-bytes)))
