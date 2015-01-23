;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :x32)

(defun emit-with-format-and-operands-x32 (code-format req-operands &rest given-operands)
  "This function emits code (list of binary code bytes) for one x32 instruction variant."
  (error "x32 encoding not yet implemented"))
