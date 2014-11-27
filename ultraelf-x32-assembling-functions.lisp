;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun emit-with-format-and-operands-x32 (code-format operands &rest args)
  "This function emits code (list of binary code bytes) for one x32 instruction variant."
  (error "x32 encoding not yet implemented"))
