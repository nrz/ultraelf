;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun bits-pseudo-op (arg1 &rest args)
  "This pseudo-op sets *bits* according to what is requested.
   Example usage: [bits 64]"
  (defparameter *bits* (parse-integer (get-string-without-last-character arg1)))
  nil)
