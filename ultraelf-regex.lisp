;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun convert-list-of-strings-to-regex (my-list)
  "This function converts a list of strings to a regex."
  (format nil "(~{~A~^|~})" my-list))
