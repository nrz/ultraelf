;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun get-list-without-last-element (my-list)
  "This function returns a list without the last element of the input list."
  (loop for element on my-list
        while (rest element)
        collect (first element)))
