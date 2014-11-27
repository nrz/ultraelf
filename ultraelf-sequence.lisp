;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun get-list (my-list)
  "This function goes through list wrapped in a list... until the last list is found.
   Works nice for &rest args passed through one or more functions."
  (if (and
        (listp (first my-list))
        (not (null my-list)))
    (get-list (first my-list))
    my-list))
