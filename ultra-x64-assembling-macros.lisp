;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :x64)

(defmacro emit-and-update-instruction-length (&body body)
  ;; This macro increments `instruction-length-in-bytes` by the size of `body`, and then returns `body`.
  `(progn
     (incf instruction-length-in-bytes (length ,@body))
     ,@body))
