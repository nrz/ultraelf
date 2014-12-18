;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun convert-negative-byte-to-positive (my-byte)
  (cond
    ((not (integerp my-byte))
     (error "binary code bytes must be integers"))
    ((< my-byte -128)
     (error "binary code values must not be less than -128"))
    ((> my-byte 127)
     (error "binary code values must not be greater than 127"))
    ((< my-byte 0)
     (+ my-byte 256))
    (t my-byte)))
