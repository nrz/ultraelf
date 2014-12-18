;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defmethod emit ((x32-asm-instruction x32-asm-instruction) &rest args)
  (emit-with-format-and-operands-x32
    (slot-value x32-asm-instruction 'code-format)
    (slot-value x32-asm-instruction 'operands)
    args))

(defmethod emit-hex ((x32-asm-instruction x32-asm-instruction) &rest args)
  (print-hex
    (emit-with-format-and-operands-x32
      (slot-value x32-asm-instruction 'code-format)
      (slot-value x32-asm-instruction 'operands)
      args)))
