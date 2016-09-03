;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defmethod emit ((x16-asm-instruction x16-asm-instruction) given-operands)
  (x16:emit-with-format-and-operands-x16
    (slot-value x16-asm-instruction 'code-format)
    (slot-value x16-asm-instruction 'req-operands)
    :given-operands given-operands))
