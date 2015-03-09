;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defmethod emit ((x32-asm-instruction x32-asm-instruction) &rest given-operands)
  (x32:emit-with-format-and-operands-x32
    (slot-value x32-asm-instruction 'code-format)
    (slot-value x32-asm-instruction 'req-operands)
    :given-operands given-operands))

(defmethod emit-hex ((x32-asm-instruction x32-asm-instruction) &rest given-operands)
  (print-hex
    (x32:emit-with-format-and-operands-x32
      (slot-value x32-asm-instruction 'code-format)
      (slot-value x32-asm-instruction 'req-operands)
      :given-operands given-operands)))
