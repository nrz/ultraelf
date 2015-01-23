;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defmethod emit ((x64-asm-instruction x64-asm-instruction) &rest given-operands)
  (x64:emit-with-format-and-operands-x64
    (slot-value x64-asm-instruction 'code-format)
    (slot-value x64-asm-instruction 'req-operands)
    :given-operands given-operands
    :msg nil))

(defmethod emit-hex ((x64-asm-instruction x64-asm-instruction) &rest given-operands)
  (print-hex
    (x64:emit-with-format-and-operands-x64
      (slot-value x64-asm-instruction 'code-format)
      (slot-value x64-asm-instruction 'req-operands)
      :given-operands given-operands
      :msg nil)))
