;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defclass arm-asm-instruction (arm-architecture asm-instruction)
  ((is-arm-asm-instruction
     :reader is-arm-asm-instruction
     :allocation :class
     :initform t)))

(defmethod emit ((arm-asm-instruction arm-asm-instruction) &rest args)
  (arm:emit-with-format-and-operands-arm
    (slot-value arm-asm-instruction 'code-format)
    (slot-value arm-asm-instruction 'req-operands)
    :args args
    :msg nil))

(defmethod emit-hex ((arm-asm-instruction arm-asm-instruction) &rest args)
  (print-hex
    (arm:emit-with-format-and-operands-arm
      (slot-value arm-asm-instruction 'code-format)
      (slot-value arm-asm-instruction 'req-operands)
      :args args
      :msg nil)))
