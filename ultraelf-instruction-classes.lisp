;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

;; an object will be created for each instruction based on the instruction data:
;; x86/x86-64: `insns.dat` from NASM source.
;; ARM:        `ARMTABLE.INC` from FASMARM source.

(defclass asm-instruction ()
  ((name
     :reader name
     :initarg :name
     :initform (error "name must be specified")
     :documentation "The instruction mnemonic.")
   (operands
     :reader operands
     :initarg :operands
     :documentation "A string defining the operands of this instruction object.")
   (code-format
     :reader code-format
     :initarg :code-format
     :documentation "A list defining the code format of this instruction object.")
   (arch-flags
     :reader arch-flags
     :initarg :arch-flags
     :documentation "A list defining the architecture flags of this instruction object.")
   (variants
     :accessor variants
     :allocation :class
     :initarg :variants
     :documentation "A list of variants contained in a container. Each variant must be an object of 'asm-instruction class or of some its subclass.")
   (is-asm-instruction
     :reader is-asm-instruction
     :allocation :class
     :initform t)
   (is-x86-instruction
     :reader is-x86-instruction
     :initform nil)
   (is-x16-instruction
     :reader is-x16-instruction
     :initform nil)
   (is-x32-instruction
     :reader is-x32-instruction
     :initform nil)
   (is-x64-instruction
     :reader is-x64-instruction
     :initform nil)
   (is-arm-instruction
     :reader is-arm-instruction
     :initform nil)
   (modifies-flags
     :reader modifies-flags
     :initform t
     :documentation "By default all instructions modify flags, but the the precise flags modified (flags-affected) is set to nil.")
   (flags-affected
     :reader flags-affected
     :initform nil
     :documentation "This must be set to a list of strings of modified flags.")
   (flags-zeroed
     :reader flags-zeroed
     :initform nil
     :documentation "This must be set to a list of strings of zeroed flags.")
   (flags-set
     :reader flags-set
     :initform nil
     :documentation "This must be set to a list of strings of flags set to 1.")
   (flags-undefined
     :reader flags-undefined
     :initform nil
     :documentation "This must be set to a list of strings of undefined flags.")
   (depends-on-flags
     :reader depends-on-flags
     :initform nil
     :documentation "For conditional jumps, conditional moves, `lahf` & `pushf` this must be set to `t`.")
   (flags-depends-on
     :reader flags-depends-on
     :initform nil
     :documentation "For conditional jumps, conditional moves, `lahf` & `pushf` this must be set to a list of strings of flags the instruction depends on.")
   (depends-on-stack
     :reader depends-on-stack
     :initform nil
     :documentation "For instructions that depend on the main CPU stack (`iret`, `leave`, `pop`, `popf`, `ret`) this must be set to `t`.")
   (alt-code
     :reader alt-code
     :initform nil)))

(defclass arm-asm-instruction (arm-architecture asm-instruction)
  ((is-arm-asm-instruction
     :reader is-arm-asm-instruction
     :initform t)))

(defclass x86-asm-instruction (x86-architecture asm-instruction)
  ((is-x86-asm-instruction
     :reader is-x86-asm-instruction
     :allocation :class
     :initform t)))

(defclass x16-asm-instruction (x16-architecture asm-instruction)
  ((is-x16-asm-instruction
     :reader is-x16-asm-instruction
     :allocation :class
     :initform t)))

(defclass x32-asm-instruction (x32-architecture asm-instruction)
  ((is-x32-asm-instruction
     :reader is-x32-asm-instruction
     :allocation :class
     :initform t)))

(defclass x64-asm-instruction (x64-architecture asm-instruction)
  ((is-x64-asm-instruction
     :reader is-x64-asm-instruction
     :allocation :class
     :initform t)))

(defclass arm-asm-instruction (arm-architecture asm-instruction)
  ((is-arm-asm-instruction
     :reader is-arm-asm-instruction
     :initform t)))

(defgeneric emit (asm-instruction &rest args)
  (:documentation "emit instruction without rep/repz/repe/repnz/repne."))

(defgeneric emit-hex (asm-instruction &rest args)
  (:documentation "emit instruction without rep/repz/repe/repnz/repne and print in hexadecimal."))

(defmethod emit ((x16-asm-instruction x16-asm-instruction) &rest args)
  (emit-with-format-and-operands-x16
    (slot-value x16-asm-instruction 'code-format)
    (slot-value x16-asm-instruction 'operands)
    args))

(defmethod emit-hex ((x16-asm-instruction x16-asm-instruction) &rest args)
  (print-hex
    (emit-with-format-and-operands-x16
      (slot-value x16-asm-instruction 'code-format)
      (slot-value x16-asm-instruction 'operands)
      args)))

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

(defmethod emit ((x64-asm-instruction x64-asm-instruction) &rest args)
  (emit-with-format-and-operands-x64
    (slot-value x64-asm-instruction 'code-format)
    (slot-value x64-asm-instruction 'operands)
    args))

(defmethod emit-hex ((x64-asm-instruction x64-asm-instruction) &rest args)
  (print-hex
    (emit-with-format-and-operands-x64
      (slot-value x64-asm-instruction 'code-format)
      (slot-value x64-asm-instruction 'operands)
      args)))

(defmethod emit ((arm-asm-instruction arm-asm-instruction) &rest args)
  (emit-with-format-and-operands-arm
    (slot-value arm-asm-instruction 'code-format)
    (slot-value arm-asm-instruction 'operands)
    args))

(defmethod emit-hex ((arm-asm-instruction arm-asm-instruction) &rest args)
  (print-hex
    (emit-with-format-and-operands-arm
      (slot-value arm-asm-instruction 'code-format)
      (slot-value arm-asm-instruction 'operands)
      args)))
