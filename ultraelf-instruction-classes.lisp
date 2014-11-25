;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

;; an object will be created for each instruction based on the instruction data:
;; x86/x86-64: `insns.dat` from NASM source.
;; ARM:        `ARMTABLE.INC` from FASMARM source.

(defclass architecture ()
  ((is-x86-architecture
     :reader is-x86-architecture
     :initarg :is-x86-architecture
     :initform nil)
   (is-x16-architecture
     :reader is-x16-architecture
     :initarg :is-x16-architecture
     :initform nil)
   (is-x32-architecture
     :reader is-x32-architecture
     :initarg :is-x32-architecture
     :initform nil)
   (is-x64-architecture
     :reader is-x64-architecture
     :initarg :is-x64-architecture
     :initform nil)
   (is-arm-architecture
     :reader is-arm-architecture
     :initarg :is-arm-architecture
     :initform nil)))

(defclass x86-architecture (architecture)
  ((is-x86-architecture
     :reader is-x86-architecture
     :allocation :class
     :initarg :is-x86-architecture
     :initform t)))

(defclass x16-architecture (x86-architecture) ; 16-bit x86 architecture
  ((is-x16-architecture
     :reader is-x16-architecture
     :allocation :class
     :initarg :is-x16-architecture
     :initform t)))

(defclass x32-architecture (x86-architecture) ; 32-bit x86 architecture
  ((is-x32-architecture
     :reader is-x32-architecture
     :allocation :class
     :initarg :is-x32-architecture
     :initform t)))

(defclass x64-architecture (x86-architecture) ; 64-bit x86-64 architecture
  ((is-x64-architecture
     :reader is-x64-architecture
     :allocation :class
     :initarg :is-x64-architecture
     :initform t)))

(defclass arm-architecture (architecture) ; ARM architecture
  ((is-arm-architecture
     :reader is-arm-architecture
     :allocation :class
     :initarg :is-arm-architecture
     :initform t)))

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

(defclass x86-string-instruction (x86-asm-instruction argument)
  ((is-string-instruction
     :reader is-string-instruction
     :allocation :class
     :initform t
     :documentation "any string instruction")
   (op-code
     :initarg :op-code
     :reader op-code
     :initform (error "op-code must be specified"))))

(defclass x86-8-bit-string-instruction (x86-string-instruction)
  ((operand-size
     :reader operand-size
     :allocation :class
     :initform 8)))

(defclass x86-16-bit-string-instruction (x86-string-instruction)
  ((operand-size
     :reader operand-size
     :allocation :class
     :initform 16)))

(defclass x86-32-bit-string-instruction (x86-string-instruction)
  ((operand-size
     :reader operand-size
     :allocation :class
     :initform 32)))

(defclass x86-64-bit-string-instruction (x86-string-instruction)
  ((operand-size
     :reader operand-size
     :allocation :class
     :initform 64)))

(defgeneric emit (asm-instruction &rest args)
  (:documentation "emit instruction without rep/repz/repe/repnz/repne."))

(defgeneric emit-hex (asm-instruction &rest args)
  (:documentation "emit instruction without rep/repz/repe/repnz/repne and print in hexadecimal."))

(defgeneric rep (x86-string-instruction)
  (:documentation "emit rep xxx for the x86-string-instruction in question."))

(defgeneric repz (x86-string-instruction)
  (:documentation "emit repz xxx for the x86-string-instruction in question."))

(defgeneric repe (x86-string-instruction)
  (:documentation "emit repe xxx for the x86-string-instruction in question."))

(defgeneric repnz (x86-string-instruction)
  (:documentation "emit repnz xxx for the x86-string-instruction in question."))

(defgeneric repne (x86-string-instruction)
  (:documentation "emit repne xxx for the x86-string-instruction in question."))

(defgeneric my-string (argument)
  (:documentation "string that is converted to this instance."))

(defmethod emit ((x86-8-bit-string-instruction x86-8-bit-string-instruction) &rest args)
  (list (slot-value x86-8-bit-string-instruction 'op-code)))
(defmethod emit ((x86-16-bit-string-instruction x86-16-bit-string-instruction) &rest args)
  (list #x66 (slot-value x86-16-bit-string-instruction 'op-code)))
(defmethod emit ((x86-32-bit-string-instruction x86-32-bit-string-instruction) &rest args)
  (list (slot-value x86-32-bit-string-instruction 'op-code)))
(defmethod emit ((x86-64-bit-string-instruction x86-64-bit-string-instruction) &rest args)
  (append (emit-high-rex) (list (slot-value x86-64-bit-string-instruction 'op-code))))

(defmethod emit ((x64-asm-instruction x64-asm-instruction) &rest args)
  (emit-with-format-and-operands
    (slot-value x64-asm-instruction 'code-format)
    (slot-value x64-asm-instruction 'operands)
    args))

(defmethod emit-hex ((x64-asm-instruction x64-asm-instruction) &rest args)
  (print-hex
    (emit-with-format-and-operands
      (slot-value x64-asm-instruction 'code-format)
      (slot-value x64-asm-instruction 'operands)
      args)))

(defmethod rep ((x86-8-bit-string-instruction x86-8-bit-string-instruction))
  (list #xf3 (slot-value x86-8-bit-string-instruction 'op-code)))
(defmethod rep ((x86-16-bit-string-instruction x86-16-bit-string-instruction))
  (list #x66 #xf3 (slot-value x86-16-bit-string-instruction 'op-code)))
(defmethod rep ((x86-32-bit-string-instruction x86-32-bit-string-instruction))
  (list #xf3 (slot-value x86-32-bit-string-instruction 'op-code)))
(defmethod rep ((x86-64-bit-string-instruction x86-64-bit-string-instruction))
  (cons #xf3 (append (emit-high-rex) (list (slot-value x86-64-bit-string-instruction 'op-code)))))

(defmethod repz ((x86-8-bit-string-instruction x86-8-bit-string-instruction))
  (list #xf3 (slot-value x86-8-bit-string-instruction 'op-code)))
(defmethod repz ((x86-16-bit-string-instruction x86-16-bit-string-instruction))
  (list #x66 #xf3 (slot-value x86-16-bit-string-instruction 'op-code)))
(defmethod repz ((x86-32-bit-string-instruction x86-32-bit-string-instruction))
  (list #xf3 (slot-value x86-32-bit-string-instruction 'op-code)))
(defmethod repz ((x86-64-bit-string-instruction x86-64-bit-string-instruction))
  (cons #xf3 (append (emit-high-rex) (list (slot-value x86-64-bit-string-instruction 'op-code)))))

(defmethod repe ((x86-8-bit-string-instruction x86-8-bit-string-instruction))
  (list #xf3 (slot-value x86-8-bit-string-instruction 'op-code)))
(defmethod repe ((x86-16-bit-string-instruction x86-16-bit-string-instruction))
  (list #x66 #xf3 (slot-value x86-16-bit-string-instruction 'op-code)))
(defmethod repe ((x86-32-bit-string-instruction x86-32-bit-string-instruction))
  (list #xf3 (slot-value x86-32-bit-string-instruction 'op-code)))
(defmethod repe ((x86-64-bit-string-instruction x86-64-bit-string-instruction))
  (cons #xf3 (append (emit-high-rex) (list (slot-value x86-64-bit-string-instruction 'op-code)))))

(defmethod repnz ((x86-8-bit-string-instruction x86-8-bit-string-instruction))
  (list #xf2 (slot-value x86-8-bit-string-instruction 'op-code)))
(defmethod repnz ((x86-16-bit-string-instruction x86-16-bit-string-instruction))
  (list #x66 #xf2 (slot-value x86-16-bit-string-instruction 'op-code)))
(defmethod repnz ((x86-32-bit-string-instruction x86-32-bit-string-instruction))
  (list #xf2 (slot-value x86-32-bit-string-instruction 'op-code)))
(defmethod repnz ((x86-64-bit-string-instruction x86-64-bit-string-instruction))
  (cons #xf2 (append (emit-high-rex) (list (slot-value x86-64-bit-string-instruction 'op-code)))))

(defmethod repne ((x86-8-bit-string-instruction x86-8-bit-string-instruction))
  (list #xf2 (slot-value x86-8-bit-string-instruction 'op-code)))
(defmethod repne ((x86-16-bit-string-instruction x86-16-bit-string-instruction))
  (list #x66 #xf2 (slot-value x86-16-bit-string-instruction 'op-code)))
(defmethod repne ((x86-32-bit-string-instruction x86-32-bit-string-instruction))
  (list #xf2 (slot-value x86-32-bit-string-instruction 'op-code)))
(defmethod repne ((x86-64-bit-string-instruction x86-64-bit-string-instruction))
  (cons #xf2 (append (emit-high-rex) (list (slot-value x86-64-bit-string-instruction 'op-code)))))

(defmethod my-string ((argument argument))
  (slot-value argument 'name))
