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
     :initarg :name
     :reader name
     :initform (error "name must be specified")
     :documentation "The instruction mnemonic.")
   (operands
     :initarg :operands
     :reader operands
     :documentation "A string defining the operands of this instruction object.")
   (code-string
     :initarg :code-string
     :reader code-string
     :documentation "A string defining the code-string of this instruction object.")
   (arch-flags
     :initarg :arch-flags
     :reader arch-flags
     :documentation "A list defining the architecture flags of this instruction object.")
   (is-variant
     :initarg :is-variant
     :reader is-variant
     :initform nil
     :documentation "A variant contains the complete information for some encoding.")
   (is-container
     :initarg :is-container
     :reader is-container
     :initform nil
     :documentation "A container contains one or more variants, but it itself doesn't contain any encoding information.")
   (variants
     :initarg :variants
     :reader variants
     :documentation "A list of variants contained in a container. Each variant must be an object of 'asm-instruction class or of some its subclass.")
   (is-asm-instruction
     :reader is-asm-instruction
     :initform t)
   (is-x86-instruction
     :reader is-x86-instruction
     :initform nil)
   (is-arm-instruction
     :reader is-arm-instruction
     :initform nil)
   (emit
     :reader emit)
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

(defclass x86-asm-instruction (asm-instruction)
  ((is-x86-instruction
     :reader is-x86-instruction
     :initform t)))

(defclass x86-nop (x86-asm-instruction)
  ((emit
     :reader emit
     :initform #'nop-x86)))
