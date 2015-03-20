;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

;; an object will be created for each instruction based on the instruction data:
;; x86/x86-64: `insns.dat` from NASM source.
;; ARM:        `ARMTABLE.INC` from FASMARM source.
;;
;; ultraELF additions to NASM syntax (these are higher-level code):
;; `add!`    functionally equivalent to `add`, but allows also `add! [mem],[mem]`.
;; `adc!`    functionally equivalent to `adc`, but allows also `adc! [mem],[mem]`.
;; `cmovcc!` functionally equivalent to `cmovcc`, but allows also `cmovcc! [mem],[mem]`, works for any condicion `cc`.
;; `mov!`    functionally equivalent to `mov`, but allows also `mov! [mem],[mem]`.
;; `neg!`    functionally equivalent to `neg`.
;; `not!`    functionally equivalent to `not`.
;; `pop!`    functionally equivalent to `pop`.
;; `push!`   functionally equivalent to `push`.
;; `sbb!`    functionally equivalent to `sbb`, but allows also `sbb! [mem],[mem]`.
;; `sub!`    functionally equivalent to `sub`, but allows also `sub! [mem],[mem]`.
;; `xor!`    functionally equivalent to `xor`, but allows also `xor! [mem],[mem]`.
;; `xchg!`   functionally equivalent to `xchg`, but allows also `xchg! [mem],[mem]`.
;; `setcc!`  functionally equivalent to `setcc`, but allows also `setcc! r/m`, works for any condicion `cc`.
;; `set0!`   sets arg1 to 0.
;; `set1!`   sets arg1 to +1.
;; `set+1!`  sets arg1 to +1.
;; `set-1!`  sets arg1 to -1.
;; `set!`    sets arg1 to arg2.
;; `jmp?`    jumps to next instruction if the blocks are not consecutive in binary code.

(defclass asm-instruction ()
  ((name
     :reader name
     :initarg :name
     :initform (error "name must be specified")
     :documentation "The instruction mnemonic.")
   (req-operands
     :reader req-operands
     :initarg :req-operands
     :documentation "A string defining the operands of this instruction object.")
   (code-format
     :reader code-format
     :initarg :code-format
     :documentation "A list defining the code format of this instruction object.")
   (arch-flags
     :reader arch-flags
     :initarg :arch-flags
     :documentation "A list defining the architecture flags of this instruction object.")
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

(defgeneric emit (asm-instruction &rest given-operands)
  (:documentation "emit instruction without rep/repz/repe/repnz/repne."))

(defgeneric emit-hex (asm-instruction &rest given-operands)
  (:documentation "emit instruction without rep/repz/repe/repnz/repne and print in hexadecimal."))
