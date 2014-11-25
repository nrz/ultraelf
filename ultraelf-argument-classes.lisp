;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defclass argument ()
  ((name
     :initarg :name
     :reader name
     :initform nil
     :documentation "any argument to an instruction")
   (value
     :initarg :value
     :reader value
     :initform nil
     :documentation "numeric value of the argument. may be integer or real. must be converted into bytes as needed.")))

(defclass unknown (argument)
  ((is-unknown
     :reader is-unknown
     :allocation :class
     :initform t)
   (name
     :initarg :name
     :reader name
     :initform (error "name must be specified")
     :documentation "the unknown string must be given as a name")
   (is-reg
     :reader is-reg
     :initform nil
     :allocation :class
     :documentation "registers are _not_ unknown.")
   (is-register-indirect
     :reader is-register-indirect
     :allocation :class
     :initform nil
     :documentation "register indirects are _not_ unknown.")
   (is-address
     :reader is-address
     :allocation :class
     :initform nil
     :documentation "addresses indirects are _not_ unknown (although their values may be).")
   (is-string-instruction
     :reader is-string-instruction
     :allocation :class
     :initform nil
     :documentation "string instructions are _not_ unknown.")))

(defclass address (argument)
  ((name
     :initarg :name
     :accessor name
     :initform nil)
   (is-unknown
     :reader is-unknown
     :initform nil
     :allocation :class
     :documentation "Addresses are not unknown, although their value may be unknown.")
   (is-reg
     :reader is-reg
     :allocation :class
     :initform nil)
   (is-register-indirect
     :reader is-register-indirect
     :allocation :class
     :initform nil)
   (is-address
     :reader is-address
     :allocation :class
     :initform t)
   (is-string-instruction
     :reader is-string-instruction
     :allocation :class
     :initform nil)
   (value
     :accessor value
     :initform nil)))

(defclass x86-string-instruction (argument)
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

(defgeneric emit (x86-string-instruction)
  (:documentation "emit instruction without rep/repz/repe/repnz/repne."))

(defmethod emit ((x86-8-bit-string-instruction x86-8-bit-string-instruction))
  (list (slot-value x86-8-bit-string-instruction 'op-code)))
(defmethod emit ((x86-16-bit-string-instruction x86-16-bit-string-instruction))
  (list #x66 (slot-value x86-16-bit-string-instruction 'op-code)))
(defmethod emit ((x86-32-bit-string-instruction x86-32-bit-string-instruction))
  (list (slot-value x86-32-bit-string-instruction 'op-code)))
(defmethod emit ((x86-64-bit-string-instruction x86-64-bit-string-instruction))
  (append (emit-high-rex) (list (slot-value x86-64-bit-string-instruction 'op-code))))

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

(defgeneric my-string (argument)
  (:documentation "string that is converted to this instance."))

(defmethod my-string ((argument argument))
  (slot-value argument 'name))
