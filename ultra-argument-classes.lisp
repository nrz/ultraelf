;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
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
   (is-reg-indirect
     :reader is-reg-indirect
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
   (is-reg-indirect
     :reader is-reg-indirect
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

(defgeneric my-string (argument)
  (:documentation "string that is converted to this instance."))

(defmethod my-string ((argument argument))
  (slot-value argument 'name))
