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
   (is-reg
     :reader is-reg
     :allocation :class
     :initform nil)
   (is-memory-addressing
     :reader is-memory-addressing
     :allocation :class
     :initform nil)
   (is-abs-addressing
     :reader is-abs-addressing
     :allocation :class
     :initform nil)
   (is-pc-relative
     :reader is-pc-relative
     :allocation :class
     :initform nil)
   (is-reg-indirect
     :reader is-reg-indirect
     :allocation :class
     :initform nil)
   (is-immediate
     :reader is-immediate
     :allocation :class
     :initform nil)
   (is-string
     :reader is-string
     :allocation :class
     :initform nil)
   (needs-rex
     :reader needs-rex
     :allocation :class
     :initform nil)
   (works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform nil)
   (is-string-instruction
     :reader is-string-instruction
     :allocation :class
     :initform nil)
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
     :documentation "the unknown string must be given as a name")))

(defclass immediate (argument)
  ((name
     :initarg :name
     :accessor name
     :initform nil)
   (is-immediate
     :reader is-immediate
     :allocation :class
     :initform t)
   (allowed-targets
     :reader allowed-targets
     :initform (list "imm" "imm|short" "imm|near" "imm|far"
                     "imm8"
                     "imm16" "imm16|short" "imm16|near" "imm16|far"
                     "imm32" "imm32|short" "imm32|near" "imm32|far"
                     "imm64" "imm64|short" "imm64|near" "imm64|far"
                     "imm:imm" "imm16:imm" "imm:imm16" "imm32:imm" "imm:imm32")
     :documentation "allowed encodings in NASM's `insns.dat` syntax")
   (value
     :accessor value
     :initform nil)))

(defgeneric my-string (argument)
  (:documentation "string that is converted to this instance."))

(defgeneric fits-in-unsigned-byte (immediate)
  (:documentation "is this value in the range [0,255]?"))

(defgeneric fits-in-unsigned-word (immediate)
  (:documentation "is this value in the range [0,65535"))

(defgeneric fits-in-signed-byte (immediate)
  (:documentation "is this value in the range [-128,127]?"))

(defgeneric fits-in-signed-word (immediate)
  (:documentation "is this value in the range [-32768,32767"))

(defmethod my-string ((argument argument))
  (slot-value argument 'name))

(defmethod fits-in-unsigned-byte ((immediate immediate))
  (and
    (>= (slot-value immediate 'value) 0)
    (<= (slot-value immediate 'value) 255)))

(defmethod fits-in-unsigned-word ((immediate immediate))
  (and
    (>= (slot-value immediate 'value) 0)
    (<= (slot-value immediate 'value) 65535)))

(defmethod fits-in-signed-byte ((immediate immediate))
  (and
    (>= (slot-value immediate 'value) -128)
    (<= (slot-value immediate 'value) 127)))

(defmethod fits-in-signed-word ((immediate immediate))
  (and
    (>= (slot-value immediate 'value) -32768)
    (<= (slot-value immediate 'value) 32767)))
