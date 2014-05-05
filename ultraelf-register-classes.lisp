;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defclass register ()
  ((register-name
     :accessor register-name
     :documentation "any CPU-register")))

(defclass x86-register (register)
  ((is-x86-register
     :reader is-x86-register
     :initform t)))

(defclass x86-old-register (x86-register)
  ((is-old-register
     :reader is-old-register
     :initform t
     :documentation "old registers are all registers which can be accessed without REX")
   (is-new-register
     :reader is-new-register
     :initform nil
     :documentation "new registers are all registers which can _not_ be accessed without REX")))

(defclass x86-new-register (x86-register)
  ((is-old-register
     :reader is-old-register
     :initform nil
     :documentation "old registers are all registers which can be accessed without REX")
   (is-new-register
     :reader is-new-register
     :initform t
     :documentation "new registers are all registers which can _not_ be accessed without REX")))

(defclass x86-8-bits-register (x86-register)
  ((register-size
     :reader register-size
     :initform 8
     :documentation "register size in bits")))

(defclass x86-16-bits-register (x86-register)
  ((register-size
     :reader register-size
     :initform 16
     :documentation "register size in bits")))

(defclass x86-32-bits-register (x86-register)
  ((register-size
     :reader register-size
     :initform 32
     :documentation "register size in bits")))

(defclass x86-64-bits-register (x86-register)
  ((register-size
     :reader register-size
     :initform 64
     :documentation "register size in bits")))

(defclass x86-old-8-bits-low-register (x86-old-register x86-16-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :initform nil
     :documentation "REX is not needed")))

(defclass x86-old-8-bits-high-register (x86-old-register x86-16-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :initform nil
     :documentation "can _not_ be used with REX")
   (needs-rex
     :reader needs-rex
     :initform nil
     :documentation "REX is not needed")))

(defclass x86-old-16-bits-register (x86-old-register x86-16-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :initform nil
     :documentation "REX is not needed")))

(defclass x86-old-32-bits-register (x86-old-register x86-32-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :initform nil
     :documentation "REX is not needed")))

(defclass x86-old-64-bits-register (x86-old-register x86-64-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :initform t
     :documentation "REX is needed (but see special cases such as jmp!)")))

(defclass x86-new-8-bits-register (x86-new-register x86-8-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :initform t
     :documentation "REX is needed")))

(defclass x86-new-16-bits-register (x86-new-register x86-16-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :initform t
     :documentation "REX is needed")))

(defclass x86-new-32-bits-register (x86-new-register x86-32-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :initform t
     :documentation "REX is needed")))

(defclass x86-new-64-bits-register (x86-new-register x86-64-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :initform t
     :documentation "REX is needed")))

(defparameter *old-low-reg8-list*  (list  "al"  "cl"  "dl"  "bl"))
(defparameter *old-high-reg8-list* (list  "ah"  "ch"  "dh"  "bh"))
(defparameter *old-reg16-list*     (list  "ax"  "cx"  "dx"  "bx"  "sp"  "bp"  "si"  "di"))
(defparameter *old-reg32-list*     (list "eax" "ecx" "edx" "ebx" "esp" "ebp" "esi" "edi"))
(defparameter *old-reg64-list*     (list "rax" "rcx" "rdx" "rbx" "rsp" "rbp" "rsi" "rdi"))

(defparameter *new-low-reg8-list*  (list "spl" "bpl"  "sil"  "dil"  "r8b"  "r9b" "r10b" "r11b" "r12b" "r13b" "r14b" "r15b"))
(defparameter *new-reg16-list*     (list "r8w" "r9w" "r10w" "r11w" "r12w" "r13w" "r14w" "r15w"))
(defparameter *new-reg32-list*     (list "r8d" "r9d" "r10d" "r11d" "r12d" "r13d" "r14d" "r15d"))
(defparameter *new-reg64-list*     (list  "r8"  "r9"  "r10"  "r11"  "r12"  "r13"  "r14"  "r15"))

(defparameter *create-register-classes-list*
  (list
    (list *old-low-reg8-list* 'x86-old-8-bits-low-register)
    (list *old-high-reg8-list* 'x86-old-8-bits-high-register)
    (list *old-reg16-list* 'x86-old-16-bits-register)
    (list *old-reg32-list* 'x86-old-32-bits-register)
    (list *old-reg64-list* 'x86-old-64-bits-register)
    (list *new-low-reg8-list* 'x86-new-8-bits-register)
    (list *new-reg16-list* 'x86-new-16-bits-register)
    (list *new-reg32-list* 'x86-new-32-bits-register)
    (list *new-reg64-list* 'x86-new-64-bits-register)))
