;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defclass register ()
  ((register-name
     :reader register-name
     :documentation "any CPU-register")))

(defclass x86-register (register)
  ((is-x86-register
     :reader is-x86-register
     :initform t)
   (modrm
     :initarg :modrm
     :reader modrm
     :initform (error "ModRM must be specified")
     :documentation "ModRM")))

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
