;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

;; addressing-form
;; - register (addressing-form)
;; - x86-addressing-form (addressing-form)
;;
;; register
;; - x86-register (register x86-addressing-form)
;;
;; x86-register-indirect (x86-addressing-form)
;; - x86-needs-sib (x86-register-indirect)
;; - x86-does-not-need-sib (x86-register-indirect)
;; - x86-old-register-indirect (x86-rex.b-0 x86-register-indirect)
;; - x86-new-register-indirect (x86-rex.b-1 x86-register-indirect)
;;
;; x86-needs-sib (x86-register-indirect)
;; - x86-sib-0 (x86-rex.b-0 x86-needs-sib)
;; - x86-sib-1 (x86-rex.b-1 x86-needs-sib)
;; - x86-sib (x86-needs-sib)
;; - x86-register-indirect-needs-sib (x86-needs-sib)
;;
;; x86-does-not-need-sib (x86-register-indirect)
;; - x86-rip-relative (x86-does-not-need-sib)
;; - x86-register-indirect-does-not-need-sib (x86-does-not-need-sib)
;;
;; x86-modrm.mod-b00 (x86-addressing-form)
;;
;; x86-modrm.mod-b01 (x86-addressing-form)
;;
;; x86-modrm.mod-b10 (x86-addressing-form)
;;
;; x86-modrm.mod-b11 (x86-addressing-form)
;;
;; x86-rex.b-0  (x86-addressing-form)
;; - x86-sib-0 (x86-rex.b-0 x86-needs-sib)
;; - x86-old-register-indirect (x86-rex.b-0 x86-register-indirect)
;;
;; x86-rex.b-1 (x86-addressing-form)
;; - x86-sib-1 (x86-rex.b-1 x86-needs-sib)
;; - x86-new-register-indirect (x86-rex.b-1 x86-register-indirect)
;;
;; x86-sib-0 (x86-rex.b-0 x86-needs-sib)
;;
;; x86-sib-1 (x86-rex.b-1 x86-needs-sib)
;;
;; x86-sib (x86-needs-sib)
;;
;; x86-register-indirect-needs-sib (x86-needs-sib)
;; - x86-old-register-indirect-needs-sib (x86-old-register-indirect x86-register-indirect-needs-sib)
;; - x86-new-register-indirect-needs-sib (x86-new-register-indirect x86-register-indirect-needs-sib)
;;
;; x86-old-register-indirect (x86-rex.b-0 x86-register-indirect)
;; - x86-old-register-indirect-needs-sib (x86-old-register-indirect x86-register-indirect-needs-sib)
;; - x86-old-register-indirect-does-not-need-sib (x86-old-register-indirect x86-register-indirect-does-not-need-sib)
;;
;; x86-new-register-indirect (x86-rex.b-1 x86-register-indirect)
;; - x86-new-register-indirect-needs-sib (x86-new-register-indirect x86-register-indirect-needs-sib)
;; - x86-new-register-indirect-does-not-need-sib (x86-new-register-indirect x86-register-indirect-does-not-need-sib)
;;
;; x86-register (register x86-addressing-form)
;; - x86-old-register (x86-register)
;; - x86-new-register (x86-register)
;; - x86-8-bits-register (x86-register)
;; - x86-16-bits-register (x86-register)
;; - x86-32-bits-register (x86-register)
;; - x86-64-bits-register (x86-register)
;; - x86-mmx-register (x86-register)
;; - x86-xmm-register (x86-register)
;; - x86-ymm-register (x86-register)
;; - x86-zmm-register (x86-register)
;;
;; x86-old-register (x86-register)
;; - x86-old-8-bits-low-register (x86-old-register x86-16-bits-register)
;; - x86-old-8-bits-high-register (x86-old-register x86-16-bits-register)
;; - x86-old-16-bits-register (x86-old-register x86-16-bits-register)
;; - x86-old-32-bits-register (x86-old-register x86-32-bits-register)
;; - x86-old-64-bits-register (x86-old-register x86-64-bits-register)
;;
;; x86-new-register (x86-register)
;; - x86-new-8-bits-register (x86-new-register x86-8-bits-register)
;; - x86-new-16-bits-register (x86-new-register x86-16-bits-register)
;; - x86-new-32-bits-register (x86-new-register x86-32-bits-register)
;; - x86-new-64-bits-register (x86-new-register x86-64-bits-register)
;;
;; x86-8-bits-register (x86-register)
;; - x86-old-8-bits-low-register (x86-old-register x86-8-bits-register)
;; - x86-new-8-bits-register (x86-new-register x86-8-bits-register)
;;
;; x86-16-bits-register (x86-register)
;; - x86-old-16-bits-register (x86-old-register x86-16-bits-register)
;; - x86-new-16-bits-register (x86-new-register x86-16-bits-register)
;;
;; x86-32-bits-register (x86-register)
;; - x86-old-32-bits-register (x86-old-register x86-32-bits-register)
;; - x86-new-32-bits-register (x86-new-register x86-32-bits-register)
;;
;; x86-64-bits-register (x86-register)
;; - x86-old-64-bits-register (x86-old-register x86-64-bits-register)
;; - x86-new-64-bits-register (x86-new-register x86-64-bits-register)
;;
;; x86-old-8-bits-low-register (x86-old-register x86-8-bits-register)
;;
;; x86-old-8-bits-high-register (x86-old-register x86-8-bits-register)
;;
;; x86-old-16-bits-register (x86-old-register x86-16-bits-register)
;;
;; x86-old-32-bits-register (x86-old-register x86-32-bits-register)
;;
;; x86-old-64-bits-register (x86-old-register x86-64-bits-register)
;;
;; x86-new-8-bits-register (x86-new-register x86-8-bits-register)
;;
;; x86-new-16-bits-register (x86-new-register x86-16-bits-register)
;;
;; x86-new-32-bits-register (x86-new-register x86-32-bits-register)
;;
;; x86-new-64-bits-register (x86-new-register x86-64-bits-register)
;;
;; x86-mmx-register (x86-register)
;;
;; x86-xmm-register (x86-register)
;;
;; x86-ymm-register (x86-register)
;;
;; x86-zmm-register (x86-register)
;;
;; x86-rip-relative (x86-does-not-need-sib)
;; - x86-rip-disp32-0 (x86-rip-relative)
;; - x86-rip-disp32-1 (x86-rip-relative)
;; - x86-rip-disp32 (x86-rip-relative)
;;
;; x86-rip-disp32-0 (x86-rip-relative)
;;
;; x86-rip-disp32-1 (x86-rip-relative)
;;
;; x86-rip-disp32 (x86-rip-relative)
;;
;; x86-register-indirect-does-not-need-sib (x86-does-not-need-sib)
;; - x86-old-register-indirect-does-not-need-sib (x86-old-register-indirect x86-register-indirect-does-not-need-sib)
;; - x86-new-register-indirect-does-not-need-sib (x86-new-register-indirect x86-register-indirect-does-not-need-sib)
;;
;; x86-old-register-indirect-needs-sib (x86-old-register-indirect x86-register-indirect-needs-sib)
;;
;; x86-old-register-indirect-does-not-need-sib (x86-old-register-indirect x86-register-indirect-does-not-need-sib)
;;
;; x86-new-register-indirect-needs-sib (x86-new-register-indirect x86-register-indirect-needs-sib)
;;
;; x86-new-register-indirect-does-not-need-sib (x86-new-register-indirect x86-register-indirect-does-not-need-sib)

(defclass addressing-form ()
  ((addressing-form-name
     :reader addressing-form-name
     :documentation "any CPU addressing form")))

(defclass register (addressing-form)
  ((is-reg
     :reader is-reg
     :initform t)
   (is-register-indirect
     :reader is-register-indirect
     :initform nil)
   (register-name
     :reader register-name
     :documentation "any CPU-register")))

(defclass x86-addressing-form (addressing-form)
  ((r/m
     :initarg :r/m
     :reader r/m
     :initform (error "r/m must be specified")
     :documentation "r/m bits of ModRM byte")))

(defclass x86-register-indirect (x86-addressing-form)
  ((is-reg
     :reader is-reg
     :initform nil)
   (is-register-indirect
     :reader is-register-indirect
     :initform t)
   (is-memory-addressing
     :reader is-memory-addressing
     :initform t)))

(defclass x86-needs-sib (x86-register-indirect)
  ((needs-sib
     :reader needs-sib
     :initform t
     :documentation "[rsp], [rbp], [r12] and [r13] and [base+index] forms do need SIB.")))

(defclass x86-does-not-need-sib (x86-register-indirect)
  ((needs-sib
     :reader needs-sib
     :initform nil
     :documentation "[rax], [rcx], [rdx], [rbx], [rsi], [rdi], [r8], [r9], [r10], [r11], [r14] and [r15] do _not_ need SIB.")))

(defclass x86-modrm.mod-b00 (x86-addressing-form)
  ((modrm.mod
     :reader modrm.mod
     :initform #b00)))

(defclass x86-modrm.mod-b01 (x86-addressing-form)
  ((modrm.mod
     :reader modrm.mod
     :initform #b01)))

(defclass x86-modrm.mod-b10 (x86-addressing-form)
  ((modrm.mod
     :reader modrm.mod
     :initform #b10)))

(defclass x86-modrm.mod-b11 (x86-addressing-form)
  ((modrm.mod
     :reader modrm.mod
     :initform #b11)))

(defclass x86-rex.r-0 (x86-addressing-form)
  ((rex.r
     :reader rex.r
     :initform 0
     :documentation "REX.R = 0")))

(defclass x86-rex.r-1 (x86-addressing-form)
  ((rex.r
     :reader rex.r
     :initform 1
     :documentation "REX.R = 1")))

(defclass x86-rex.b-0 (x86-addressing-form)
  ((rex.b
     :reader rex.b
     :initform 0
     :documentation "REX.B = 0")))

(defclass x86-rex.b-1 (x86-addressing-form)
  ((rex.b
     :reader rex.b
     :initform 1
     :documentation "REX.B = 1")))

(defclass x86-sib-0 (x86-rex.b-0 x86-needs-sib)
  ())

(defclass x86-sib-1 (x86-rex.b-1 x86-needs-sib)
  ())

(defclass x86-sib (x86-needs-sib)
  ((rex.b
     :reader rex.b
     :initform (list 0 1)
     :documentation "REX.B can be 0 or 1")))

(defclass x86-register-indirect-needs-sib (x86-needs-sib)
  ())

(defclass x86-old-register-indirect (x86-rex.b-0 x86-register-indirect)
  ((needs-rex
     :reader needs-rex
     :initform nil
     :documentation "[rax], [rcx], [rdx], [rbx], [rsi] & [rdi] do _not_ need REX.")))

(defclass x86-new-register-indirect (x86-rex.b-1 x86-register-indirect)
  ((needs-rex
     :reader needs-rex
     :initform t
     :documentation "[r8], [r9], [r10], [r11], [r14] & [r15] do need REX.")))

(defclass x86-register (register x86-addressing-form)
  ((is-x86-register
     :reader is-x86-register
     :initform t)))

(defclass x86-old-register (x86-register)
  ((is-old-reg
     :reader is-old-reg
     :initform t
     :documentation "old registers are all registers which can be accessed without REX")
   (is-new-reg
     :reader is-new-reg
     :initform nil
     :documentation "new registers are all registers which can _not_ be accessed without REX")))

(defclass x86-new-register (x86-register)
  ((is-old-reg
     :reader is-old-reg
     :initform nil
     :documentation "old registers are all registers which can be accessed without REX")
   (is-new-reg
     :reader is-new-reg
     :initform t
     :documentation "new registers are all registers which can _not_ be accessed without REX")))

(defclass x86-8-bits-register (x86-register)
  ((reg-size
     :reader reg-size
     :initform 8
     :documentation "register size in bits")))

(defclass x86-16-bits-register (x86-register)
  ((reg-size
     :reader reg-size
     :initform 16
     :documentation "register size in bits")))

(defclass x86-32-bits-register (x86-register)
  ((reg-size
     :reader reg-size
     :initform 32
     :documentation "register size in bits")))

(defclass x86-64-bits-register (x86-register)
  ((reg-size
     :reader reg-size
     :initform 64
     :documentation "register size in bits")))

(defclass x86-old-8-bits-low-register (x86-old-register x86-8-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :initform nil
     :documentation "REX is _not_ needed")))

(defclass x86-old-8-bits-high-register (x86-old-register x86-8-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :initform nil
     :documentation "can _not_ be used with REX")
   (needs-rex
     :reader needs-rex
     :initform nil
     :documentation "REX is _not_ needed")))

(defclass x86-old-16-bits-register (x86-old-register x86-16-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :initform nil
     :documentation "REX is _not_ needed")))

(defclass x86-old-32-bits-register (x86-old-register x86-32-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :initform nil
     :documentation "REX is _not_ needed")))

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

(defclass x86-new-8-bits-register-rex.r-0 (x86-rex.r-0 x86-new-8-bits-register)
  ((rex.r
     :reader rex.r
     :initform 0
     :documentation "Only spl, bpl, sil & dil belong to this class.")))

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

(defclass x86-mmx-register (x86-register)
  ((is-mmx-reg
     :reader is-mmx-reg
     :initform t)
   (reg-size
     :reader reg-size
     :initform 64
     :documentation "register size in bits")))

(defclass x86-xmm-register (x86-register)
  ((is-xmm-reg
     :reader is-xmm-reg
     :initform t)
   (reg-size
     :reader reg-size
     :initform 128
     :documentation "register size in bits")))

(defclass x86-ymm-register (x86-register)
  ((is-ymm-reg
     :reader is-ymm-reg
     :initform t)
   (reg-size
     :reader reg-size
     :initform 256
     :documentation "register size in bits")))

(defclass x86-zmm-register (x86-register)
  ((is-zmm-reg
     :reader is-ymm-reg
     :initform t)
   (reg-size
     :reader reg-size
     :initform 512
     :documentation "register size in bits")))

(defclass x86-rip-relative (x86-does-not-need-sib)
  ((is-reg
     :reader is-reg
     :initform nil)
   (displacement-size
     :reader displacement-size
     :initform 32
     :documentation "RIP-relative addressing form has always disp32.")
   (needs-rex
     :reader needs-rex
     :initform nil
     :documentation "RIP-relative addressing does not need REX.")))

(defclass x86-rip-disp32-0 (x86-rip-relative)
  ((rex.b
     :reader rex.b
     :initform 0
     :documentation "REX.B = 0")))

(defclass x86-rip-disp32-1 (x86-rip-relative)
  ((rex.b
     :reader rex.b
     :initform 1
     :documentation "REX.B = 1")))

(defclass x86-rip-disp32 (x86-rip-relative)
  ((rex.b
     :reader rex.b
     :initform nil
     :documentation "REX.B can be 0 or 1")))

(defclass x86-register-indirect-does-not-need-sib (x86-does-not-need-sib)
  ((needs-sib
     :reader needs-sib
     :initform nil
     :documentation "[rax], [rcx], [rdx], [rbx], [rsi], [rdi], [r8], [r9], [r10], [r11], [r14] and [r15] do _not_ need SIB.")))

(defclass x86-old-register-indirect-needs-sib (x86-old-register-indirect x86-register-indirect-needs-sib)
  ())

(defclass x86-old-register-indirect-does-not-need-sib (x86-old-register-indirect x86-register-indirect-does-not-need-sib)
  ())

(defclass x86-new-register-indirect-needs-sib (x86-new-register-indirect x86-register-indirect-needs-sib)
  ())

(defclass x86-new-register-indirect-does-not-need-sib (x86-new-register-indirect x86-register-indirect-does-not-need-sib)
  ())
