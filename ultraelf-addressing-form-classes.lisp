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

(defclass addressing-form (argument)
  ())

(defclass register (addressing-form)
  ((is-reg
     :reader is-reg
     :allocation :class
     :initform t)
   (is-register-indirect
     :reader is-register-indirect
     :allocation :class
     :initform nil)
   (register-name
     :reader register-name
     :documentation "any CPU-register")))

(defclass x86-addressing-form (addressing-form)
  ((r/m
     :reader r/m
     :initarg :r/m
     :initform (error "r/m must be specified")
     :documentation "r/m bits of ModRM byte")))

(defclass x86-register-indirect (x86-addressing-form)
  ((is-reg
     :reader is-reg
     :allocation :class
     :initform nil)
   (is-register-indirect
     :reader is-register-indirect
     :allocation :class
     :initform t)
   (displacement-size
     :reader displacement-size
     :initarg :displacement-size
     :initform 0
     :documentation "Displacement size can be 0, 8 or 32 (bits). 0 is the default.")
   (is-memory-addressing
     :reader is-memory-addressing
     :allocation :class
     :initform t)))

(defclass x86-needs-sib (x86-register-indirect)
  ((needs-sib
     :reader needs-sib
     :allocation :class
     :initform t
     :documentation "[rsp], [rbp], [r12] and [r13] and [base+index] forms do need SIB.")))

(defclass x86-does-not-need-sib (x86-register-indirect)
  ((needs-sib
     :reader needs-sib
     :allocation :class
     :initform nil
     :documentation "[rax], [rcx], [rdx], [rbx], [rsi], [rdi], [r8], [r9], [r10], [r11], [r14] and [r15] do _not_ need SIB.")))

(defclass x86-modrm.mod-b00 (x86-addressing-form)
  ((modrm.mod
     :reader modrm.mod
     :allocation :class
     :initform #b00)))

(defclass x86-modrm.mod-b01 (x86-addressing-form)
  ((modrm.mod
     :reader modrm.mod
     :allocation :class
     :initform #b01)))

(defclass x86-modrm.mod-b10 (x86-addressing-form)
  ((modrm.mod
     :reader modrm.mod
     :allocation :class
     :initform #b10)))

(defclass x86-modrm.mod-b11 (x86-addressing-form)
  ((modrm.mod
     :reader modrm.mod
     :allocation :class
     :initform #b11)))

(defclass x86-rex.r-0 (x86-addressing-form)
  ((rex.r
     :reader rex.r
     :allocation :class
     :initform 0
     :documentation "REX.R = 0")))

(defclass x86-rex.r-1 (x86-addressing-form)
  ((rex.r
     :reader rex.r
     :allocation :class
     :initform 1
     :documentation "REX.R = 1")))

(defclass x86-rex.b-0 (x86-addressing-form)
  ((rex.b
     :reader rex.b
     :allocation :class
     :initform 0
     :documentation "REX.B = 0")))

(defclass x86-rex.b-1 (x86-addressing-form)
  ((rex.b
     :reader rex.b
     :allocation :class
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
     :allocation :class
     :initform nil
     :documentation "[rax], [rcx], [rdx], [rbx], [rsi] & [rdi] do _not_ need REX.")))

(defclass x86-new-register-indirect (x86-rex.b-1 x86-register-indirect)
  ((needs-rex
     :reader needs-rex
     :allocation :class
     :initform t
     :documentation "[r8], [r9], [r10], [r11], [r14] & [r15] do need REX.")))

(defclass x86-register (register x86-addressing-form)
  ((is-x86-register
     :reader is-x86-register
     :allocation :class
     :initform t)))

(defclass x86-old-register (x86-register)
  ((is-old-reg
     :reader is-old-reg
     :allocation :class
     :initform t
     :documentation "old registers are all registers which can be accessed without REX")
   (is-new-reg
     :reader is-new-reg
     :allocation :class
     :initform nil
     :documentation "new registers are all registers which can _not_ be accessed without REX")
   (rex.r
     :reader rex.r
     :allocation :class
     :initform 0
     :documentation "REX.R must be 0 for all old registers.")))

(defclass x86-new-register (x86-register)
  ((is-old-reg
     :reader is-old-reg
     :allocation :class
     :initform nil
     :documentation "old registers are all registers which can be accessed without REX")
   (is-new-reg
     :reader is-new-reg
     :allocation :class
     :initform t
     :documentation "new registers are all registers which can _not_ be accessed without REX")))

(defclass x86-8-bits-register (x86-register)
  ((reg-size
     :reader reg-size
     :allocation :class
     :initform 8
     :documentation "register size in bits")))

(defclass x86-16-bits-register (x86-register)
  ((reg-size
     :reader reg-size
     :allocation :class
     :initform 16
     :documentation "register size in bits")))

(defclass x86-32-bits-register (x86-register)
  ((reg-size
     :reader reg-size
     :allocation :class
     :initform 32
     :documentation "register size in bits")))

(defclass x86-64-bits-register (x86-register)
  ((reg-size
     :reader reg-size
     :allocation :class
     :initform 64
     :documentation "register size in bits")))

(defclass x86-old-8-bits-low-register (x86-old-register x86-8-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :allocation :class
     :initform nil
     :documentation "REX is _not_ needed")))

(defclass x86-old-8-bits-high-register (x86-old-register x86-8-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform nil
     :documentation "can _not_ be used with REX")
   (needs-rex
     :reader needs-rex
     :allocation :class
     :initform nil
     :documentation "REX is _not_ needed")))

(defclass x86-old-16-bits-register (x86-old-register x86-16-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :allocation :class
     :initform nil
     :documentation "REX is _not_ needed")))

(defclass x86-old-32-bits-register (x86-old-register x86-32-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :allocation :class
     :initform nil
     :documentation "REX is _not_ needed")))

(defclass x86-old-64-bits-register (x86-old-register x86-64-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :allocation :class
     :initform t
     :documentation "REX is needed (but see special cases such as jmp!)")))

(defclass x86-new-8-bits-register (x86-new-register x86-8-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :allocation :class
     :initform t
     :documentation "REX is needed")))

(defclass x86-new-8-bits-register-rex.r-0 (x86-rex.r-0 x86-new-8-bits-register)
  ((rex.r
     :reader rex.r
     :allocation :class
     :initform 0
     :documentation "Only spl, bpl, sil & dil belong to this class.")))

(defclass x86-new-8-bits-register-rex.r-1 (x86-rex.r-1 x86-new-8-bits-register)
  ((rex.r
     :reader rex.r
     :allocation :class
     :initform 1
     :documentation "r8b, r9b, r10b, r11b, r12b, r13b, r14b & r15b belong to this class.")))

(defclass x86-new-16-bits-register (x86-new-register x86-16-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :allocation :class
     :initform t
     :documentation "REX is needed")
   (rex.r
     :reader rex.r
     :allocation :class
     :initform 1
     :documentation "REX.R must be 1")))

(defclass x86-new-32-bits-register (x86-new-register x86-32-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :allocation :class
     :initform t
     :documentation "REX is needed")
   (rex.r
     :reader rex.r
     :allocation :class
     :initform 1
     :documentation "REX.R must be 1")))

(defclass x86-new-64-bits-register (x86-new-register x86-64-bits-register)
  ((works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :allocation :class
     :initform t
     :documentation "REX is needed")
   (rex.r
     :reader rex.r
     :allocation :class
     :initform 1
     :documentation "REX.R must be 1")))

(defclass x86-mmx-register (x86-register)
  ((is-mmx-reg
     :reader is-mmx-reg
     :allocation :class
     :initform t)
   (reg-size
     :reader reg-size
     :allocation :class
     :initform 64
     :documentation "register size in bits")
   (rex.r
     :reader rex.r
     :initform (list 0 1)
     :documentation "REX.R can be 0 or 1, no difference.")))

(defclass x86-xmm-register (x86-register)
  ((is-xmm-reg
     :reader is-xmm-reg
     :allocation :class
     :initform t)
   (reg-size
     :reader reg-size
     :allocation :class
     :initform 128
     :documentation "register size in bits")))

(defclass x86-old-xmm-register (x86-rex.r-0 x86-xmm-register)
  ((needs-rex
     :reader needs-rex
     :allocation :class
     :initform nil
     :documentation "xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6 & xmm7 do not need REX.")))

(defclass x86-new-xmm-register (x86-rex.r-1 x86-xmm-register)
  ((needs-rex
     :reader needs-rex
     :allocation :class
     :initform t
     :documentation "xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14 & xmm15 _do_ need REX.")))

(defclass x86-ymm-register (x86-register)
  ((is-ymm-reg
     :reader is-ymm-reg
     :allocation :class
     :initform t)
   (reg-size
     :reader reg-size
     :allocation :class
     :initform 256
     :documentation "register size in bits")))

(defclass x86-old-ymm-register (x86-rex.r-0 x86-ymm-register)
  ((needs-rex
     :reader needs-rex
     :allocation :class
     :initform nil
     :documentation "ymm0, ymm1, ymm2, ymm3, ymm4, ymm5, ymm6 & ymm7 do not need REX.")))

(defclass x86-new-ymm-register (x86-rex.r-1 x86-ymm-register)
  ((needs-rex
     :reader needs-rex
     :allocation :class
     :initform t
     :documentation "ymm8, ymm9, ymm10, ymm11, ymm12, ymm13, ymm14 & ymm15 _do_ need REX.")))

(defclass x86-zmm-register (x86-register)
  ((is-zmm-reg
     :reader is-ymm-reg
     :allocation :class
     :initform t)
   (reg-size
     :reader reg-size
     :allocation :class
     :initform 512
     :documentation "register size in bits")))

(defclass x86-old-zmm-register (x86-rex.r-0 x86-zmm-register)
  ())

(defclass x86-new-zmm-register (x86-rex.r-1 x86-zmm-register)
  ())

(defclass x86-rip-relative (x86-does-not-need-sib)
  ((is-reg
     :reader is-reg
     :allocation :class
     :initform nil)
   (displacement-size
     :reader displacement-size
     :allocation :class
     :initform 32
     :documentation "RIP-relative addressing form has always disp32.")
   (needs-rex
     :reader needs-rex
     :allocation :class
     :initform nil
     :documentation "RIP-relative addressing does not need REX.")))

(defclass x86-rip-disp32-0 (x86-rip-relative)
  ((rex.b
     :reader rex.b
     :allocation :class
     :initform 0
     :documentation "REX.B = 0")))

(defclass x86-rip-disp32-1 (x86-rip-relative)
  ((rex.b
     :reader rex.b
     :allocation :class
     :initform 1
     :documentation "REX.B = 1")))

(defclass x86-rip-disp32 (x86-rip-relative)
  ((rex.b
     :reader rex.b
     :allocation :class
     :initform nil
     :documentation "REX.B can be 0 or 1")))

(defclass x86-register-indirect-does-not-need-sib (x86-does-not-need-sib)
  ((needs-sib
     :reader needs-sib
     :allocation :class
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

(defgeneric modrm.mod (x86-addressing-form)
  (:documentation "mod bits (bits 6 and 7 of ModRM) specify the addressing mode for an operand.
                   0b00 register-indirect: [rax], [rcx], [rdx], [rbx], SIB, disp32, [rsi], [rdi].
                   0b01 register-indirect: [rax+disp8], [rcx+disp8], [rdx+disp8], [rbx+disp8], SIB+disp8, [rbp+disp8], [rsi+disp8], [rdi+disp8].
                   0b10 register-indirect: [rax+dis32], [rcx+dis32], [rdx+dis32], [rbx+dis32], SIB+dis32, [rbp+dis32], [rsi+dis32], [rdi+dis32].
                   0b11 register-direct:   any register-direct, see eg. AMD Architecture Programmers Manual Volume 3, Table 1-10."))

(defgeneric modrm.r/m (x86-addressing-form)
  (:documentation "r/m bits (bits 0 .. 2) of ModRM byte."))

(defmethod modrm.mod ((x86-addressing-form x86-addressing-form))
  (cond
    ((slot-value x86-addressing-form 'is-reg)
     #b11)
    ((slot-value x86-addressing-form 'needs-sib)
     ;; check the need for SIB first.
     (cond
       ((eql (slot-value x86-addressing-form 'displacement-size) 0)
        #b00)
       ((eql (slot-value x86-addressing-form 'displacement-size) 8)
        #b01)
       ((eql (slot-value x86-addressing-form 'displacement-size) 32)
        #b10)
       (t (error "error in defmethod modrm.mod."))))
    ((slot-value x86-addressing-form 'is-register-indirect)
     (cond
       ((eql (slot-value x86-addressing-form 'displacement-size) 0)
        #b00)
       ((eql (slot-value x86-addressing-form 'displacement-size) 8)
        #b01)
       ((eql (slot-value x86-addressing-form 'displacement-size) 32)
        #b10)
       (t (error "error in defmethod modrm.mod."))))
    (t (error "error in defmethod modrm.mod."))))

(defmethod modrm.r/m ((x86-addressing-form x86-addressing-form))
  (slot-value x86-addressing-form 'r/m))
