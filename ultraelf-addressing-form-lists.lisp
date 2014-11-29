;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

;; register lists. must be ordered in ModRM order.
;; general purpose registers.
(defparameter *old-low-reg8-list*          (list  "al"  "cl"   "dl"   "bl"))
(defparameter *old-high-reg8-list*                                    (list  "ah"   "ch"   "dh"   "bh"))
(defparameter *old-reg16-list*             (list  "ax"  "cx"   "dx"   "bx"   "sp"   "bp"   "si"   "di"))
(defparameter *old-reg32-list*             (list "eax" "ecx"  "edx"  "ebx"  "esp"  "ebp"  "esi"  "edi"))
(defparameter *old-reg64-list*             (list "rax" "rcx"  "rdx"  "rbx"  "rsp"  "rbp"  "rsi"  "rdi"))
(defparameter *new-low-reg8-list-from-spl*                            (list "spl"  "bpl"  "sil"  "dil"))
(defparameter *new-low-reg8-list-from-r8b* (list "r8b" "r9b" "r10b" "r11b" "r12b" "r13b" "r14b" "r15b"))
(defparameter *new-reg16-list*             (list "r8w" "r9w" "r10w" "r11w" "r12w" "r13w" "r14w" "r15w"))
(defparameter *new-reg32-list*             (list "r8d" "r9d" "r10d" "r11d" "r12d" "r13d" "r14d" "r15d"))
(defparameter *new-reg64-list*             (list  "r8"  "r9"  "r10"  "r11"  "r12"  "r13"  "r14"  "r15"))

;; SIMD registers.
(defparameter *mmx-reg-list*           (list "mm0"   "mm1"   "mm2"   "mm3"   "mm4"   "mm5"   "mm6"   "mm7"))
(defparameter *xmm-reg-list-from-xmm0* (list "xmm0" "xmm1"  "xmm2"  "xmm3"  "xmm4"  "xmm5"  "xmm6"  "xmm7"))
(defparameter *xmm-reg-list-from-xmm8* (list "xmm8" "xmm9" "xmm10" "xmm11" "xmm12" "xmm13" "xmm14" "xmm15"))
(defparameter *ymm-reg-list-from-ymm0* (list "ymm0" "ymm1"  "ymm2"  "ymm3"  "ymm4"  "ymm5"  "ymm6"  "ymm7"))
(defparameter *ymm-reg-list-from-ymm8* (list "ymm8" "ymm9" "ymm10" "ymm11" "ymm12" "ymm13" "ymm14" "ymm15"))
(defparameter *zmm-reg-list-from-zmm0* (list "zmm0" "zmm1"  "zmm2"  "zmm3"  "zmm4"  "zmm5"  "zmm6"  "zmm7"))
(defparameter *zmm-reg-list-from-zmm8* (list "zmm8" "zmm9" "zmm10" "zmm11" "zmm12" "zmm13" "zmm14" "zmm15"))

; other addressing form lists, must be ordered in ModRM order.
; `noindex` is a reserved word in ultraELF.
(defparameter *reg64-indirect-list-from-rax* (list "[rax]" "[rcx]" "[rdx]" "[rbx]"))
(defparameter *reg64-indirect-list-from-rsi* (list "[rsi]" "[rdi]"))
(defparameter *reg64-indirect-list-from-r8*  (list "[r8]"  "[r9]"  "[r10]" "[r11]"))
(defparameter *reg64-indirect-list-from-r14* (list "[r14]" "[r15]"))

; aadressing forms which need SIB.
(defparameter *reg64-register-indirect-rip-0*        (list "[rip0]"))
(defparameter *reg64-register-indirect-rip-1*        (list "[rip1]"))
(defparameter *reg64-register-indirect-rip*          (list "[rip]"))
(defparameter *reg64-register-indirect-rip-disp32-0* (list "[rip0+disp32]"))
(defparameter *reg64-register-indirect-rip-disp32-1* (list "[rip1+disp32]"))
(defparameter *reg64-register-indirect-rip-disp32*   (list "[rip+disp32]"))

(defparameter *create-addressing-form-instances-list*
  (list
    (list #b000 *old-low-reg8-list*                    'x86-old-8-bits-low-register)
    (list #b100 *old-high-reg8-list*                   'x86-old-8-bits-high-register)
    (list #b000 *old-reg16-list*                       'x86-old-16-bits-register)
    (list #b000 *old-reg32-list*                       'x86-old-32-bits-register)
    (list #b000 *old-reg64-list*                       'x86-old-64-bits-register)
    (list #b100 *new-low-reg8-list-from-spl*           'x86-new-8-bits-register-rex.r-0)
    (list #b000 *new-low-reg8-list-from-r8b*           'x86-new-8-bits-register-rex.r-1)
    (list #b000 *new-reg16-list*                       'x86-new-16-bits-register)
    (list #b000 *new-reg32-list*                       'x86-new-32-bits-register)
    (list #b000 *new-reg64-list*                       'x86-new-64-bits-register)
    (list #b000 *mmx-reg-list*                         'x86-mmx-register)
    (list #b000 *xmm-reg-list-from-xmm0*               'x86-old-xmm-register)
    (list #b000 *xmm-reg-list-from-xmm8*               'x86-new-xmm-register)
    (list #b000 *ymm-reg-list-from-ymm0*               'x86-old-ymm-register)
    (list #b000 *ymm-reg-list-from-ymm8*               'x86-new-ymm-register)
    (list #b000 *zmm-reg-list-from-zmm0*               'x86-old-zmm-register)
    (list #b000 *zmm-reg-list-from-zmm8*               'x86-new-zmm-register)
    (list #b000 *reg64-indirect-list-from-rax*         'x64-old-register-indirect-does-not-need-sib)
    (list #b110 *reg64-indirect-list-from-rsi*         'x64-old-register-indirect-does-not-need-sib)
    (list #b000 *reg64-indirect-list-from-r8*          'x64-new-register-indirect-does-not-need-sib)
    (list #b110 *reg64-indirect-list-from-r14*         'x64-new-register-indirect-does-not-need-sib)
    (list #b101 *reg64-register-indirect-rip-0*        'x86-rip-disp32-0)
    (list #b101 *reg64-register-indirect-rip-1*        'x86-rip-disp32-1)
    (list #b101 *reg64-register-indirect-rip*          'x86-rip-disp32)
    (list #b101 *reg64-register-indirect-rip-disp32-0* 'x86-rip-disp32-0)
    (list #b101 *reg64-register-indirect-rip-disp32-1* 'x86-rip-disp32-1)
    (list #b101 *reg64-register-indirect-rip-disp32*   'x86-rip-disp32)))
