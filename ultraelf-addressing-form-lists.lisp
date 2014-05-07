;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
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
(defparameter *mmx-reg-list* (list "mmx0" "mmx1" "mmx2" "mmx3" "mmx4" "mmx5" "mmx6" "mmx7"))
(defparameter *xmm-reg-list* (list "xmm0" "xmm1" "xmm2" "xmm3" "xmm4" "xmm5" "xmm6" "xmm7"))
(defparameter *ymm-reg-list* (list "ymm0" "ymm1" "ymm2" "ymm3" "ymm4" "ymm5" "ymm6" "ymm7"))
(defparameter *zmm-reg-list* (list "zmm0" "zmm1" "zmm2" "zmm3" "zmm4" "zmm5" "zmm6" "zmm7"))

; other addressing form lists, must be ordered in ModRM order.
; `noindex` is a reserved word in ultraELF.
(defparameter *reg64-indirect-no-scale-list* (list   "[rax]"   "[rcx]"   "[rdx]"   "[rbx]"   "noindex"   "[rbp]"   "[rsi]"   "[rdi]"))
(defparameter *reg64-indirect-scale-1-list*  (list "[1*rax]" "[1*rcx]" "[1*rdx]" "[1*rbx]" "1*noindex" "[1*rbp]" "[1*rsi]" "[1*rdi]"))
(defparameter *reg64-indirect-scale-2-list*  (list "[2*rax]" "[2*rcx]" "[2*rdx]" "[2*rbx]" "2*noindex" "[2*rbp]" "[2*rsi]" "[2*rdi]"))
(defparameter *reg64-indirect-scale-4-list*  (list "[4*rax]" "[4*rcx]" "[4*rdx]" "[4*rbx]" "4*noindex" "[4*rbp]" "[4*rsi]" "[4*rdi]"))
(defparameter *reg64-indirect-scale-8-list*  (list "[8*rax]" "[8*rcx]" "[8*rdx]" "[8*rbx]" "8*noindex" "[8*rbp]" "[8*rsi]" "[8*rdi]"))

(defparameter *create-addressing-form-instances-list*
  (list
    (list 0 *old-low-reg8-list* 'x86-old-8-bits-low-register)
    (list 4 *old-high-reg8-list* 'x86-old-8-bits-high-register)
    (list 0 *old-reg16-list* 'x86-old-16-bits-register)
    (list 0 *old-reg32-list* 'x86-old-32-bits-register)
    (list 0 *old-reg64-list* 'x86-old-64-bits-register)
    (list 4 *new-low-reg8-list-from-spl* 'x86-new-8-bits-register)
    (list 0 *new-low-reg8-list-from-r8b* 'x86-new-8-bits-register)
    (list 0 *new-reg16-list* 'x86-new-16-bits-register)
    (list 0 *new-reg32-list* 'x86-new-32-bits-register)
    (list 0 *new-reg64-list* 'x86-new-64-bits-register)
    (list 0 *mmx-reg-list* 'x86-mmx-register)
    (list 0 *xmm-reg-list* 'x86-xmm-register)
    (list 0 *ymm-reg-list* 'x86-ymm-register)
    (list 0 *zmm-reg-list* 'x86-zmm-register)
    (list 0 *reg64-indirect-no-scale-list* 'x86-register-indirect)
    (list 0 *reg64-indirect-scale-1-list* 'x86-register-indirect)
    (list 0 *reg64-indirect-scale-2-list* 'x86-register-indirect)
    (list 0 *reg64-indirect-scale-4-list* 'x86-register-indirect)
    (list 0 *reg64-indirect-scale-8-list* 'x86-register-indirect)))
