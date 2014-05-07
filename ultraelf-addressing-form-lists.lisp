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
(defparameter *reg64-indirect-list-from-rax* (list "[rax]" "[rcx]" "[rdx]" "[rbx]"))
(defparameter *reg64-indirect-list-from-rsi* (list "[rsi]" "[rdi]"))
(defparameter *reg64-indirect-list-from-r8*  (list "[r8]"  "[r9]"  "[r10]" "[r11]"))
(defparameter *reg64-indirect-list-from-r14* (list "[r14]" "[r15]"))

(defparameter *create-addressing-form-instances-list*
  (list
    (list #b000 *old-low-reg8-list* 'x86-old-8-bits-low-register)
    (list #b100 *old-high-reg8-list* 'x86-old-8-bits-high-register)
    (list #b000 *old-reg16-list* 'x86-old-16-bits-register)
    (list #b000 *old-reg32-list* 'x86-old-32-bits-register)
    (list #b000 *old-reg64-list* 'x86-old-64-bits-register)
    (list #b100 *new-low-reg8-list-from-spl* 'x86-new-8-bits-register)
    (list #b000 *new-low-reg8-list-from-r8b* 'x86-new-8-bits-register)
    (list #b000 *new-reg16-list* 'x86-new-16-bits-register)
    (list #b000 *new-reg32-list* 'x86-new-32-bits-register)
    (list #b000 *new-reg64-list* 'x86-new-64-bits-register)
    (list #b000 *mmx-reg-list* 'x86-mmx-register)
    (list #b000 *xmm-reg-list* 'x86-xmm-register)
    (list #b000 *ymm-reg-list* 'x86-ymm-register)
    (list #b000 *zmm-reg-list* 'x86-zmm-register)
    (list #b000 *reg64-indirect-list-from-rax* 'x86-old-register-indirect)
    (list #b110 *reg64-indirect-list-from-rsi* 'x86-old-register-indirect)
    (list #b000 *reg64-indirect-list-from-r8*  'x86-new-register-indirect)
    (list #b110 *reg64-indirect-list-from-r14* 'x86-new-register-indirect)))
