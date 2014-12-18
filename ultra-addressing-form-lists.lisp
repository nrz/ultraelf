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

; ultraELF assumes `[base+index]` and `[base+scale*index]` (Intel style), _not_ `[index+base]` and `[scale*index+base]` (AMD style)!!!

; begins from 0b00000000
(defparameter *reg64-sib-list-no-scale-from-rax-rax* (list "[rax+rax]" "[rcx+rax]" "[rdx+rax]" "[rbx+rax]"
                                                           "[rsp+rax]" "[rbp+rax]" "[rsi+rax]" "[rdi+rax]"   ; [rxx+rax]
                                                           "[rax+rcx]" "[rcx+rcx]" "[rdx+rcx]" "[rbx+rcx]"
                                                           "[rsp+rcx]" "[rbp+rcx]" "[rsi+rcx]" "[rdi+rcx]"   ; [rxx+rcx]
                                                           "[rax+rdx]" "[rcx+rdx]" "[rdx+rdx]" "[rbx+rdx]"
                                                           "[rsp+rdx]" "[rbp+rdx]" "[rsi+rdx]" "[rdi+rdx]"   ; [rxx+rdx]
                                                           "[rax+rbx]" "[rcx+rbx]" "[rdx+rbx]" "[rbx+rbx]"
                                                           "[rsp+rbx]" "[rbp+rbx]" "[rsi+rbx]" "[rdi+rbx]")) ; [rxx+rbx]

; begins from 0b00101000
(defparameter *reg64-sib-list-no-scale-from-rax-rbx* (list "[rax+rbp]" "[rcx+rbp]" "[rdx+rbp]" "[rbx+rbp]" "[rsp+rbp]" "[rbp+rbp]" "[rsi+rbp]" "[rdi+rbp]"   ; [rxx+rbp]
                                                           "[rax+rsi]" "[rcx+rsi]" "[rdx+rsi]" "[rbx+rsi]" "[rsp+rsi]" "[rbp+rsi]" "[rsi+rsi]" "[rdi+rsi]"   ; [rxx+rsi]
                                                           "[rax+rdi]" "[rcx+rdi]" "[rdx+rdi]" "[rbx+rdi]" "[rsp+rdi]" "[rbp+rdi]" "[rsi+rdi]" "[rdi+rdi]")) ; [rxx+rdi]
; begins from 0b00000000
(defparameter *reg64-sib-list-no-scale-from-rax-1-times-rax* (list "[rax+1*rax]" "[rcx+1*rax]" "[rdx+1*rax]" "[rbx+1*rax]"
                                                                   "[rsp+1*rax]" "[rbp+1*rax]" "[rsi+1*rax]" "[rdi+1*rax]"   ; [rxx+1*rax]
                                                                   "[rax+1*rcx]" "[rcx+1*rcx]" "[rdx+1*rcx]" "[rbx+1*rcx]"
                                                                   "[rsp+1*rcx]" "[rbp+1*rcx]" "[rsi+1*rcx]" "[rdi+1*rcx]"   ; [rxx+1*rcx]
                                                                   "[rax+1*rdx]" "[rcx+1*rdx]" "[rdx+1*rdx]" "[rbx+1*rdx]"
                                                                   "[rsp+1*rdx]" "[rbp+1*rdx]" "[rsi+1*rdx]" "[rdi+1*rdx]"   ; [rxx+1*rdx]
                                                                   "[rax+1*rbx]" "[rcx+1*rbx]" "[rdx+1*rbx]" "[rbx+1*rbx]"
                                                                   "[rsp+1*rbx]" "[rbp+1*rbx]" "[rsi+1*rbx]" "[rdi+1*rbx]")) ; [rxx+1*rbx]
; begins from 0b01000000
(defparameter *reg64-sib-list-no-scale-from-rax-2-times-rax* (list "[rax+2*rax]" "[rcx+2*rax]" "[rdx+2*rax]" "[rbx+2*rax]"
                                                                   "[rsp+2*rax]" "[rbp+2*rax]" "[rsi+2*rax]" "[rdi+2*rax]"   ; [rxx+2*rax]
                                                                   "[rax+2*rcx]" "[rcx+2*rcx]" "[rdx+2*rcx]" "[rbx+2*rcx]"
                                                                   "[rsp+2*rcx]" "[rbp+2*rcx]" "[rsi+2*rcx]" "[rdi+2*rcx]"   ; [rxx+2*rcx]
                                                                   "[rax+2*rdx]" "[rcx+2*rdx]" "[rdx+2*rdx]" "[rbx+2*rdx]"
                                                                   "[rsp+2*rdx]" "[rbp+2*rdx]" "[rsi+2*rdx]" "[rdi+2*rdx]"   ; [rxx+2*rdx]
                                                                   "[rax+2*rbx]" "[rcx+2*rbx]" "[rdx+2*rbx]" "[rbx+2*rbx]"
                                                                   "[rsp+2*rbx]" "[rbp+2*rbx]" "[rsi+2*rbx]" "[rdi+2*rbx]")) ; [rxx+2*rbx]
; begins from 0b10000000
(defparameter *reg64-sib-list-no-scale-from-rax-1-times-rax* (list "[rax+4*rax]" "[rcx+4*rax]" "[rdx+4*rax]" "[rbx+4*rax]"
                                                                   "[rsp+4*rax]" "[rbp+4*rax]" "[rsi+4*rax]" "[rdi+4*rax]"   ; [rxx+4*rax]
                                                                   "[rax+4*rcx]" "[rcx+4*rcx]" "[rdx+4*rcx]" "[rbx+4*rcx]"
                                                                   "[rsp+4*rcx]" "[rbp+4*rcx]" "[rsi+4*rcx]" "[rdi+4*rcx]"   ; [rxx+4*rcx]
                                                                   "[rax+4*rdx]" "[rcx+4*rdx]" "[rdx+4*rdx]" "[rbx+4*rdx]"
                                                                   "[rsp+4*rdx]" "[rbp+4*rdx]" "[rsi+4*rdx]" "[rdi+4*rdx]"   ; [rxx+4*rdx]
                                                                   "[rax+4*rbx]" "[rcx+4*rbx]" "[rdx+4*rbx]" "[rbx+4*rbx]"
                                                                   "[rsp+4*rbx]" "[rbp+4*rbx]" "[rsi+4*rbx]" "[rdi+4*rbx]")) ; [rxx+4*rbx]

; begins from 0b11000000
(defparameter *reg64-sib-list-no-scale-from-rax-1-times-rax* (list "[rax+8*rax]" "[rcx+8*rax]" "[rdx+8*rax]" "[rbx+8*rax]"
                                                                   "[rsp+8*rax]" "[rbp+8*rax]" "[rsi+8*rax]" "[rdi+8*rax]"   ; [rxx+8*rax]
                                                                   "[rax+8*rcx]" "[rcx+8*rcx]" "[rdx+8*rcx]" "[rbx+8*rcx]"
                                                                   "[rsp+8*rcx]" "[rbp+8*rcx]" "[rsi+8*rcx]" "[rdi+8*rcx]"   ; [rxx+8*rcx]
                                                                   "[rax+8*rdx]" "[rcx+8*rdx]" "[rdx+8*rdx]" "[rbx+8*rdx]"
                                                                   "[rsp+8*rdx]" "[rbp+8*rdx]" "[rsi+8*rdx]" "[rdi+8*rdx]"   ; [rxx+8*rdx]
                                                                   "[rax+8*rbx]" "[rcx+8*rbx]" "[rdx+8*rbx]" "[rbx+8*rbx]"
                                                                   "[rsp+8*rbx]" "[rbp+8*rbx]" "[rsi+8*rbx]" "[rdi+8*rbx]")) ; [rxx+8*rbx]


; begins from 0b00000000
(defparameter *reg64-sib-list-no-scale-from-rax-rax-disp8* (list "[rax+rax+disp8]" "[rcx+rax+disp8]" "[rdx+rax+disp8]" "[rbx+rax+disp8]"
                                                                 "[rsp+rax+disp8]" "[rbp+rax+disp8]" "[rsi+rax+disp8]" "[rdi+rax+disp8]"   ; [rxx+rax+disp8]
                                                                 "[rax+rcx+disp8]" "[rcx+rcx+disp8]" "[rdx+rcx+disp8]" "[rbx+rcx+disp8]"
                                                                 "[rsp+rcx+disp8]" "[rbp+rcx+disp8]" "[rsi+rcx+disp8]" "[rdi+rcx+disp8]"   ; [rxx+rcx+disp8]
                                                                 "[rax+rdx+disp8]" "[rcx+rdx+disp8]" "[rdx+rdx+disp8]" "[rbx+rdx+disp8]"
                                                                 "[rsp+rdx+disp8]" "[rbp+rdx+disp8]" "[rsi+rdx+disp8]" "[rdi+rdx+disp8]"   ; [rxx+rdx+disp8]
                                                                 "[rax+rbx+disp8]" "[rcx+rbx+disp8]" "[rdx+rbx+disp8]" "[rbx+rbx+disp8]"
                                                                 "[rsp+rbx+disp8]" "[rbp+rbx+disp8]" "[rsi+rbx+disp8]" "[rdi+rbx+disp8]")) ; [rxx+rbx+disp8]

; begins from 0b00101000
(defparameter *reg64-sib-list-no-scale-from-rax-rbx-disp8* (list "[rax+rbp+disp8]" "[rcx+rbp+disp8]" "[rdx+rbp+disp8]" "[rbx+rbp+disp8]"
                                                                 "[rsp+rbp+disp8]" "[rbp+rbp+disp8]" "[rsi+rbp+disp8]" "[rdi+rbp+disp8]"   ; [rxx+rbp+disp8]
                                                                 "[rax+rsi+disp8]" "[rcx+rsi+disp8]" "[rdx+rsi+disp8]" "[rbx+rsi+disp8]"
                                                                 "[rsp+rsi+disp8]" "[rbp+rsi+disp8]" "[rsi+rsi+disp8]" "[rdi+rsi+disp8]"   ; [rxx+rsi+disp8]
                                                                 "[rax+rdi+disp8]" "[rcx+rdi+disp8]" "[rdx+rdi+disp8]" "[rbx+rdi+disp8]"
                                                                 "[rsp+rdi+disp8]" "[rbp+rdi+disp8]" "[rsi+rdi+disp8]" "[rdi+rdi+disp8]")) ; [rxx+rdi+disp8]
; begins from 0b00000000
(defparameter *reg64-sib-list-no-scale-from-rax-1-times-rax-disp8* (list "[rax+1*rax+disp8]"
                                                                         "[rcx+1*rax+disp8]"
                                                                         "[rdx+1*rax+disp8]"
                                                                         "[rbx+1*rax+disp8]"
                                                                         "[rsp+1*rax+disp8]"
                                                                         "[rbp+1*rax+disp8]"
                                                                         "[rsi+1*rax+disp8]"
                                                                         "[rdi+1*rax+disp8]"   ; [rxx+1*rax+disp8]
                                                                         "[rax+1*rcx+disp8]"
                                                                         "[rcx+1*rcx+disp8]"
                                                                         "[rdx+1*rcx+disp8]"
                                                                         "[rbx+1*rcx+disp8]"
                                                                         "[rsp+1*rcx+disp8]"
                                                                         "[rbp+1*rcx+disp8]"
                                                                         "[rsi+1*rcx+disp8]"
                                                                         "[rdi+1*rcx+disp8]"   ; [rxx+1*rcx+disp8]
                                                                         "[rax+1*rdx+disp8]"
                                                                         "[rcx+1*rdx+disp8]"
                                                                         "[rdx+1*rdx+disp8]"
                                                                         "[rbx+1*rdx+disp8]"
                                                                         "[rsp+1*rdx+disp8]"
                                                                         "[rbp+1*rdx+disp8]"
                                                                         "[rsi+1*rdx+disp8]"
                                                                         "[rdi+1*rdx+disp8]"   ; [rxx+1*rdx+disp8]
                                                                         "[rax+1*rbx+disp8]"
                                                                         "[rcx+1*rbx+disp8]"
                                                                         "[rdx+1*rbx+disp8]"
                                                                         "[rbx+1*rbx+disp8]"
                                                                         "[rsp+1*rbx+disp8]"
                                                                         "[rbp+1*rbx+disp8]"
                                                                         "[rsi+1*rbx+disp8]"
                                                                         "[rdi+1*rbx+disp8]")) ; [rxx+1*rbx+disp8]
; begins from 0b01000000
(defparameter *reg64-sib-list-no-scale-from-rax-2-times-rax-disp8* (list "[rax+2*rax+disp8]"
                                                                         "[rcx+2*rax+disp8]"
                                                                         "[rdx+2*rax+disp8]"
                                                                         "[rbx+2*rax+disp8]"
                                                                         "[rsp+2*rax+disp8]"
                                                                         "[rbp+2*rax+disp8]"
                                                                         "[rsi+2*rax+disp8]"
                                                                         "[rdi+2*rax+disp8]"   ; [rxx+2*rax+disp8]
                                                                         "[rax+2*rcx+disp8]"
                                                                         "[rcx+2*rcx+disp8]"
                                                                         "[rdx+2*rcx+disp8]"
                                                                         "[rbx+2*rcx+disp8]"
                                                                         "[rsp+2*rcx+disp8]"
                                                                         "[rbp+2*rcx+disp8]"
                                                                         "[rsi+2*rcx+disp8]"
                                                                         "[rdi+2*rcx+disp8]"   ; [rxx+2*rcx+disp8]
                                                                         "[rax+2*rdx+disp8]"
                                                                         "[rcx+2*rdx+disp8]"
                                                                         "[rdx+2*rdx+disp8]"
                                                                         "[rbx+2*rdx+disp8]"
                                                                         "[rsp+2*rdx+disp8]"
                                                                         "[rbp+2*rdx+disp8]"
                                                                         "[rsi+2*rdx+disp8]"
                                                                         "[rdi+2*rdx+disp8]"   ; [rxx+2*rdx+disp8]
                                                                         "[rax+2*rbx+disp8]"
                                                                         "[rcx+2*rbx+disp8]"
                                                                         "[rdx+2*rbx+disp8]"
                                                                         "[rbx+2*rbx+disp8]"
                                                                         "[rsp+2*rbx+disp8]"
                                                                         "[rbp+2*rbx+disp8]"
                                                                         "[rsi+2*rbx+disp8]"
                                                                         "[rdi+2*rbx+disp8]")) ; [rxx+2*rbx+disp8]
; begins from 0b10000000
(defparameter *reg64-sib-list-no-scale-from-rax-1-times-rax-disp8* (list "[rax+4*rax+disp8]"
                                                                         "[rcx+4*rax+disp8]"
                                                                         "[rdx+4*rax+disp8]"
                                                                         "[rbx+4*rax+disp8]"
                                                                         "[rsp+4*rax+disp8]"
                                                                         "[rbp+4*rax+disp8]"
                                                                         "[rsi+4*rax+disp8]"
                                                                         "[rdi+4*rax+disp8]"   ; [rxx+4*rax+disp8]
                                                                         "[rax+4*rcx+disp8]"
                                                                         "[rcx+4*rcx+disp8]"
                                                                         "[rdx+4*rcx+disp8]"
                                                                         "[rbx+4*rcx+disp8]"
                                                                         "[rsp+4*rcx+disp8]"
                                                                         "[rbp+4*rcx+disp8]"
                                                                         "[rsi+4*rcx+disp8]"
                                                                         "[rdi+4*rcx+disp8]"   ; [rxx+4*rcx+disp8]
                                                                         "[rax+4*rdx+disp8]"
                                                                         "[rcx+4*rdx+disp8]"
                                                                         "[rdx+4*rdx+disp8]"
                                                                         "[rbx+4*rdx+disp8]"
                                                                         "[rsp+4*rdx+disp8]"
                                                                         "[rbp+4*rdx+disp8]"
                                                                         "[rsi+4*rdx+disp8]"
                                                                         "[rdi+4*rdx+disp8]"   ; [rxx+4*rdx+disp8]
                                                                         "[rax+4*rbx+disp8]"
                                                                         "[rcx+4*rbx+disp8]"
                                                                         "[rdx+4*rbx+disp8]"
                                                                         "[rbx+4*rbx+disp8]"
                                                                         "[rsp+4*rbx+disp8]"
                                                                         "[rbp+4*rbx+disp8]"
                                                                         "[rsi+4*rbx+disp8]"
                                                                         "[rdi+4*rbx+disp8]")) ; [rxx+4*rbx+disp8]

; begins from 0b11000000
(defparameter *reg64-sib-list-no-scale-from-rax-1-times-rax-disp8* (list "[rax+8*rax+disp8]"
                                                                         "[rcx+8*rax+disp8]"
                                                                         "[rdx+8*rax+disp8]"
                                                                         "[rbx+8*rax+disp8]"
                                                                         "[rsp+8*rax+disp8]"
                                                                         "[rbp+8*rax+disp8]"
                                                                         "[rsi+8*rax+disp8]"
                                                                         "[rdi+8*rax+disp8]"   ; [rxx+8*rax+disp8]
                                                                         "[rax+8*rcx+disp8]"
                                                                         "[rcx+8*rcx+disp8]"
                                                                         "[rdx+8*rcx+disp8]"
                                                                         "[rbx+8*rcx+disp8]"
                                                                         "[rsp+8*rcx+disp8]"
                                                                         "[rbp+8*rcx+disp8]"
                                                                         "[rsi+8*rcx+disp8]"
                                                                         "[rdi+8*rcx+disp8]"   ; [rxx+8*rcx+disp8]
                                                                         "[rax+8*rdx+disp8]"
                                                                         "[rcx+8*rdx+disp8]"
                                                                         "[rdx+8*rdx+disp8]"
                                                                         "[rbx+8*rdx+disp8]"
                                                                         "[rsp+8*rdx+disp8]"
                                                                         "[rbp+8*rdx+disp8]"
                                                                         "[rsi+8*rdx+disp8]"
                                                                         "[rdi+8*rdx+disp8]"   ; [rxx+8*rdx+disp8]
                                                                         "[rax+8*rbx+disp8]"
                                                                         "[rcx+8*rbx+disp8]"
                                                                         "[rdx+8*rbx+disp8]"
                                                                         "[rbx+8*rbx+disp8]"
                                                                         "[rsp+8*rbx+disp8]"
                                                                         "[rbp+8*rbx+disp8]"
                                                                         "[rsi+8*rbx+disp8]"
                                                                         "[rdi+8*rbx+disp8]")) ; [rxx+8*rbx+disp8]

; begins from 0b00000000
(defparameter *reg64-sib-list-no-scale-from-rax-rax-disp32* (list "[rax+rax+disp32]" "[rcx+rax+disp32]" "[rdx+rax+disp32]" "[rbx+rax+disp32]"
                                                                  "[rsp+rax+disp32]" "[rbp+rax+disp32]" "[rsi+rax+disp32]" "[rdi+rax+disp32]"   ; [rxx+rax+disp32]
                                                                  "[rax+rcx+disp32]" "[rcx+rcx+disp32]" "[rdx+rcx+disp32]" "[rbx+rcx+disp32]"
                                                                  "[rsp+rcx+disp32]" "[rbp+rcx+disp32]" "[rsi+rcx+disp32]" "[rdi+rcx+disp32]"   ; [rxx+rcx+disp32]
                                                                  "[rax+rdx+disp32]" "[rcx+rdx+disp32]" "[rdx+rdx+disp32]" "[rbx+rdx+disp32]"
                                                                  "[rsp+rdx+disp32]" "[rbp+rdx+disp32]" "[rsi+rdx+disp32]" "[rdi+rdx+disp32]"   ; [rxx+rdx+disp32]
                                                                  "[rax+rbx+disp32]" "[rcx+rbx+disp32]" "[rdx+rbx+disp32]" "[rbx+rbx+disp32]"
                                                                  "[rsp+rbx+disp32]" "[rbp+rbx+disp32]" "[rsi+rbx+disp32]" "[rdi+rbx+disp32]")) ; [rxx+rbx+disp32]

; begins from 0b00101000
(defparameter *reg64-sib-list-no-scale-from-rax-rbx-disp32* (list "[rax+rbp+disp32]" "[rcx+rbp+disp32]" "[rdx+rbp+disp32]" "[rbx+rbp+disp32]"
                                                                  "[rsp+rbp+disp32]" "[rbp+rbp+disp32]" "[rsi+rbp+disp32]" "[rdi+rbp+disp32]"   ; [rxx+rbp+disp32]
                                                                  "[rax+rsi+disp32]" "[rcx+rsi+disp32]" "[rdx+rsi+disp32]" "[rbx+rsi+disp32]"
                                                                  "[rsp+rsi+disp32]" "[rbp+rsi+disp32]" "[rsi+rsi+disp32]" "[rdi+rsi+disp32]"   ; [rxx+rsi+disp32]
                                                                  "[rax+rdi+disp32]" "[rcx+rdi+disp32]" "[rdx+rdi+disp32]" "[rbx+rdi+disp32]"
                                                                  "[rsp+rdi+disp32]" "[rbp+rdi+disp32]" "[rsi+rdi+disp32]" "[rdi+rdi+disp32]")) ; [rxx+rdi+disp32]
; begins from 0b00000000
(defparameter *reg64-sib-list-no-scale-from-rax-1-times-rax-disp32* (list "[rax+1*rax+disp32]"
                                                                          "[rcx+1*rax+disp32]"
                                                                          "[rdx+1*rax+disp32]"
                                                                          "[rbx+1*rax+disp32]"
                                                                          "[rsp+1*rax+disp32]"
                                                                          "[rbp+1*rax+disp32]"
                                                                          "[rsi+1*rax+disp32]"
                                                                          "[rdi+1*rax+disp32]"   ; [rxx+1*rax+disp32]
                                                                          "[rax+1*rcx+disp32]"
                                                                          "[rcx+1*rcx+disp32]"
                                                                          "[rdx+1*rcx+disp32]"
                                                                          "[rbx+1*rcx+disp32]"
                                                                          "[rsp+1*rcx+disp32]"
                                                                          "[rbp+1*rcx+disp32]"
                                                                          "[rsi+1*rcx+disp32]"
                                                                          "[rdi+1*rcx+disp32]"   ; [rxx+1*rcx+disp32]
                                                                          "[rax+1*rdx+disp32]"
                                                                          "[rcx+1*rdx+disp32]"
                                                                          "[rdx+1*rdx+disp32]"
                                                                          "[rbx+1*rdx+disp32]"
                                                                          "[rsp+1*rdx+disp32]"
                                                                          "[rbp+1*rdx+disp32]"
                                                                          "[rsi+1*rdx+disp32]"
                                                                          "[rdi+1*rdx+disp32]"   ; [rxx+1*rdx+disp32]
                                                                          "[rax+1*rbx+disp32]"
                                                                          "[rcx+1*rbx+disp32]"
                                                                          "[rdx+1*rbx+disp32]"
                                                                          "[rbx+1*rbx+disp32]"
                                                                          "[rsp+1*rbx+disp32]"
                                                                          "[rbp+1*rbx+disp32]"
                                                                          "[rsi+1*rbx+disp32]"
                                                                          "[rdi+1*rbx+disp32]")) ; [rxx+1*rbx+disp32]
; begins from 0b01000000
(defparameter *reg64-sib-list-no-scale-from-rax-2-times-rax-disp32* (list "[rax+2*rax+disp32]"
                                                                          "[rcx+2*rax+disp32]"
                                                                          "[rdx+2*rax+disp32]"
                                                                          "[rbx+2*rax+disp32]"
                                                                          "[rsp+2*rax+disp32]"
                                                                          "[rbp+2*rax+disp32]"
                                                                          "[rsi+2*rax+disp32]"
                                                                          "[rdi+2*rax+disp32]"   ; [rxx+2*rax+disp32]
                                                                          "[rax+2*rcx+disp32]"
                                                                          "[rcx+2*rcx+disp32]"
                                                                          "[rdx+2*rcx+disp32]"
                                                                          "[rbx+2*rcx+disp32]"
                                                                          "[rsp+2*rcx+disp32]"
                                                                          "[rbp+2*rcx+disp32]"
                                                                          "[rsi+2*rcx+disp32]"
                                                                          "[rdi+2*rcx+disp32]"   ; [rxx+2*rcx+disp32]
                                                                          "[rax+2*rdx+disp32]"
                                                                          "[rcx+2*rdx+disp32]"
                                                                          "[rdx+2*rdx+disp32]"
                                                                          "[rbx+2*rdx+disp32]"
                                                                          "[rsp+2*rdx+disp32]"
                                                                          "[rbp+2*rdx+disp32]"
                                                                          "[rsi+2*rdx+disp32]"
                                                                          "[rdi+2*rdx+disp32]"   ; [rxx+2*rdx+disp32]
                                                                          "[rax+2*rbx+disp32]"
                                                                          "[rcx+2*rbx+disp32]"
                                                                          "[rdx+2*rbx+disp32]"
                                                                          "[rbx+2*rbx+disp32]"
                                                                          "[rsp+2*rbx+disp32]"
                                                                          "[rbp+2*rbx+disp32]"
                                                                          "[rsi+2*rbx+disp32]"
                                                                          "[rdi+2*rbx+disp32]")) ; [rxx+2*rbx+disp32]
; begins from 0b10000000
(defparameter *reg64-sib-list-no-scale-from-rax-1-times-rax-disp32* (list "[rax+4*rax+disp32]"
                                                                          "[rcx+4*rax+disp32]"
                                                                          "[rdx+4*rax+disp32]"
                                                                          "[rbx+4*rax+disp32]"
                                                                          "[rsp+4*rax+disp32]"
                                                                          "[rbp+4*rax+disp32]"
                                                                          "[rsi+4*rax+disp32]"
                                                                          "[rdi+4*rax+disp32]"   ; [rxx+4*rax+disp32]
                                                                          "[rax+4*rcx+disp32]"
                                                                          "[rcx+4*rcx+disp32]"
                                                                          "[rdx+4*rcx+disp32]"
                                                                          "[rbx+4*rcx+disp32]"
                                                                          "[rsp+4*rcx+disp32]"
                                                                          "[rbp+4*rcx+disp32]"
                                                                          "[rsi+4*rcx+disp32]"
                                                                          "[rdi+4*rcx+disp32]"   ; [rxx+4*rcx+disp32]
                                                                          "[rax+4*rdx+disp32]"
                                                                          "[rcx+4*rdx+disp32]"
                                                                          "[rdx+4*rdx+disp32]"
                                                                          "[rbx+4*rdx+disp32]"
                                                                          "[rsp+4*rdx+disp32]"
                                                                          "[rbp+4*rdx+disp32]"
                                                                          "[rsi+4*rdx+disp32]"
                                                                          "[rdi+4*rdx+disp32]"   ; [rxx+4*rdx+disp32]
                                                                          "[rax+4*rbx+disp32]"
                                                                          "[rcx+4*rbx+disp32]"
                                                                          "[rdx+4*rbx+disp32]"
                                                                          "[rbx+4*rbx+disp32]"
                                                                          "[rsp+4*rbx+disp32]"
                                                                          "[rbp+4*rbx+disp32]"
                                                                          "[rsi+4*rbx+disp32]"
                                                                          "[rdi+4*rbx+disp32]")) ; [rxx+4*rbx+disp32]

; begins from 0b11000000
(defparameter *reg64-sib-list-no-scale-from-rax-1-times-rax-disp32* (list "[rax+8*rax+disp32]"
                                                                          "[rcx+8*rax+disp32]"
                                                                          "[rdx+8*rax+disp32]"
                                                                          "[rbx+8*rax+disp32]"
                                                                          "[rsp+8*rax+disp32]"
                                                                          "[rbp+8*rax+disp32]"
                                                                          "[rsi+8*rax+disp32]"
                                                                          "[rdi+8*rax+disp32]"   ; [rxx+8*rax+disp32]
                                                                          "[rax+8*rcx+disp32]"
                                                                          "[rcx+8*rcx+disp32]"
                                                                          "[rdx+8*rcx+disp32]"
                                                                          "[rbx+8*rcx+disp32]"
                                                                          "[rsp+8*rcx+disp32]"
                                                                          "[rbp+8*rcx+disp32]"
                                                                          "[rsi+8*rcx+disp32]"
                                                                          "[rdi+8*rcx+disp32]"   ; [rxx+8*rcx+disp32]
                                                                          "[rax+8*rdx+disp32]"
                                                                          "[rcx+8*rdx+disp32]"
                                                                          "[rdx+8*rdx+disp32]"
                                                                          "[rbx+8*rdx+disp32]"
                                                                          "[rsp+8*rdx+disp32]"
                                                                          "[rbp+8*rdx+disp32]"
                                                                          "[rsi+8*rdx+disp32]"
                                                                          "[rdi+8*rdx+disp32]"   ; [rxx+8*rdx+disp32]
                                                                          "[rax+8*rbx+disp32]"
                                                                          "[rcx+8*rbx+disp32]"
                                                                          "[rdx+8*rbx+disp32]"
                                                                          "[rbx+8*rbx+disp32]"
                                                                          "[rsp+8*rbx+disp32]"
                                                                          "[rbp+8*rbx+disp32]"
                                                                          "[rsi+8*rbx+disp32]"
                                                                          "[rdi+8*rbx+disp32]")) ; [rxx+8*rbx+disp32]

; aadressing forms which need SIB.
(defparameter *reg64-reg-indirect-rip-0*        (list "[rip0]"))
(defparameter *reg64-reg-indirect-rip-1*        (list "[rip1]"))
(defparameter *reg64-reg-indirect-rip*          (list "[rip]"))
(defparameter *reg64-reg-indirect-rip-disp32-0* (list "[rip0+disp32]"))
(defparameter *reg64-reg-indirect-rip-disp32-1* (list "[rip1+disp32]"))
(defparameter *reg64-reg-indirect-rip-disp32*   (list "[rip+disp32]"))

(defparameter *create-addressing-form-instances-list*
  (list
    (list #b000 *old-low-reg8-list*                    'x86-old-8-bits-low-reg)
    (list #b100 *old-high-reg8-list*                   'x86-old-8-bits-high-reg)
    (list #b000 *old-reg16-list*                       'x86-old-16-bits-reg)
    (list #b000 *old-reg32-list*                       'x86-old-32-bits-reg)
    (list #b000 *old-reg64-list*                       'x86-old-64-bits-reg)
    (list #b100 *new-low-reg8-list-from-spl*           'x86-new-8-bits-reg-rex.r-0)
    (list #b000 *new-low-reg8-list-from-r8b*           'x86-new-8-bits-reg-rex.r-1)
    (list #b000 *new-reg16-list*                       'x86-new-16-bits-reg)
    (list #b000 *new-reg32-list*                       'x86-new-32-bits-reg)
    (list #b000 *new-reg64-list*                       'x86-new-64-bits-reg)
    (list #b000 *mmx-reg-list*                         'x86-mmx-reg)
    (list #b000 *xmm-reg-list-from-xmm0*               'x86-old-xmm-reg)
    (list #b000 *xmm-reg-list-from-xmm8*               'x86-new-xmm-reg)
    (list #b000 *ymm-reg-list-from-ymm0*               'x86-old-ymm-reg)
    (list #b000 *ymm-reg-list-from-ymm8*               'x86-new-ymm-reg)
    (list #b000 *zmm-reg-list-from-zmm0*               'x86-old-zmm-reg)
    (list #b000 *zmm-reg-list-from-zmm8*               'x86-new-zmm-reg)
    (list #b000 *reg64-indirect-list-from-rax*         'x64-old-reg-indirect-does-not-need-sib)
    (list #b110 *reg64-indirect-list-from-rsi*         'x64-old-reg-indirect-does-not-need-sib)
    (list #b000 *reg64-indirect-list-from-r8*          'x64-new-reg-indirect-does-not-need-sib)
    (list #b110 *reg64-indirect-list-from-r14*         'x64-new-reg-indirect-does-not-need-sib)
    (list #b101 *reg64-reg-indirect-rip-0*        'x86-rip-disp32-0)
    (list #b101 *reg64-reg-indirect-rip-1*        'x86-rip-disp32-1)
    (list #b101 *reg64-reg-indirect-rip*          'x86-rip-disp32)
    (list #b101 *reg64-reg-indirect-rip-disp32-0* 'x86-rip-disp32-0)
    (list #b101 *reg64-reg-indirect-rip-disp32-1* 'x86-rip-disp32-1)
    (list #b101 *reg64-reg-indirect-rip-disp32*   'x86-rip-disp32)))
