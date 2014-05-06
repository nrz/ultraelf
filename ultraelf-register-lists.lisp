;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

;; register lists. must be ordered in ModRM order.
(defparameter *old-low-reg8-list*  (list  "al"  "cl"   "dl"   "bl"))
(defparameter *old-high-reg8-list* (list  "ah"  "ch"   "dh"   "bh"))
(defparameter *old-reg16-list*     (list  "ax"  "cx"   "dx"   "bx"   "sp"   "bp"   "si"   "di"))
(defparameter *old-reg32-list*     (list "eax" "ecx"  "edx"  "ebx"  "esp"  "ebp"  "esi"  "edi"))
(defparameter *old-reg64-list*     (list "rax" "rcx"  "rdx"  "rbx"  "rsp"  "rbp"  "rsi"  "rdi"))
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
