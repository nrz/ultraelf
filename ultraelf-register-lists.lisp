;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf) 

(defparameter *old-low-reg8-list*  (list  "al"  "cl"  "dl"  "bl"))
(defparameter *old-high-reg8-list* (list  "ah"  "ch"  "dh"  "bh"))
(defparameter *new-low-reg8-list*  (list "spl" "bpl" "sil" "dil" "r8b" "r9b" "r10b" "r11b" "r12b" "r13b" "r14b" "r15b"))
(defparameter *old-reg8-list* (append *old-low-reg8-list* *old-high-reg8-list*))
(defparameter *low-reg8-list* (append *old-low-reg8-list* *new-low-reg8-list*))
(defparameter *reg8-list* (append *old-low-reg8-list* *old-high-reg8-list* *new-low-reg8-list*))

(defparameter *old-reg16-list* (list  "ax"  "cx"   "dx"   "bx"   "sp"   "bp"   "si"   "di"))
(defparameter *new-reg16-list* (list "r8w" "r9w" "r10w" "r11w" "r12w" "r13w" "r14w" "r15w"))
(defparameter *reg16-list* (append *old-reg16-list* *new-reg16-list*))

(defparameter *old-reg32-list* (list "eax" "ecx"  "edx"  "ebx"  "esp"  "ebp"  "esi"  "edi"))
(defparameter *new-reg32-list* (list "r8d" "r9d" "r10d" "r11d" "r12d" "r13d" "r14d" "r15d"))
(defparameter *reg32-list* (append *old-reg32-list* *new-reg32-list*))

(defparameter *old-reg64-list* (list "rax" "rcx" "rdx" "rbx" "rsp" "rbp" "rsi" "rdi"))
(defparameter *new-reg64-list* (list  "r8"  "r9" "r10" "r11" "r12" "r13" "r14" "r15"))
(defparameter *reg64-list* (append *old-reg64-list* *new-reg64-list*))

