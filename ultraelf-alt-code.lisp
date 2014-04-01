;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defparameter *alt-mov-reg64-reg64-push-pop*
  ;; clean, does not modify flags.
  #a
  push  arg2
  pop   arg1
  #e)

(defparameter *alt-mov-reg64-reg64-lea*
  ;; clean, does not modify flags.
  #a
  lea   arg1,[arg2]
  #e)

(defparameter *alt-lea-reg64-m32-mov*
  ;; clean, does not modify flags.
  #a
  mov   arg1,arg2
  #e)

(defparameter *alt-lea-reg64-indirect-reg64-mov*
  ;; clean, does not modify flags.
  #a
  mov   arg1,arg2
  #e)

(defparameter *alt-push-reg64-lea-mov*
  #a
  lea   rsp,[rsp-8]
  mov   [rsp],arg1
  #e)

(defparameter *alt-push-imm8-lea-mov*
  #a
  lea   rsp,[rsp-8]
  mov   [rsp],qword arg1
  #e)

(defparameter *alt-push-imm16-lea-mov*
  #a
  lea   rsp,[rsp-8]
  mov   [rsp],qword arg1
  #e)

(defparameter *alt-push-imm32-lea-mov*
  #a
  lea   rsp,[rsp-8]
  mov   [rsp],qword arg1
  #e)

(defparameter *alt-pop-reg64-lea-mov-lea*
  #a
  mov   arg1,[rsp]
  lea   rsp,[rsp+8]
  #e)

(defparameter *alt-add-reg64-reg64-lea*
  ;; clean, does not modify flags.
  ;; overflow?
  #a
  lea   arg1,[arg1+arg1]
  #e)

(defparameter *alt-xchg-reg64-reg64-push-pop-1*
  ;; clean, does not modify flags.
  #a
  push  arg1
  push  arg2
  pop   arg1
  pop   arg2
  #e)

(defparameter *alt-xchg-reg64-reg64-push-pop-2*
  ;; clean, does not modify flags.
  #a
  push  arg2
  push  arg1
  pop   arg2
  pop   arg1
  #e)

(defparameter *alt-zero-reg-xor*
  ;; not clean.
  ;; modifies flags.
  #a
  xor   arg1,arg1
  #e)

(defparameter *alt-zero-reg-sub*
  ;; not clean.
  ;; modifies flags.
  #a
  sub   arg1,arg1
  #e)

(defparameter *alt-add-reg-reg-adc*
  ;; clean, modifies flags identically.
  #a
  clc
  adc   arg1,arg2
  #e)

(defparameter *alt-sub-reg-reg-sbb*
  ;; clean, modifies flags identically.
  #a
  clc
  sbb   arg1,arg2
  #e)

(defparameter *alt-cmp-reg64-any-push-sub-pop*
  ;; clean, modifies flags identically.
  #a
  push  arg1
  sub   arg1,arg2
  pop   arg1
  #e)

(defparameter *alt-and-reg1-reg1-test*
  ;; clean, modifies flags identically.
  #a
  test  arg1,arg1
  #e)

(defparameter *alt-or-reg1-reg1-test*
  ;; clean?
  #a
  test  arg1,arg1
  #e)

(defparameter *alt-test-reg1-reg1-or*
  ;; clean?
  #a
  or    arg1,arg1
  #e)

(defparameter *alt-test-reg1-reg1-and*
  ;; clean, modifies flags identically.
  #a
  and   arg1,arg1
  #e)

(defparameter *alt-call-push-jmp*
  ;; clean.
  ;; uses forward reference to a local label.
  #a
  push  qword >.label
  jmp   arg1
  .label:
  #e)

(defparameter *alt-ret-ret-0*
  ;; clean.
  #a
  ret   0
  #e)

(defparameter *alt-ret-near-ret-near-0*
  ;; clean.
  #a
  ret   near 0
  #e)

(defparameter *alt-ret-far-ret-far-0*
  ;; clean.
  #a
  ret   far 0
  #e)
