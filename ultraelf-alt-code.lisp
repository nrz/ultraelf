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
  ;; modifies flags.
  #a
  xor   arg1,arg1
  #e)

(defparameter *alt-zero-reg-sub*
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

(defparameter *alt-or-and-reg-reg-test*
  #a
  test  arg1,arg1
  #e)

(defparameter *alt-test-reg-reg-or*
  #a
  or    arg1,arg1
  #e)

(defparameter *alt-test-reg-reg-and*
  #a
  and   arg1,arg1
  #e)

