;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :x64)

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

(defparameter *alt-zero-reg-sbb*
  ;; not clean.
  ;; modifies flags.
  #a
  clc
  sbb   arg1,arg1
  #e)

(defparameter *alt-neg-not-inc*
  ;; not clean.
  ;; modifies flags.
  #a
  neg   arg1
  inc   arg1
  #e)

(defparameter *alt-not-xor*
  ;; not clean?
  ;; modifies flags.
  #a
  xor   arg1,-1
  #e)

(defparameter *alt-not-neg-dec*
  ;; not clean.
  ;; modifies flags.
  #a
  neg   arg1
  dec   arg1
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

(defparameter *alt-dec-sub*
  ;; not clean, modifies carry.
  #a
  sub   arg1,1
  #e)

(defparameter *alt-dec-clc-sbb*
  ;; not clean, modifies carry.
  #a
  clc
  sbb   arg1,1
  #e)

(defparameter *alt-dec-stc-sub*
  ;; not clean, modifies carry.
  #a
  stc
  sbb   arg1,0
  #e)

(defparameter *alt-inc-add*
  ;; not clean, modifies carry.
  #a
  add   arg1,1
  #e)

(defparameter *alt-inc-clc-adc*
  ;; not clean, modifies carry.
  #a
  clc
  adc   arg1,1
  #e)

(defparameter *alt-inc-stc-adc*
  ;; not clean, modifies carry.
  #a
  stc
  adc   arg1,0
  #e)

(defparameter *alt-cmp-reg64-any-push-sub-pop*
  ;; clean, modifies flags identically.
  #a
  push  arg1
  sub   arg1,arg2
  pop   arg1
  #e)

(defparameter *alt-shl-rm32-rol-and*
  ;; not clean?
  #a
  rol    arg1,1
  and    arg1,0xfffffffe
  #e)

(defparameter *alt-shl-rm16-rol-and*
  ;; not clean?
  #a
  rol    arg1,1
  and    arg1,0xfffe
  #e)

(defparameter *alt-shl-rm8-rol-and*
  ;; not clean?
  #a
  rol    arg1,1
  and    arg1,0xfe
  #e)

(defparameter *alt-shl-rm32-rcl-and*
  ;; not clean?
  #a
  rcl    arg1,1
  and    arg1,0xfffffffe
  #e)

(defparameter *alt-shl-rm16-rcl-and*
  ;; not clean?
  #a
  rcl    arg1,1
  and    arg1,0xfffe
  #e)

(defparameter *alt-shl-rm8-rcl-and*
  ;; not clean?
  #a
  rcl    arg1,1
  and    arg1,0xfe
  #e)

(defparameter *alt-shr-rm32-sar-pushf-and-popf*
  ;; clean, modifies flags identically.
  #a
  sar   arg1,1
  pushf
  and   arg1,0x7fffffff
  popf
  #e)

(defparameter *alt-shr-rm16-sar-pushf-and-popf*
  ;; clean, modifies flags identically.
  #a
  sar   arg1,1
  pushf
  and   arg1,0x7fff
  popf
  #e)

(defparameter *alt-rol-shl-adc*
  ;; not clean?
  #a
  shl    arg1,1
  adc    arg1,0
  #e)

(defparameter *alt-rol-shl-jnc-inc*
  ;; not clean?
  #a
  shl    arg1,1
  jnc    >.label
  inc    arg1
  .label:
  #e)

(defparameter *alt-rol-shl-jnc-or*
  ;; not clean?
  #a
  shl    arg1,1
  jnc    >.label
  or     arg1,1
  .label:
  #e)

(defparameter *alt-rol-shl-jnc-xor*
  ;; not clean?
  #a
  shl    arg1,1
  jnc    >.label
  xor    arg1,1
  .label:
  #e)

(defparameter *alt-shr-rm8-sar-pushf-and-popf*
  ;; clean, modifies flags identically.
  #a
  sar   arg1,1
  pushf
  and   arg1,0x7f
  popf
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
