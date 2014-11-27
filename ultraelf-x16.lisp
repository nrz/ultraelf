;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :x16
  (:use :essentials)
  (:import-from
    :ultraelf
    ;; printing functions.
    :print-hex
    ;; list-handling functions.
    :get-list
    ;; emit-code functions.
    :emit
    :emit-hex
    :emit-modrm
    :emit-modrm-byte
    :assemble
    :assemble-alternatives
    :get-all-encodings-for-syntax-tree
    ;; instruction class names.
    :x16-asm-instruction
    ;; instruction classes' slots.
    :is-reg
    :reg-size
    :register-name
    :is-x86-register
    :is-old-reg
    :is-memory-addressing
    :is-register-indirect
    :code-format
    :operands
    :displacement-size
    :needs-sib
    :r/m
    ;; instruction classes' methods.
    :modrm.mod
    :modrm.r/m
    ;; x86 registers common to all x86.
    :al  :cl  :dl   :bl
    :ah  :ch  :dh   :bh
    :ax  :cx  :dx   :bx
    :sp  :bp  :si   :di
    ;; TODO: x16 register indirects.
    ;; :\[bx\] :\[bp\] :\[si\] :\[di\] ...
    ))
