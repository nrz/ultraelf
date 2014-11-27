;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :arm
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
    :assemble
    :assemble-alternatives
    :get-all-encodings-for-syntax-tree
    ;; instruction class names.
    :arm-asm-instruction))
