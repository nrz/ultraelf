;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :arm
  (:use :essentials)
  (:import-from
    :cl
    :nil)
  (:import-from
    :ultraelf
    ;; printing functions.
    :print-hex
    ;; string functions.
    :parse-number
    ;; list-handling functions.
    :get-list
    :sort-sublists-shortest-first
    ;; emit-code functions.
    :emit
    :create-syntax-tree
    :create-and-eval-syntax-tree
    :emit-binary-code-for-one-instruction
    :emit-binary-code-list
    :emit-binary-code
    :emit-binary-code-and-print-hex
    :assemble
    :assemble-and-print-hex
    :assemble-alternatives
    :assemble-alternatives-and-print-hex
    :check-args
    :get-all-encodings-for-syntax-tree
    :get-all-encodings-for-syntax-tree-and-print-hex
    ;; symbols.
    :convert-string-to-symbol-if-symbol-exists
    ;; instruction classes' slots.
    :name
    :req-operands
    :code-format
    :arch-flags
    :is-asm-instruction
    :is-arm-instruction
    :modifies-flags
    :flags-affected
    :flags-zeroed
    :flags-set
    :flags-undefined
    :depends-on-flags
    :flags-depends-on
    :depends-on-stack
    :alt-code
    ;; instruction class names.
    :arm-asm-instruction)
  (:export
    :emit-with-format-and-operands-arm))
