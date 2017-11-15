;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :x86
  (:import-from
    :ultraelf
    ;; global variables.
    :*global-offset* :$
    ;; printing functions.
    :print-hex
    ;; string functions.
    :parse-number
    ;; list-handling functions.
    :get-list
    :sort-sublists-shortest-first
    ;; emit-code functions.
    :emit
    :emit-modrm
    :emit-modrm-byte
    :emit-little-endian-number-in-n-bytes
    :emit-sign-extended-byte-for-n-bytes
    :emit-little-endian-sign-extended-dword-for-n-bytes
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
    :get-msg-bit
    ;; symbols.
    :convert-string-to-symbol-if-symbol-exists
    ;; instruction classes' slots.
    :name
    :req-operands
    :code-format
    :arch-flags
    :is-asm-instruction
    :is-x86-instruction
    :is-x16-instruction
    :is-x32-instruction
    :is-x64-instruction
    :modifies-flags
    :flags-affected
    :flags-zeroed
    :flags-set
    :flags-undefined
    :depends-on-flags
    :flags-depends-on
    :depends-on-stack
    :alt-code
    ;; addressing form classes' slots.
    :allowed-targets
    :is-reg
    :reg-size
    :reg-name
    :is-x86-reg
    :is-memory-addressing
    :is-reg-indirect
    :is-immediate
    :value
    :fits-in-unsigned-byte
    :fits-in-unsigned-word
    :fits-in-signed-byte
    :fits-in-signed-word
    :displacement-size
    :needs-sib
    :r/m
    ;; addressing form classes' methods.
    :modrm-mod
    :modrm-reg
    :modrm-r/m
    ;; x86 registers common to all x86.
    :al :cl :dl :bl
    :ah :ch :dh :bh
    :ax :cx :dx :bx
    :sp :bp :si :di))

(in-package :x86)
(cl-user::do-symbols (x86-sym (cl-user::find-package :x86)) (cl-user::export x86-sym))
