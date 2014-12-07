;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :x86
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
    :check-args
    :get-all-encodings-for-syntax-tree
    :get-msg-bit
    ;; instruction classes' slots.
    :allowed-targets
    :code-format
    :operands
    ;; addressing form classes' slots.
    :is-reg
    :reg-size
    :reg-name
    :is-x86-reg
    :is-memory-addressing
    :is-reg-indirect
    :displacement-size
    :needs-sib
    :r/m
    ;; addressing form classes' methods.
    :modrm.mod
    :modrm.r/m
    ;; x86 registers common to all x86.
    :al :cl :dl :bl
    :ah :ch :dh :bh
    :ax :cx :dx :bx
    :sp :bp :si :di))

(in-package :x86)
(cl-user::do-symbols (x86-sym (cl-user::find-package :x86)) (cl-user::export x86-sym))
