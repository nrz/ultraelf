;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :x16)

(defun emit-with-format-and-operands-x16 (code-format req-operands given-operands)
  "This function emits code (list of binary code bytes) for one x16 instruction variant."
  (let*
    ((encoding-type (first code-format))
     (my-args (get-list given-operands))
     (my-operands (get-list req-operands)))
    (check-args my-operands my-args)
    (error "x16 encoding not yet implemented")))
