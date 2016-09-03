;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :x32)

(defun emit-with-format-and-operands-x32 (code-format req-operands given-operands)
  "This function emits code (list of binary code bytes) for one x32 instruction variant."
  (let*
    ((my-args (get-list given-operands))
     (my-operands (get-list req-operands)))
    (check-args my-operands my-args)
    (error "x32 encoding not yet implemented")))
