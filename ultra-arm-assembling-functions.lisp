;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :arm)

(defun emit-with-format-and-operands-arm (code-format req-operands given-operands)
  "This function emits code (list of binary code bytes) for one ARM instruction variant."
  (let*
    ((my-args (get-list given-operands))
     (my-operands (get-list req-operands)))
    (check-args my-operands my-args)
    (error "ARM encoding not yet implemented")))
