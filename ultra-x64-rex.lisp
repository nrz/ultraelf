;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :x64)

(defun emit-rex-byte (rex-w rex-r rex-x rex-b)
  "This function emits a REX prefix as requested."
  (list (logior #x40
                rex-b
                (ash rex-x 1)
                (ash rex-r 2)
                (ash rex-w 3))))
