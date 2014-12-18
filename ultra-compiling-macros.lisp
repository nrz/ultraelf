;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defmacro compile-ultraelf ()
  (asdf:oos 'asdf:load-op 'ultraelf))

(defmacro c-u ()
  (asdf:oos 'asdf:load-op 'ultraelf))
