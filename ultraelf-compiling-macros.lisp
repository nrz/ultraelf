;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defmacro compile-ultraelf ()
  (asdf:oos 'asdf:load-op 'ultraelf))

(defmacro c-u ()
  (asdf:oos 'asdf:load-op 'ultraelf))
