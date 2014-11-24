;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(defpackage :x64
  (:import-from :cl :defclass
                    :defpackage
                    :defparameter
                    :in-package
                    :list
                    :make-instance)
  (:import-from :ultraelf :x64-asm-instruction
                          :name
                          :operands
                          :code-string
                          :arch-flags))

(in-package :x64)

