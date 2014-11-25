;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :x64
  (:import-from :cl :defclass
                    :defpackage
                    :defparameter
                    :equalp
                    :gethash
                    :in-package
                    :list
                    :make-hash-table
                    :make-instance
                    :nth
                    :push)
  (:import-from :ultraelf
                :emit :emit-hex :x64-asm-instruction :name :operands :code-format :arch-flags
                :print-hex))

(in-package :x64)

