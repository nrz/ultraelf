;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defparameter *global-offset* 0)
(defparameter $ *global-offset*)
(defvar *emit-function-hash-table-x64*)
(defvar *sib-scale-hash-table-x64*)
