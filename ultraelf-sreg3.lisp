;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defparameter *sreg3-hash-table-x64* (make-hash-table :test 'equalp))
(setf (gethash "es" *sreg2-hash-table-x64*) #b000)
(setf (gethash "cs" *sreg2-hash-table-x64*) #b001)
(setf (gethash "ss" *sreg2-hash-table-x64*) #b010)
(setf (gethash "ds" *sreg2-hash-table-x64*) #b011)
(setf (gethash "fs" *sreg2-hash-table-x64*) #b100)
(setf (gethash "gs" *sreg2-hash-table-x64*) #b101)
