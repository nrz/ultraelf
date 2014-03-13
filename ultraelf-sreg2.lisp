;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defparameter *sreg2-hash-table-x64* (make-hash-table :test 'equalp))
(setf (gethash "es" *sreg2-hash-table-x64*) #b00)
(setf (gethash "cs" *sreg2-hash-table-x64*) #b01)
(setf (gethash "ss" *sreg2-hash-table-x64*) #b10)
(setf (gethash "ds" *sreg2-hash-table-x64*) #b11)
