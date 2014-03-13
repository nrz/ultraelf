;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defparameter *sib-scale-hash-table-x64* (make-hash-table :test 'equalp))
(setf (gethash "1" *sib-scale-hash-table-x64*) #b00)
(setf (gethash "2" *sib-scale-hash-table-x64*) #b01)
(setf (gethash "4" *sib-scale-hash-table-x64*) #b10)
(setf (gethash "8" *sib-scale-hash-table-x64*) #b11)
