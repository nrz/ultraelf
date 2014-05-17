;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

;; create addressing form instances.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for string-instruction-list in *create-argument-instances-list*
        do (loop for instruction-data-list in (first string-instruction-list)
                 do (setf (symbol-value (intern (string-upcase (first instruction-data-list))))
                          (make-instance (second string-instruction-list)
                                         :op-code (second instruction-data-list)
                                         :name (string-upcase (first instruction-data-list)))))))
