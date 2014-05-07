;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

;; create addressing form instances.
;; note: indirect addressing form that use `[` & `]` need backslash in SBCL REPL,
;; eg: ULTRAELF> \[rax\]
(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for i below (length *create-addressing-form-instances-list*)
        do (loop for j below (length (second (nth i *create-addressing-form-instances-list*)))
                 do (setf (symbol-value (intern (string-upcase (nth j (second (nth i *create-addressing-form-instances-list*))))))
                          (make-instance (third (nth i *create-addressing-form-instances-list*)) :r/m (+ j (first (nth i *create-addressing-form-instances-list*))))))))
