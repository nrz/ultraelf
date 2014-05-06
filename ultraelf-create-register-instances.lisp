;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

;; create register instances.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for i below (length *create-register-classes-list*)
        do (loop for j below (length (second (nth i *create-register-classes-list*)))
                 do (setf (symbol-value (intern (string-upcase (nth j (second (nth i *create-register-classes-list*))))))
                          (make-instance (third (nth i *create-register-classes-list*)) :modrm (+ j (first (nth i *create-register-classes-list*))))))))
