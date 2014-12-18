;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

;; create addressing form instances.
;; note: indirect addressing form that use `[` & `]` need backslash in SBCL REPL,
;; eg: ULTRAELF> \[rax\]
(loop for register-list in *create-addressing-form-instances-list*
      do (loop for register-index below (length (second register-list))
               do (setf (symbol-value (intern (string-upcase (nth register-index (second register-list)))))
                        (make-instance (third register-list)
                                       :r/m (+ register-index (first register-list))
                                       :name (string-upcase (nth register-index (second register-list)))))))
