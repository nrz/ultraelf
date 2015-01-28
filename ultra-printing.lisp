;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun print-hex (my-number)
  (cond
    ((and
       (listp my-number)
       (listp (first my-number)))
     (mapcar #'print-hex my-number))
    ((listp my-number)
     (format nil "(~{~2,'0x~^ ~})" my-number))
    (t (format nil "~2,'0x" my-number))))

(defun print-hex-list (my-list)
  (mapcar #'(lambda (x) (print-hex x)) my-list))
