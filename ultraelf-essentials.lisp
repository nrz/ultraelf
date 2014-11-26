;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :essentials
  (:import-from :cl
                :t
                :nil
                :if
                :cond
                :unless
                :defun
                :defclass
                :defpackage
                :defparameter
                :defmethod
                :equalp
                :format
                :gethash
                :in-package
                :list
                :make-hash-table
                :make-instance
                :nth
                :princ
                :print
                :push
                :setf
                :slot-value)
  (:export :t
           :nil
           :if
           :cond
           :unless
           :defun
           :defclass
           :defpackage
           :defparameter
           :equalp
           :format
           :gethash
           :in-package
           :list
           :make-hash-table
           :make-instance
           :nth
           :princ
           :print
           :push
           :setf
           :slot-value))
