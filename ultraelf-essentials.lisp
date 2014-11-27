;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :essentials
  (:import-from :cl
                ;; t, nil
                :t :nil
                ;; conditionals.
                :cond :loop
                ;; define instructions.
                :defun :defclass :defpackage :defparameter :defmethod
                ;; comparison functions.
                :eq :eql :equal :equalp :null :listp
                ;; error handling.
                :error
                ;; printing.
                :format :princ :print
                ;; hash tables.
                :gethash :make-hash-table
                ;; package handling.
                :in-package
                ;; variables.
                :setf
                ;; lists and other sequences.
                :append :concatenate :list :nth :first :second :third :fourth :fifth :sixth :seventh :eighth :ninth :tenth :rest :push :pushnew
                ;; numbers.
                :parse-integer
                ;; CLOS.
                :make-instance :slot-value)
  (:export
    ;; t, nil
    :t :nil
    ;; conditionals.
    :cond :loop
    ;; define instructions.
    :defun :defclass :defpackage :defparameter :defmethod
    ;; comparison functions.
    :eq :eql :equal :equalp :null :listp
    ;; error handling.
    :error
    ;; printing.
    :format :princ :print
    ;; hash tables.
    :gethash :make-hash-table
    ;; package handling.
    :in-package
    ;; variables.
    :setf
    ;; lists and other sequences.
    :append :concatenate :list :nth :first :second :third :fourth :fifth :sixth :seventh :eighth :ninth :tenth :rest :push :pushnew
    ;; numbers.
    :parse-integer
    ;; CLOS.
    :make-instance :slot-value))
