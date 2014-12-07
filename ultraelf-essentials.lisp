;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :essentials
  (:import-from :cl
                ;; t, nil
                :t :nil
                ;; code flow macros.
                :cond :loop :when :unless
                ;; lambda.
                :lambda
                ;; define instructions.
                :defun :defclass :defpackage :defparameter :defmethod
                ;; comparison functions and related stuff.
                :eq :eql :equal :equalp :null :listp :not
                ;; error handling.
                :error
                ;; printing.
                :format :princ :print
                ;; hash tables.
                :gethash :make-hash-table
                ;; package handling.
                :in-package
                ;; symbols.
                :intern
                ;; variables.
                :setf
                ;; lists and other sequences.
                :every :some :notevery :notany
                :append :concatenate
                :list
                :nth :first :second :third :fourth :fifth :sixth :seventh :eighth :ninth :tenth :rest
                :push :pushnew
                ;; numbers.
                :parse-integer
                ;; CLOS.
                :make-instance :slot-value))

(in-package :essentials)
(cl-user::do-symbols (essentials-sym (cl-user::find-package :essentials)) (cl-user::export essentials-sym))
