;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :cl-user)

(defpackage :essentials
  (:import-from :cl
                ;; t.
                :t
                ;; Lisp basics.
                :cons :eval :lambda :read :progn :let :let*
                ;; logical operators.
                :and :or :not
                ;; mathematics.
                :+ :- :* :/ :ash :logand :logior :logxor :lognot
                ;; code flow macros.
                :cond :loop :when :unless
                ;; define instructions.
                :defun :defclass :defpackage :defparameter :defmethod
                ;; function calls.
                :apply :funcall
                ;; function definitions.
                :&key :&optional :&rest
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
                :every :some :subseq :notevery :notany
                :length
                :mapcan :mapcar
                :append :concatenate :nconc
                :list
                :nth :first :second :third :fourth :fifth :sixth :seventh :eighth :ninth :tenth :rest
                :push :pushnew
                ;; numbers.
                :parse-integer
                ;; CLOS.
                :make-instance :slot-value))

(in-package :essentials)
(cl-user::do-symbols (essentials-sym (cl-user::find-package :essentials)) (cl-user::export essentials-sym))
