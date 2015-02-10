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
                :cons :eval :lambda :read :read-from-string :progn :let :let* :macrolet :symbol-macrolet
                ;; logical operators.
                :and :or :not
                ;; mathematics.
                :+ :- :* :/ :mod :expt :1+ :1- :incf :decf :ash :logand :logior :logxor :lognot
                ;; code flow macros.
                :if :cond :loop :when :unless
                ;; define instructions.
                :defun :defmacro :defclass :defpackage :defparameter :defmethod
                ;; function calls.
                :apply :funcall
                ;; function and macro definitions.
                :&key :&optional :&rest :&body
                ;; comparison functions and related stuff.
                :< :> :<= :>= :eq :eql :equal :equalp :null :listp :not :functionp :numberp :stringp
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
                :elt :nth :first :second :third :fourth :fifth :sixth :seventh :eighth :ninth :tenth :rest
                :push :pushnew
                :reverse :nreverse
                ;; numbers.
                :parse-integer
                ;; CLOS.
                :make-instance :slot-value))

(in-package :essentials)
(cl-user::do-symbols (essentials-sym (cl-user::find-package :essentials)) (cl-user::export essentials-sym))
