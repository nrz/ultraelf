;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defclass asm-reader ()
  ((current-state
     :reader current-state
     :initform "start-of-line"
     :documentation "The state of the finite state machine in asm mode.")
   (current-lisp-state
     :reader current-lisp-state
     :initform "regular"
     :documentation "The state of the finite state machine in Lisp mode.")
   (state-stack
     :reader state-stack
     :initform nil
     :documentation "The state stack needed by the finite state machine.")
   (ast-string
     :reader ast-string
     :initform "(list "
     :documentation "The abstract syntax tree produced so far by the reader. Stored as a string that needs to be read and eval'd.")
   (lisp-code-string
     :reader lisp-code-string
     :initform ""
     :documentation "Lisp code string read from the source, will be evaluated.")
   (is-there-code-on-this-line
     :reader is-there-code-on-this-line
     :initform nil
     :documentation "A boolean value which tells whether this line has any code.")
   (invalid-last-characters
     :reader invalid-last-characters
     :initform (list "'" " " "(" ")")
     :documentation "Some invalid last characters.")))

(defgeneric push-state (asm-reader)
  (:documentation "store the current state on top of the state stack."))

(defgeneric pop-state (asm-reader)
  (:documentation "pop a state from the top of the state stack."))

(defmethod push-state ((asm-reader asm-reader))
  (push (slot-value asm-reader 'current-state) (slot-value asm-reader 'state-stack)))

(defmethod pop-state ((asm-reader asm-reader))
  (pop (slot-value asm-reader 'state-stack)))
