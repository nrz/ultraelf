;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defclass asm-reader ()
  ((current-state
     :accessor current-state
     :initform "start-of-line"
     :documentation "The state of the finite state machine in asm mode.")
   (current-lisp-state
     :accessor current-lisp-state
     :initform "regular"
     :documentation "The state of the finite state machine in Lisp mode.")
   (state-stack
     :accessor state-stack
     :initform nil
     :documentation "The state stack needed by the finite state machine.")
   (ast-string
     :accessor ast-string
     :initform "(list "
     :documentation "The abstract syntax tree produced so far by the reader. Stored as a string that needs to be read and eval'd.")
   (lisp-code-string
     :accessor lisp-code-string
     :initform ""
     :documentation "Lisp code string read from the source, will be evaluated.")
   (n-lisp-forms
     :accessor n-lisp-forms
     :initform 0
     :documentation "Number of recurrent Lisp forms.")
   (is-there-code-on-this-line
     :accessor is-there-code-on-this-line
     :initform nil
     :documentation "A boolean value which tells whether this line has any code.")
   (is-ready
     :accessor is-ready
     :initform nil
     :documentation "A boolean value which tells whether this finite state machine is in the accepting state.")))

(defgeneric push-state (asm-reader)
  (:documentation "store the current state on top of the state stack."))

(defgeneric push-current-and-set-state (state asm-reader)
  (:documentation "store the current state on top of the state stack and set a new state."))

(defgeneric push-state1 (state asm-reader)
  (:documentation "store the given state on top of the state stack."))

(defgeneric pop-state (asm-reader)
  (:documentation "pop a state from the top of the state stack and make it the current state."))

(defmethod push-state ((asm-reader asm-reader))
  (push (slot-value asm-reader 'current-state) (slot-value asm-reader 'state-stack)))

(defmethod push-current-and-set-state (state (asm-reader asm-reader))
  (push (slot-value asm-reader 'current-state) (slot-value asm-reader 'state-stack))
  (setf (slot-value asm-reader 'current-state) state))

(defmethod push-state1 (state (asm-reader asm-reader))
  (push state (slot-value asm-reader 'state-stack)))

(defmethod pop-state ((asm-reader asm-reader))
  (setf (slot-value asm-reader 'current-state) (pop (slot-value asm-reader 'state-stack))))
