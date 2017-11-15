;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defparameter *test-code-with-lisp-number-1*
  #a
  #l
  (concatenate 'string "foo1 bar1")
  #a
  foo2 bar2
  foo3 bar3
  #e)

(defparameter *test-code-with-lisp-number-2*
  #a
  foo1 bar1    ; foo register bar1.
  #l
  (concatenate 'string "foo2 bar2")
  #a
  foo3 bar3
  foo4 bar4
  #e)

(defparameter *test-code-with-lisp-number-3*
  #a
  foo1 bar1     ; foo register bar1.
  foo2 bar2     ; foo register bar2.
  #l
  (concatenate 'string "foo3 bar3")
  #a
  foo4 bar4
  foo5 bar5
  #e)
