;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defparameter *string-instructions-8-bit* (list (list  "insb" #x6c)
                                                (list "outsb" #x6e)
                                                (list "movsb" #xa4)
                                                (list "cmpsb" #xa6)
                                                (list "stosb" #xaa)
                                                (list "lodsb" #xac)
                                                (list "scasb" #xae)))

(defparameter *string-instructions-16-bit* (list (list  "insw" #x6c)
                                                 (list "outsw" #x6e)
                                                 (list "movsw" #xa4)
                                                 (list "cmpsw" #xa6)
                                                 (list "stosw" #xaa)
                                                 (list "lodsw" #xac)
                                                 (list "scasw" #xae)))

(defparameter *string-instructions-32-bit* (list (list  "insd"  #x6d)
                                                 (list "outsd" #x6f)
                                                 (list "movsd" #xa5)
                                                 (list "cmpsd" #xa7)
                                                 (list "stosd" #xab)
                                                 (list "lodsd" #xad)
                                                 (list "scasd" #xaf)))

(defparameter *string-instructions-64-bit* (list (list "movsq" #xa5)
                                                 (list "cmpsq" #xa7)
                                                 (list "stosq" #xab)
                                                 (list "lodsq" #xad)
                                                 (list "scasq" #xaf)))

(defparameter *create-argument-instances-list*
  (list
    (list *string-instructions-8-bit*  'x86-8-bit-string-instruction)
    (list *string-instructions-16-bit* 'x86-16-bit-string-instruction)
    (list *string-instructions-32-bit* 'x86-32-bit-string-instruction)
    (list *string-instructions-64-bit* 'x86-64-bit-string-instruction)))
