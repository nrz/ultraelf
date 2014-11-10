;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun get-last-character-string (my-string)
  "This function returns a string consisting of the last character of the input string."
  (subseq my-string (1- (length my-string))))

(defun get-string-without-last-character (my-string)
  "This function returns a string without the last character of the input string."
  (subseq my-string 0 (1- (length my-string))))

(defun get-string-without-invalid-last-character (my-string invalid-last-characters)
  "If the last character of the string is invalid, the string is returned without it, otherwise completely."
  (loop for invalid-last-character in invalid-last-characters
        do (if (equal (get-last-character-string my-string) invalid-last-character)
             (setf my-string (get-string-without-last-character my-string))))
  my-string)

(defun split-string-into-list-of-strings (my-string)
  "This function splits a string to a list of strings of 1 character each."
  (loop for i from 0 to (1- (length my-string))
        collect (subseq my-string i (1+ i))))

(defun parse-number (my-string)
  (handler-case
    (progn (parse-number:parse-number my-string))
    (parse-number:invalid-number ()
                                 nil)))
