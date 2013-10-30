;;;; ultraELF
;;;;
;;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
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
    (defun transform-code-to-string (stream sub-char numarg)
      "This function converts assembly code into a string. # marks end.
       Partially based on: http://weitz.de/macros.lisp"
      (declare (ignore sub-char numarg))
      (let*
        ((invalid-last-characters (list "`" " " "(" ")"))
         (my-string "(list `("))
        ;; loop through stream.
        (loop for my-char = (coerce (list (read-char stream t nil t)) 'string)
              ;; is character # ?
              do (if (equal my-char "#")
                   ;; done, return, add closing parenthesis to the end.
                   (return-from transform-code-to-string
                                (concatenate 'string (get-string-without-invalid-last-character
                                                       (get-string-without-invalid-last-character my-string invalid-last-characters) invalid-last-characters) "))"))
                   ;; is character space, and last character was space or opening parenthesis?
                   (if (and (equal my-char " ") (or (equal (get-last-character-string my-string) " ") (equal (get-last-character-string my-string) "(")))
                     ;; if yes, don't output anything.
                     nil
                     ;; is character comma?
                     (if (equal my-char ",") 
                       ;; if yes, output space.
                       (setf my-string (concatenate 'string my-string " "))
                       ;; is character newline and last character was not opening parenthesis?
                       (if (and (equal my-char (coerce (list #\Newline) 'string)) (not (equal (get-last-character-string my-string) "(")))
                         ;; if yes, output ")("
                         (setf my-string (concatenate 'string my-string ")`("))
                         ;; is character newline (and last character was opening parenthesis)?
                         (if (equal my-char (coerce (list #\Newline) 'string))
                           ;; if yes, don't output anything.
                           nil
                           ;; otherwise output the character.
                           (setf my-string (concatenate 'string my-string my-char))))))))))
      ;;; #a is the input which starts the custom reader.
      (set-dispatch-macro-character #\# #\a #'transform-code-to-string)))

(defun create-syntax-tree (my-string)
  "This function converts a string produced by transform-code-to-string into a syntax tree."
  (read-from-string my-string))

(defparameter *example-code*
  #a
  push rbp
  mov  rbp,rsp
  lea  rdi,[testmsg1]
  #)
