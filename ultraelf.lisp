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
         (is-instruction-printed nil)
         (is-inside-comment nil)
         (my-string "(list `("))
        ;; loop through stream.
        (loop for my-char = (coerce (list (read-char stream t nil t)) 'string)
              do (cond
                   ;; is character # ?
                   ;; if yes, we're done, fix closing parentheses and return. 
                   ((equal my-char "#")
                    (return-from transform-code-to-string
                                 (concatenate 'string (get-string-without-invalid-last-character
                                                        (get-string-without-invalid-last-character
                                                          my-string invalid-last-characters)
                                                        invalid-last-characters) "))")))
                   ;; is character newline and last character was not opening parenthesis?
                   ;; if yes, output ")`(", end comment.
                   ((and (equal my-char (coerce (list #\Newline) 'string)) (not (equal (get-last-character-string my-string) "(")))
                    (progn
                      (setf my-string (concatenate 'string my-string ")`("))
                      (setf is-instruction-printed nil) 
                      (setf is-inside-comment nil)))
                   ;; is character newline (and last character was opening parenthesis)?
                   ;; if yes, don't output anything, end comment.
                   ((equal my-char (coerce (list #\Newline) 'string))
                    (progn
                      (setf is-instruction-printed nil) 
                      (setf is-inside-comment nil)))
                   ;; are we inside a comment?
                   ;; if yes, don't output anything.
                   (is-inside-comment nil)
                   ;; is character ; ?
                   ;; if yes, don't output anything, begin comment.
                   ((equal my-char ";")
                    (setf is-inside-comment t))
                   ;; is character space?
                   ((equal my-char " ")
                    (cond
                      ;; is character space, and instruction is printed, and last character was _not_ space or opening parenthesis?
                      ;; if yes, output " and space.
                      ((and is-instruction-printed (not (equal (get-last-character-string my-string) " ")) (not (equal (get-last-character-string my-string) "(")))
                       (setf my-string (concatenate 'string my-string "\" ")))
                      ;; is character space, and instruction is not printed, and last character was _not_ space or opening parenthesis?
                      ;; if yes, mark instruction as printed, output space.
                      ((and (not (equal (get-last-character-string my-string) " ")) (not (equal (get-last-character-string my-string) "(")))
                       (progn
                         (setf is-instruction-printed t)
                         (setf my-string (concatenate 'string my-string " "))))
                      (t nil)))
                   ;; is character comma?
                   ;; if yes, output `" "`.
                   ((equal my-char ",")
                    (setf my-string (concatenate 'string my-string "\" \"")))
                   ;; is instruction printed and this is the 1st character of 1st argument?
                   ;; if yes, output " and current character.
                   ((and is-instruction-printed (equal (get-last-character-string my-string) " "))
                    (setf my-string (concatenate 'string my-string "\"" my-char))) 
                   ;; otherwise output the character.
                   (t (setf my-string (concatenate 'string my-string my-char)))))))
      ;;; #a is the input which starts the custom reader.
      (set-dispatch-macro-character #\# #\a #'transform-code-to-string)))

(defun create-syntax-tree (my-string)
  "This function converts a string produced by transform-code-to-string into a syntax tree."
  (read-from-string my-string))

(defparameter *example-code*
  #a
  mul  rax     ; rdx:rax = rax^2.
  (
   mov  rbp,rsp ; create the stack frame
   lea  rdi,[testmsg1] ; load effective address.
   )
  #)

(defun lea (arg1 arg2)
  #x80)
(defun mul (arg1)
  #x81)
(defun mov (arg1 arg2)
  #x82)
