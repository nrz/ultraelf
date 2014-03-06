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
        ((invalid-last-characters (list "'" " " "(" ")"))
         (is-there-code-on-this-line nil)
         (current-phase "beginning-of-line")
         (my-string "(list "))
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
                   ;; is character newline?
                   ((equal my-char (coerce (list #\Newline) 'string))
                    (progn
                      (cond
                        ;; is there _no_ code on this line?
                        ;; if true, do not print anything.
                        ((not is-there-code-on-this-line)
                         (setf current-phase "beginning-of-line"))
                        ;; are we inside instruction or inside a parameter?
                        ;; if true, print " )
                        ((or (equal current-phase "inside-instruction") (equal current-phase "inside-parameters"))

                         (progn
                           (setf current-phase "beginning-of-line")
                           (setf is-there-code-on-this-line nil)
                           (setf my-string (concatenate 'string my-string "\")"))))
                        ;; otherwise print )
                        (t (progn
                             (setf current-phase "beginning-of-line")
                             (setf is-there-code-on-this-line nil)
                             (setf my-string (concatenate 'string my-string ")")))))))
                   ;; are we inside a comment?
                   ;; if yes, don't output anything.
                   ((equal current-phase "inside-comment")
                    nil)
                   ;; are we in the beginning of the line?
                   ((equal current-phase "beginning-of-line")
                    (cond
                      ;; is this a space in the beginning of the line?
                      ;; if yes, do not output anything.
                      ((equal my-char " ")
                       nil)
                      ;; is this the first character of instruction and not ( or ) ?
                      ;; if yes, mark there is code on this line, mark first character as printed, output " and current character.
                      ((and
                         (not (equal my-char "("))
                         (not (equal my-char ")")))
                       (progn
                         (setf current-phase "inside-instruction")
                         (setf is-there-code-on-this-line t)
                         (setf my-string (concatenate 'string my-string "'(\"" my-char))))
                      (t nil)))
                   ;; is character ; ?
                   ;; if yes, don't output anything, begin comment.
                   ((equal my-char ";")
                    (setf current-phase "inside-comment"))
                   ;; is character space?
                   ((or (equal my-char " ") (equal my-char ","))
                    (cond
                      ;; is character space or comma, and last character was _not_ space, comma or opening parenthesis?
                      ;; if yes, output " and space.
                      ((and
                         (not (equal (get-last-character-string my-string) " "))
                         (not (equal (get-last-character-string my-string) ","))
                         (not (equal (get-last-character-string my-string) "(")))
                       (progn
                         (setf current-phase "in-space")
                         (setf my-string (concatenate 'string my-string "\" "))))
                      (t nil)))
                   ;; is instruction printed and this is the 1st character of a parameter?
                   ;; if yes, output " and current character.
                   ((and
                      (not (equal current-phase "inside-instruction"))
                      (or (equal (get-last-character-string my-string) " ")
                          (equal (get-last-character-string my-string) ",")))
                    (progn
                      (setf current-phase "inside-parameters")
                      (setf my-string (concatenate 'string my-string "\"" my-char))))
                   ;; otherwise output the character.
                   (t (setf my-string (concatenate 'string my-string my-char))))))))
    ;;; #a is the input which starts the custom reader.
    (set-dispatch-macro-character #\# #\a #'transform-code-to-string))

(defun create-syntax-tree (my-string)
  "This function converts a string produced by transform-code-to-string into a syntax tree."
  (read-from-string my-string))

(defun assemble (syntax-tree)
  (mapcar #'funcall (mapcar #'(lambda (x) (string-to-function (first x))) (eval syntax-tree))))

(defun assemble-and-print-hex (syntax-tree)
  (print-hex-list (mapcar #'funcall (mapcar #'(lambda (x) (string-to-function (first x))) (eval syntax-tree)))))

(defun print-hex (my-number)
  (format nil "~x" my-number))

(defun print-hex-list (my-list)
  (mapcar #'(lambda (x) (print-hex x)) my-list))

(defun string-to-function (my-string)
  "This fnuction converts a string to a function.
   http://stackoverflow.com/questions/2940267/call-function-based-on-a-string/2940347#2940347"
  (symbol-function (find-symbol (string-upcase my-string))))

(defun in (arg1 arg2)
  (cond
    ((and (equalp arg1 "al") (equalp arg2 "dx"))
     #xec)
    ((and (equalp arg1 "ax") (equalp arg2 "dx"))
     #x66 #xed)
    ((and (equalp arg1 "eax") (equalp arg2 "dx"))
     #xed)
    (t nil)))

(defun out (arg1 arg2)
  (cond
    ((and (equalp arg1 "dx") (equalp arg2 "al"))
     #xee)
    ((and (equalp arg1 "dx") (equalp arg2 "ax"))
     #x66 #xef)
    ((and (equalp arg1 "dx") (equalp arg2 "eax"))
     #xef)
    (t nil)))

(defun nop ()
  #x90)
(defun hlt ()
  #xf4)
(defun cmc ()
  #xf5)
(defun clc ()
  #xf8)
(defun stc ()
  #xf9)
(defun cli ()
  #xfa)
(defun sti ()
  #xfb)
(defun cld ()
  #xfc)
(defun std ()
  #xfd)

(defparameter *example-code*
  #a
  mul  rax     ; rdx:rax = rax^2.
  (
   mov  rbp,rsp ; create the stack frame
   lea  rdi,[testmsg1] ; load effective address.
   )
  #)

(defparameter *alt-mov-reg64-reg64-push-pop*
  #a
  push arg2
  pop arg1
  #)

(defparameter *alt-mov-reg64-reg64-lea*
  #a
  lea arg1,[arg2]
  #)
