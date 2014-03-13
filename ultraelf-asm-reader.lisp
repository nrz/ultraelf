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
                    ;; if true, do not output anything.
                    ((not is-there-code-on-this-line)
                     (setf current-phase "beginning-of-line"))
                    ;; are we inside instruction or inside a parameter?
                    ;; if true, output ")
                    ((or (equal current-phase "inside-instruction")
                         (equal current-phase "inside-parameters"))

                     (progn
                       (setf current-phase "beginning-of-line")
                       (setf is-there-code-on-this-line nil)
                       (setf my-string (concatenate 'string my-string "\")"))))
                    ;; otherwise output )
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
               ;; are we inside memory address syntax? 
               ;; if yes, don't output anything.
               ((equal current-phase "inside-memory-address-syntax")
                (cond
                  ;; is this a space inside memory address syntax?
                  ;; if yes, don't output anything.
                  ((equal my-char " ")
                   nil)
                  ;; is this closing square bracket?
                  ;; if yes, output ]
                  ((equal my-char "]")
                   (progn
                     (setf current-phase "closing-square-bracket")
                     (setf my-string (concatenate 'string my-string my-char))))
                  ;; otherwise output the character.
                  (t (setf my-string (concatenate 'string my-string my-char)))))
               ;; is character space or comma?
               ((or (equal my-char " ")
                    (equal my-char ","))
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
               ((and
                  (not (equal current-phase "inside-instruction"))
                  (or (equal (get-last-character-string my-string) " ")
                      (equal (get-last-character-string my-string) ",")))
                (cond
                  ;; is this memory address syntax (with square brackets)?
                  ;; if yes, mark we're inside memory address syntax, output " and current character.
                  ((equal my-char "[")
                   (progn
                     (setf current-phase "inside-memory-address-syntax")
                     (setf my-string (concatenate 'string my-string "\"" my-char))))
                  ;; this is not a memory address syntax.
                  ;; mark we're inside parameters, output " and current character.
                  (t (progn
                       (setf current-phase "inside-parameters")
                       (setf my-string (concatenate 'string my-string "\"" my-char))))))
               ;; otherwise output the character.
               (t (setf my-string (concatenate 'string my-string my-char)))))))

;;; #a is the input which starts the custom reader.
(set-dispatch-macro-character #\# #\a #'transform-code-to-string)
