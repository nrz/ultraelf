;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun new-instruction (is-there-code-on-this-line current-phase my-string)
  (cond
    ;; is there _no_ code on this line?
    ;; if true, do not output anything.
    ((not is-there-code-on-this-line)
     (setf current-phase "beginning-of-line"))
    ;; are we inside instruction or inside a parameter?
    ;; if true, output ")
    ((or (equal current-phase "inside-instruction")
         (equal current-phase "inside-parameters"))
     (setf current-phase "beginning-of-line")
     (setf is-there-code-on-this-line nil)
     (setf my-string (concatenate 'string my-string "\")")))
    ;; otherwise output )
    (t (setf current-phase "beginning-of-line")
     (setf is-there-code-on-this-line nil)
     (setf my-string (concatenate 'string my-string ")"))))
  (values is-there-code-on-this-line current-phase my-string))

(defun transform-code-to-string (stream sub-char numarg)
  "This function converts assembly code into a string.
   #l marks change to Lisp code. #a marks return to asm. #e marks end.
   Partially based on: http://weitz.de/macros.lisp"
  (declare (ignore sub-char numarg))
  (let*
    ((invalid-last-characters (list "'" " " "(" ")"))
     (current-mode "asm")
     (is-there-code-on-this-line nil)
     (current-phase "beginning-of-line")
     (is-hash-sign-read nil)
     (my-string "(list ")
     (lisp-code-string "")
     (n-lisp-forms 0))
    ;; loop through stream.
    (loop for my-char = (coerce (list (read-char stream t nil t)) 'string)
          do (cond
               ((equal current-mode "asm")
                (cond
                  (is-hash-sign-read
                    (progn
                      (setf is-hash-sign-read nil)
                      ;; is character e ?
                      ;; if yes, we're done, fix closing parentheses and return.
                      (cond
                        ((equal my-char "e")
                         (return-from transform-code-to-string
                                      (concatenate 'string (get-string-without-invalid-last-character
                                                             (get-string-without-invalid-last-character
                                                               my-string invalid-last-characters)
                                                             invalid-last-characters) "))")))
                        ;; is character a ?
                        ;; if yes, do exactly the same is if it was newline.
                        ((equal my-char "a")
                         (setf (values is-there-code-on-this-line current-phase my-string)
                               (new-instruction is-there-code-on-this-line current-phase my-string)))
                        ;; is character l ?
                        ;; if yes, change to Lisp mode.
                        ((equal my-char "l")
                         (setf current-mode "Lisp")
                         (setf is-there-code-on-this-line nil)
                         (setf lisp-code-string "")
                         (setf current-phase "beginning-of-line"))
                        ;; otherwise, print error.
                        (t (error "in asm mode undefined control character after #")))))
                  ;; is character # ?
                  ;; if yes, mark hash sign read.
                  ((equal my-char "#")
                   (setf is-hash-sign-read t))
                  ;; is character newline?
                  ((equal my-char (coerce (list #\Newline) 'string))
                   (setf (values is-there-code-on-this-line current-phase my-string)
                         (new-instruction is-there-code-on-this-line current-phase my-string)))
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
                      (setf current-phase "inside-instruction")
                      (setf is-there-code-on-this-line t)
                      (setf my-string (concatenate 'string my-string "'(\"" my-char)))
                     (t nil)))
                  ;; is character ; ?
                  ;; if yes, don't output anything, begin comment.
                  ((equal my-char ";")
                   (setf current-phase "inside-comment"))
                  ;; are we inside a Lisp form?
                  ((equal current-phase "inside-lisp-form")
                   (cond
                     ;; is this opening parenthesis?
                     ((equal my-char "(")
                      ;; if yes, increment parenthesis count and output (
                      (incf n-lisp-forms)
                      (setf my-string (concatenate 'string my-string my-char)))
                     ;; is this closing parenthesis?
                     ;; if yes, output )
                     ((equal my-char ")")
                      (if (eql (decf n-lisp-forms) 0)
                        (setf current-phase "closing-parenthesis"))
                      (setf my-string (concatenate 'string my-string my-char)))
                     ;; is character newline?
                     ((equal my-char (coerce (list #\Newline) 'string))
                      (unless (equal (get-last-character-string my-string) " ")
                        ;; if last character was not space, output space.
                        (setf my-string (concatenate 'string my-string " "))))
                     ;; otherwise output the character.
                     (t (setf my-string (concatenate 'string my-string my-char)))))
                  ((equal current-phase "inside-memory-address-syntax")
                   (cond
                     ;; is this a space inside memory address syntax?
                     ;; if yes, don't output anything.
                     ((equal my-char " ")
                      nil)
                     ;; is this closing square bracket?
                     ;; if yes, output ]
                     ((equal my-char "]")
                      (setf current-phase "closing-square-bracket")
                      (setf my-string (concatenate 'string my-string my-char)))
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
                      (setf current-phase "in-space")
                      (setf my-string (concatenate 'string my-string "\" ")))
                     (t nil)))
                  ;; is instruction printed and this is the 1st character of a parameter?
                  ((and
                     (not (equal current-phase "inside-instruction"))
                     (or (equal (get-last-character-string my-string) " ")
                         (equal (get-last-character-string my-string) ",")))
                   (cond
                     ((equal my-char "(")
                      ;; is this a Lisp form (with parentesis)?
                      ;; if yes, mark we're inside Lisp form, output " and current character.
                      (setf current-phase "inside-lisp-form")
                      (setf n-lisp-forms 1)
                      (setf my-string (concatenate 'string my-string "\"" my-char)))
                     ;; is this memory address syntax (with square brackets)?
                     ;; if yes, mark we're inside memory address syntax, output " and current character.
                     ((equal my-char "[")
                      (setf current-phase "inside-memory-address-syntax")
                      (setf my-string (concatenate 'string my-string "\"" my-char)))
                     ;; this is not a memory address syntax.
                     ;; mark we're inside parameters, output " and current character.
                     (t
                      (setf current-phase "inside-parameters")
                      (setf my-string (concatenate 'string my-string "\"" my-char)))))
                  ;; otherwise output the character.
                  (t (setf my-string (concatenate 'string my-string my-char)))))
               ((equal current-mode "Lisp")
                ;; in Lisp mode, read text until #e or #a is reached and eval it.
                (cond
                  (is-hash-sign-read
                    (progn
                      (setf is-hash-sign-read nil)
                      (cond
                        ;; is character e ?
                        ;; if yes, we're done, fix closing parentheses and return.
                        ((equal my-char "e")
                         (setf my-string (concatenate 'string
                                                      my-string
                                                      (coerce (list #\Newline) 'string)
                                                      "#a"
                                                      (coerce (list #\Newline) 'string)
                                                      (eval (read-from-string lisp-code-string))
                                                      (coerce (list #\Newline) 'string)
                                                      "#e"
                                                      (coerce (list #\Newline) 'string)
                                                      ")"))
                         (return-from transform-code-to-string
                                      (concatenate 'string (get-string-without-invalid-last-character
                                                             (get-string-without-invalid-last-character
                                                               my-string invalid-last-characters)
                                                             invalid-last-characters) ")")))
                        ;; is character a ?
                        ;; if yes, change to asm mode.
                        ((equal my-char "a")
                         (setf current-mode "asm")
                         (setf is-there-code-on-this-line nil)
                         (setf current-phase "beginning-of-line")
                         (setf my-string (concatenate 'string
                                                      my-string
                                                      (coerce (list #\Newline) 'string)
                                                      "#a"
                                                      (coerce (list #\Newline) 'string)
                                                      (eval (read-from-string lisp-code-string))
                                                      (coerce (list #\Newline) 'string)
                                                      "#e"
                                                      (coerce (list #\Newline) 'string))))
                        ;; otherwise, add # and the character to the Lisp code to be evaluated.
                        (t
                         (setf current-phase "")
                         (setf lisp-code-string (concatenate 'string lisp-code-string "#" my-char))))))
                  ;; is character # ?
                  ;; if yes, mark hash sign read.
                  ((equal my-char "#")
                   (setf is-hash-sign-read t))
                  ;; otherwise add the character to the Lisp code to be evaluated.
                  (t (setf lisp-code-string (concatenate 'string lisp-code-string my-char)))))
               (t
                 (format t "current mode: ~a~a" current-mode #\Newline)
                 (error "invalid current mode"))))))

;;; #a is the input which starts the custom reader.
(set-dispatch-macro-character #\# #\a #'transform-code-to-string)
