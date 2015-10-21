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
   This function is usually not called directly.
   `create-syntax-tree` can be used to test the reader, eg. `(create-syntax-tree #a mov ax,bx #e)`.
   This function is a finite state machine (excluding input and output).
   Usually the execution of this function is triggered by the dispatch macro character: `#a`.
   Current mode is stored in the variable `current-mode` as a string. In the beginning, `current-mode` is `\"asm\"`.
   Current phase is stored in the variable `current-phase` as a string. In the beginning, `current-phase` is `\"beginning-of-line\"`.
   NOTE: commas are considered as whitespace!

   Then, inside this function the following states and transitions are possible:

   inside-comment
   description of state: inside comment text.
   inside-comment -> a newline -> beginning-of-line

   beginning-of-line
   description of state: no non-whitespace printed on this line so far.
   beginning-of-line -> # -> hash-sign-read
   beginning-of-line -> ( -> inside-lisp-form (set `n-lisp-forms` to 1).
   beginning-of-line -> ) -> error (cannot terminate Lisp form outside Lisp form).
   beginning-of-line -> [ -> error (cannot begin memory address syntax before instruction).
   beginning-of-line -> ] -> error (cannot terminate memory address syntax before instruction).
   beginning-of-line -> ; -> inside-comment
   beginning-of-line -> a newline -> beginning-of-line
   beginning-of-line -> a whitespace character -> beginning-of-line (do not output anything).
   beginning-of-line -> a non-whitespace character -> inside-instruction

   hash-sign-read
   description of state: the previous character was a hash sign.
   hash-sign-read -> a -> beginning-of-line
   hash-sign-read -> e -> end of syntax
   hash-sign-read -> l -> switch to Lisp mode
   hash-sign-read -> any other character -> error (hash sign must be followed with `a`, `e`, or `l`).

   inside-lisp-form
   description: inside a Lisp form that will be evaluated during assembling phase.
   inside-lisp-form -> ( -> inside-lisp-form (increment `n-lisp-forms` to 1).
   inside-lisp-form -> ) -> decrement `n-lisp-forms` by 1. if `n-lisp-forms` becomes zero, then -> closing-parenthesis.
   inside-lisp-form -> a newline -> space-inside-lisp-form
   inside-lisp-form -> a whitespace character -> -> space-inside-lisp-form
   inside-lisp-form -> any other character -> inside-lisp-form

   space-inside-lisp-form
   description: a space inside a Lisp form that will be evaluated during assembling phase.
   space-inside-lisp-form -> ( -> inside-lisp-form (increment `n-lisp-forms` to 1).
   space-inside-lisp-form -> ) -> decrement `n-lisp-forms` by 1. if `n-lisp-forms` becomes zero, then -> closing-parenthesis.
   space-inside-lisp-form -> a newline -> space-inside-lisp-form (do not output anything).
   space-inside-lisp-form -> a whitespace character -> -> space-inside-lisp-form (do not output anything).
   space-inside-lisp-form -> any other character -> inside-lisp-form (output space).

   inside-instruction
   description of state: inside first no-whitespace character block of this line.
   inside-instruction -> # -> hash-sign-read
   inside-instruction -> [ -> error (cannot begin memory address syntax inside instruction).
   inside-instruction -> ] -> error (cannot terminate memory address syntax inside instruction).
   inside-instruction -> ; -> inside-comment
   inside-instruction -> a newline -> beginning-of-line
   inside-instruction -> a whitespace character -> in-space

   in-space
   description of state: inside whitespace characters between instruction and parameters (in case there is one or more parameters).
   in-space -> # -> hash-sign-read
   in-space -> ( -> inside-lisp-form (set `n-lisp-forms` to 1).
   in-space -> ) -> error (cannot terminate Lisp form outside Lisp form).
   in-space -> [ -> inside-memory-address-syntax
   in-space -> ] -> error (cannot terminate memory address syntax before any parameters).
   in-space -> ; -> inside-comment
   in-space -> a newline -> beginning-of-line
   in-space -> a whitespace character -> in-space (do not output anything).
   in-space -> a non-whitespace character -> inside-parameters

   inside-memory-address-syntax
   description of state: inside memory address syntax limited by square brackets. The content between square brackets will be parsed as a parameter.
   inside-memory-address-syntax -> # -> error (memory address syntax must be terminated with a closing square bracket before a hash sign).
   inside-memory-address-syntax -> ( -> inside-lisp-form (set `n-lisp-forms` to 1).
   inside-memory-address-syntax -> ) -> error (cannot terminate Lisp form outside Lisp form).
   inside-memory-address-syntax -> [ -> error (cannot begin a new memory address syntax inside memory address syntax).
   inside-memory-address-syntax -> ] -> closing-square-bracket (the content of `memory-address-syntax-buffer` will be converted to intermediate representation).
   inside-memory-address-syntax -> ; -> error (memory address syntax must be terminated with a closing square bracket before comment).
   inside-memory-address-syntax -> a newline -> error (memory address syntax must be terminated with a closing square bracket before newline).
   inside-memory-address-syntax -> a whitespace character -> space-inside-memory-address-syntax
   inside-memory-address-syntax -> a non-whitespace character -> inside-memory-address-syntax

   space-inside-memory-address-syntax
   description: space inside memory address syntax.
   space-inside-memory-address-syntax -> # -> error (memory address syntax must be terminated with a closing square bracket before a hash sign).
   space-inside-memory-address-syntax -> ( -> inside-lisp-form (set `n-lisp-forms` to 1).
   space-inside-memory-address-syntax -> ) -> error (cannot terminate Lisp form outside Lisp form).
   space-inside-memory-address-syntax -> [ -> error (cannot begin a new memory address syntax inside memory address syntax).
   space-inside-memory-address-syntax -> ] -> closing-square-bracket (the content of `memory-address-syntax-buffer` will be converted to intermediate representation).
   space-inside-memory-address-syntax -> ; -> error (memory address syntax must be terminated with a closing square bracket before comment).
   space-inside-memory-address-syntax -> a newline -> error (memory address syntax must be terminated with a closing square bracket before newline).
   space-inside-memory-address-syntax -> a whitespace character -> space-inside-memory-address-syntax
   space-inside-memory-address-syntax -> a non-whitespace character -> space-inside-memory-address-syntax (output space).

   closing-square-bracket
   description of state: the last character was a closing square bracket that closed memory address syntax.
   closing-square-bracket -> # -> error (a whitespace is required between closing square bracket and hash sign).
   closing-square-bracket -> [ -> error (a whitespace is required between closing square bracket and opening square bracket).
   closing-square-bracket -> ] -> error (cannot the same terminate memory address syntax twice).
   closing-square-bracket -> ; -> inside-comment
   closing-square-bracket -> a newline -> beginning-of-line
   closing-square-bracket -> a whitespace character -> in-space
   closing-square-bracket -> a non-whitespace character -> error (a whitespace is required after closing square bracket).

   inside-parameters
   description of state: inside parameter.
   inside-parameters -> # -> error (a whitespace is required between a parameter and hash sign).
   inside-parameters -> [ -> error (cannot begin memory address syntax inside a parameter).
   inside-parameters -> ] -> error (cannot terminate memory address syntax inside a parameter).
   inside-parameters -> ; -> inside-comment
   inside-parameters -> a newline -> beginning-of-line
   inside-parameters -> a whitespace character -> in-space
   inside-parameters -> a non-whitespace character -> inside-parameters

   #l marks change to Lisp mode (Common Lisp macros are executed during assembling of the code).
   #a marks return to asm, or a new instruction if we are on asm mode.
   #e marks end of syntax.
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
                      (t (error "in asm mode undefined control character after #"))))
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
                      (when (eql (decf n-lisp-forms) 0)
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
                       (setf lisp-code-string (concatenate 'string lisp-code-string "#" my-char)))))
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
