;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun new-instruction (asm-reader)
  (cond
    ;; is there _no_ code on this line?
    ;; if true, do not output anything.
    ((not (is-there-code-on-this-line asm-reader))
     nil)
    ;; are we inside instruction or inside a parameter?
    ;; if true, output ")
    ((or (equal (current-state asm-reader) "inside-instruction")
         (equal (current-state asm-reader) "inside-parameters"))
     (setf (ast-string asm-reader) (concatenate 'string (ast-string asm-reader) "\")")))
    ;; otherwise output )
    (t (setf (ast-string asm-reader) (concatenate 'string (ast-string asm-reader) ")"))))
  (setf (is-there-code-on-this-line asm-reader) nil)
  (setf (current-state asm-reader) "start-of-line")
  (setf (state-stack asm-reader) nil)
  asm-reader)

(defun asm-start-of-line (asm-reader my-char)
  (cond
    ;; is character # ?
    ;; if yes, mark hash sign read, do not output anything.
    ((equal my-char "#")
     (push-state asm-reader)
     (setf (current-state asm-reader) "hash-sign-read"))
    ((equal my-char "(")
     ;; is this a Lisp form (with parentesis)?
     ;; if yes, mark we are inside Lisp form, mark that there is code on this line, output "(
     (push-state1 "in-space" asm-reader)
     (setf (current-state asm-reader) "inside-lisp-form")
     (setf (n-lisp-forms asm-reader) 1)
     (setf (is-there-code-on-this-line asm-reader) t)
     (setf (ast-string asm-reader) (concatenate 'string (ast-string asm-reader) "\"(")))
    ((equal my-char ")")
     (error "cannot terminate Lisp form outside a Lisp form"))
    ((equal my-char "[")
     (push-state asm-reader)
     (setf (current-state asm-reader) "opening-square-bracket")
     (setf (is-there-code-on-this-line asm-reader) t)
     (setf (ast-string asm-reader) (concatenate 'string (ast-string asm-reader) "'(\"[")))
    ((equal my-char "]")
     (error "cannot terminate memory address syntax before instruction"))
    ;; is character ; ?
    ;; if yes, don't output anything, begin comment.
    ((equal my-char ";")
     (setf (current-state asm-reader) "inside-comment"))
    ;; is character / ?
    ;; if yes, mark we have a slash.
    ((equal my-char "/")
     (push-state asm-reader)
     (setf (current-state asm-reader) "slash"))
    ;; is character newline?
    ;; if yes, start a new instruction.
    ((equal my-char (coerce (list #\Newline) 'string))
     (setf asm-reader (new-instruction asm-reader)))
    ;; is character backslash?
    ;; if yes, mark we have a backslash in start of line.
    ((equal my-char "\\")
     (push-state asm-reader)
     (setf (current-state asm-reader) "backslash-in-start-of-line"))
    ;; is character space?
    ;; if yes, do not output anything.
    ((equal my-char " ")
     nil)
    ;; otherwise mark we are inside an instruction, mark that there is code on this line, output " and current character.
    (t (setf (current-state asm-reader) "inside-instruction")
     (setf (is-there-code-on-this-line asm-reader) t)
     (setf (ast-string asm-reader) (concatenate 'string (ast-string asm-reader) "'(\"" my-char))))
  (values (is-there-code-on-this-line asm-reader) (current-state asm-reader) (state-stack asm-reader) (ast-string asm-reader)))

(defun transform-code-to-string (stream current-mode)
  "This function converts assembly code into a string.
   This function is usually not called directly.
   `create-syntax-tree` can be used to test the reader, eg. `(create-syntax-tree #a mov ax,bx #e)`.
   This function is a finite state machine (excluding input and output).
   Usually the execution of this function is triggered by the dispatch macro character: `#a`.
   Current mode is stored in the variable `current-mode` as a string. In the beginning, `current-mode` is `\"asm\"`.
   Current phase is stored in the variable `current-state` as a string. In the beginning, `current-state` is `\"start-of-line\"`.
   NOTE: commas are considered as whitespace!

   Then, inside this function the following states and transitions are possible:

   start-of-line
   description of state: no non-whitespace printed on this line so far.
   start-of-line -> # -> hash-sign-read
   start-of-line -> ( -> inside-lisp-form (set `n-lisp-forms` to 1).
   start-of-line -> ) -> error (cannot terminate Lisp form outside a Lisp form).
   start-of-line -> [ -> opening-square-bracket (this is needed for `[bits 64]` etc.).
   start-of-line -> ] -> error (cannot terminate memory address syntax before instruction).
   start-of-line -> ; -> inside-comment
   start-of-line -> / -> slash
   start-of-line -> \ -> backslash-in-start-of-line
   start-of-line -> a newline -> start-of-line (do not output anything).
   start-of-line -> a space -> start-of-line (do not output anything).
   start-of-line -> any character -> inside-instruction

   backslash-in-start-of-line
   description of state: the line begins with a backslash (possibly after whitespace). I wonder if there is need for such lines.
   backslash-in-start-of-line -> TODO!

   hash-sign-read
   description of state: the previous character was a hash sign.
   hash-sign-read -> a -> start-of-line
   hash-sign-read -> e -> end of syntax
   hash-sign-read -> l -> switch to Lisp mode
   hash-sign-read -> any other character -> error (hash sign must be followed with `a`, `e`, or `l`).

   inside-lisp-form
   description: inside a Lisp form that will be evaluated during assembling phase.
   inside-lisp-form -> ( -> inside-lisp-form (increment `n-lisp-forms` to 1).
   inside-lisp-form -> ) -> decrement `n-lisp-forms` by 1. if `n-lisp-forms` becomes zero, then -> closing-parenthesis.
   inside-lisp-form -> a newline -> space-inside-lisp-form
   inside-lisp-form -> a space -> -> space-inside-lisp-form
   inside-lisp-form -> any other character -> inside-lisp-form

   space-inside-lisp-form
   description: a space inside a Lisp form that will be evaluated during assembling phase.
   space-inside-lisp-form -> ( -> inside-lisp-form (increment `n-lisp-forms` to 1).
   space-inside-lisp-form -> ) -> decrement `n-lisp-forms` by 1. if `n-lisp-forms` becomes zero, then -> closing-parenthesis.
   space-inside-lisp-form -> a newline -> space-inside-lisp-form (do not output anything).
   space-inside-lisp-form -> a space -> -> space-inside-lisp-form (do not output anything).
   space-inside-lisp-form -> any other character -> inside-lisp-form.

   inside-instruction
   description of state: inside first no-whitespace character block of this line.
   inside-instruction -> # -> error (a whitespace is required between instruction and hash sign).
   inside-instruction -> [ -> error (cannot begin memory address syntax inside instruction).
   inside-instruction -> ] -> error (cannot terminate memory address syntax inside instruction).
   inside-instruction -> ; -> inside-comment
   inside-instruction -> / -> slash
   inside-instruction -> a newline -> start-of-line
   inside-instruction -> , -> in-space
   inside-instruction -> a space -> in-space
   inside-instruction -> any other character -> inside-instruction

   in-space
   description of state: inside whitespace characters between instruction and parameters (in case there is one or more parameters).
   in-space -> # -> hash-sign-read
   in-space -> ( -> inside-lisp-form (set `n-lisp-forms` to 1).
   in-space -> ) -> error (cannot terminate Lisp form outside a Lisp form).
   in-space -> [ -> opening-square-bracket
   in-space -> ] -> error (cannot terminate memory address syntax outside memory address syntax).
   in-space -> ; -> inside-comment
   in-space -> / -> slash
   in-space -> \ -> backslash-in-space
   in-space -> a newline -> start-of-line
   in-space -> a space -> in-space (do not output anything).
   in-space -> any other character -> inside-parameters

   backslash-in-space
   description of state: immediately after a backslash in space between instruction and parameters (in case there is one or more parameters).
   backslash-in-space -> TODO!

   inside-parameters
   description of state: inside parameter.
   inside-parameters -> # -> error (a whitespace is required between a parameter and hash sign).
   inside-parameters -> ( -> error (a whitespace is required between a parameter and a Lisp form).
   inside-parameters -> ) -> error (cannot terminate Lisp form outside a Lisp form).
   inside-parameters -> [ -> error (cannot begin memory address syntax inside a parameter).
   inside-parameters -> ] -> error (cannot terminate memory address syntax inside a parameter).
   inside-parameters -> ; -> inside-comment
   inside-parameters -> / -> slash
   inside-parameters -> \ -> backslash-inside-parameters
   inside-parameters -> a newline -> start-of-line
   inside-parameters -> , -> in-space
   inside-parameters -> a space -> in-space
   inside-parameters -> any other character -> inside-parameters

   backslash-inside-parameters
   description of state: backslash inside parameters.
   backslash-inside-parameters -> TODO!

   opening-square-bracket
   description of state: the last character was a opening square bracket that opened memory address syntax.
   opening-square-bracket -> # -> error (memory address syntax must be terminated with a closing square bracket before a hash sign)
   opening-square-bracket -> ( -> inside-lisp-form-inside-memory-address-syntax (set `n-lisp-forms` to 1).
   opening-square-bracket -> ) -> error (cannot terminate Lisp form outside a Lisp form)
   opening-square-bracket -> [ -> error (cannot begin a new memory address syntax inside memory address syntax).
   opening-square-bracket -> ] -> closing-square-bracket (the content of `memory-address-syntax-buffer` will be converted to intermediate representation).
   opening-square-bracket -> ; -> error (memory address syntax must be terminated with a closing square bracket before comment).
   opening-square-bracket -> / -> slash
   opening-square-bracket -> \ -> backslash-inside-memory-address-syntax
   opening-square-bracket -> + -> plus-inside-memory-address-syntax
   opening-square-bracket -> a newline -> space-inside-memory-address-syntax
   opening-square-bracket -> a space -> space-inside-memory-address-syntax
   opening-square-bracket -> any other character -> inside-memory-address-syntax

   inside-memory-address-syntax
   description of state: inside memory address syntax between the square brackets. The content between square brackets will be parsed as a parameter.
   inside-memory-address-syntax -> # -> error (memory address syntax must be terminated with a closing square bracket before a hash sign).
   inside-memory-address-syntax -> ( -> inside-lisp-form-inside-memory-address-syntax (set `n-lisp-forms` to 1). TODO
   inside-memory-address-syntax -> ) -> error (cannot terminate Lisp form outside a Lisp form).
   inside-memory-address-syntax -> [ -> error (cannot begin a new memory address syntax inside memory address syntax).
   inside-memory-address-syntax -> ] -> closing-square-bracket (the content of `memory-address-syntax-buffer` will be converted to intermediate representation).
   inside-memory-address-syntax -> ; -> error (memory address syntax must be terminated with a closing square bracket before comment).
   inside-memory-address-syntax -> / -> slash
   inside-memory-address-syntax -> \ -> backslash-inside-memory-address-syntax
   inside-memory-address-syntax -> + -> plus-inside-memory-address-syntax
   inside-memory-address-syntax -> a newline -> space-inside-memory-address-syntax (do not output anything).
   inside-memory-address-syntax -> a space -> space-inside-memory-address-syntax (do not output anything).
   inside-memory-address-syntax -> any other character -> inside-memory-address-syntax

   space-inside-memory-address-syntax
   description of state: space inside memory address syntax.
   space-inside-memory-address-syntax -> # -> error (memory address syntax must be terminated with a closing square bracket before a hash sign).
   space-inside-memory-address-syntax -> ( -> inside-lisp-form-inside-memory-address-syntax (set `n-lisp-forms` to 1). TODO!
   space-inside-memory-address-syntax -> ) -> error (cannot terminate Lisp form outside a Lisp form).
   space-inside-memory-address-syntax -> [ -> error (cannot begin a new memory address syntax inside memory address syntax).
   space-inside-memory-address-syntax -> ] -> closing-square-bracket (the content of `memory-address-syntax-buffer` will be converted to intermediate representation).
   space-inside-memory-address-syntax -> ; -> error (memory address syntax must be terminated with a closing square bracket before comment).
   space-inside-memory-address-syntax -> / -> slash
   space-inside-memory-address-syntax -> \ -> backslash-inside-memory-address-syntax
   space-inside-memory-address-syntax -> + -> plus-inside-memory-address-syntax
   space-inside-memory-address-syntax -> a newline -> error (memory address syntax must be terminated with a closing square bracket before newline).
   space-inside-memory-address-syntax -> a space -> space-inside-memory-address-syntax
   space-inside-memory-address-syntax -> any other character -> inside-memory-address-syntax (output space if needed and always the character).

   plus-inside-memory-address-syntax
   description: plus sign inside memory address syntax.
   plus-inside-memory-address-syntax -> # -> error (memory address syntax must be terminated with a closing square bracket before a hash sign).
   plus-inside-memory-address-syntax -> ( -> inside-lisp-form-inside-memory-address-syntax (set `n-lisp-forms` to 1). TODO!
   plus-inside-memory-address-syntax -> ) -> error (cannot terminate Lisp form outside a Lisp form).
   plus-inside-memory-address-syntax -> [ -> error (cannot begin a new memory address syntax inside memory address syntax).
   plus-inside-memory-address-syntax -> ] -> closing-square-bracket (the content of `memory-address-syntax-buffer` will be converted to intermediate representation).
   plus-inside-memory-address-syntax -> ; -> error (memory address syntax must be terminated with a closing square bracket before comment).
   plus-inside-memory-address-syntax -> / -> slash
   plus-inside-memory-address-syntax -> \ -> backslash-inside-memory-address-syntax
   plus-inside-memory-address-syntax -> + -> plus-inside-memory-address-syntax
   plus-inside-memory-address-syntax -> a newline -> plus-inside-memory-address-syntax (do not output anything).
   plus-inside-memory-address-syntax -> a space -> plus-inside-memory-address-syntax (do not output anything).
   plus-inside-memory-address-syntax -> any other character -> inside-memory-address-syntax

   backslash-inside-memory-address-syntax
   description of state: backslash inside memory address syntax.
   backslash-inside-memory-address-syntax -> TODO!

   inside-lisp-form-inside-memory-address-syntax TODO!

   closing-square-bracket
   description of state: the last character was a closing square bracket that closed memory address syntax.
   closing-square-bracket -> # -> error (a whitespace is required between closing square bracket and hash sign).
   closing-square-bracket -> ( -> error (a whitespace is required between closing square bracket and a Lisp form).
   closing-square-bracket -> ) -> error (cannot terminate Lisp form outside a Lisp form).
   closing-square-bracket -> [ -> error (a whitespace is required between closing square bracket and opening square bracket).
   closing-square-bracket -> ] -> error (cannot terminate the same terminate memory address syntax twice).
   closing-square-bracket -> ; -> inside-comment
   closing-square-bracket -> / -> slash
   closing-square-bracket -> a newline -> start-of-line
   closing-square-bracket -> , -> in-space
   closing-square-bracket -> a space -> in-space
   closing-square-bracket -> any other character -> error (a whitespace is required after closing square bracket).

   closing-parenthesis
   description of state: the last character was a closing parenthesis that closed a Lisp form.
   closing-parenthesis -> # -> error (a whitespace is required between closing parenthesis and hash sign).
   closing-parenthesis -> ( -> error (a whitespace is required between closing parenthesis and a Lisp form).
   closing-parenthesis -> ) -> error (cannot terminate Lisp form outside a Lisp form).
   closing-parenthesis -> [ -> error (a whitespace is required between closing parenthesis and opening square bracket).
   closing-parenthesis -> ] -> error (cannot terminate memory address syntax outside memory address syntax).
   closing-parenthesis -> ; -> inside-comment
   closing-parenthesis -> / -> slash
   closing-parenthesis -> a newline -> start-of-line
   closing-parenthesis -> , -> in-space
   closing-parenthesis -> a space -> in-space
   closing-parenthesis -> any other character -> error (a whitespace is required after closing parenthesis).

   inside-comment
   description of state: inside comment text.
   inside-comment -> a newline -> start-of-line
   inside-comment -> any other character -> inside-comment (do not output anything).

   slash -> * -> inside-c-comment
   description of state: last character was slash `/`.
   slash -> any other character -> pop earlier state from state stack (output / and the current character).

   inside-c-comment
   description of state: inside comment delimited by `/*` and `*/`.
   inside-c-comment -> * -> asterisk-inside-c-comment

   asterisk-inside-c-comment
   description of state: last character was asterisk `*` inside C comment.
   asterisk-inside-c-comment -> / -> pop earlier state from state stack.
   asterisk-inside-c-comment -> * -> asterisk-inside-c-comment (do not output anything).
   asterisk-inside-c-comment -> any other other character -> inside-c-comment (do not output anything).

   #l marks change to Lisp mode (Common Lisp macros are executed during assembling of the code), or a new Lisp mode instance if we are on Lisp mode.
   #a marks return to asm, or a new instruction if we are on asm mode.
   #e marks end of syntax.
   Partially based on: http://weitz.de/macros.lisp"
  (let*
    ((asm-reader (make-instance 'asm-reader))
     (invalid-last-characters (list "'" " " "(" ")"))
     (is-there-code-on-this-line nil)
     (current-state "start-of-line")
     (current-lisp-state "regular")
     (state-stack nil)
     (ast-string "(list ")
     (lisp-code-string "")
     (n-lisp-forms 0))
    ;; loop through stream.
    (loop for my-char = (coerce (list (read-char stream t nil t)) 'string)
          do (cond
               ((equal current-mode "asm")
                (cond
                  ;; are we in the start of the line?
                  ((equal current-state "start-of-line")
                   ;; temporary code before `asm-reader` is taken fully into use.
                   (setf (current-state asm-reader) current-state)
                   (setf (current-lisp-state asm-reader) current-lisp-state)
                   (setf (state-stack asm-reader) state-stack)
                   (setf (ast-string asm-reader) ast-string)
                   (setf (lisp-code-string asm-reader) lisp-code-string)
                   (setf (n-lisp-forms asm-reader) n-lisp-forms)
                   (setf (is-there-code-on-this-line asm-reader) is-there-code-on-this-line)
                   ;; temporary code before `asm-reader` is taken fully into use ends here.
                   (setf (values is-there-code-on-this-line current-state state-stack ast-string)
                         (asm-start-of-line asm-reader my-char)))
                  ((equal current-state "hash-sign-read")
                   (cond
                     ;; is character a ?
                     ;; if yes, do exactly the same is if it was newline.
                     ((equal my-char "a")
                      ;; temporary code before `asm-reader` is taken fully into use.
                      (setf (current-state asm-reader) current-state)
                      (setf (current-lisp-state asm-reader) current-lisp-state)
                      (setf (state-stack asm-reader) state-stack)
                      (setf (ast-string asm-reader) ast-string)
                      (setf (lisp-code-string asm-reader) lisp-code-string)
                      (setf (n-lisp-forms asm-reader) n-lisp-forms)
                      (setf (is-there-code-on-this-line asm-reader) is-there-code-on-this-line)
                      ;; temporary code before `asm-reader` is taken fully into use ends here.
                      (setf asm-reader (new-instruction asm-reader))
                      ;; temporary code before `asm-reader` is taken fully into use.
                      (setf current-state (current-state asm-reader))
                      (setf current-lisp-state (current-lisp-state asm-reader))
                      (setf state-stack (state-stack asm-reader))
                      (setf ast-string (ast-string asm-reader))
                      (setf lisp-code-string (lisp-code-string asm-reader))
                      (setf n-lisp-forms (n-lisp-forms asm-reader))
                      (setf is-there-code-on-this-line (is-there-code-on-this-line asm-reader)))
                      ;; temporary code before `asm-reader` is taken fully into use ends here.
                     ;; is character e ?
                     ;; if yes, we are done, fix closing parentheses and return.
                     ((equal my-char "e")
                      (return-from transform-code-to-string
                                   (concatenate 'string (get-string-without-invalid-last-character
                                                          (get-string-without-invalid-last-character
                                                            ast-string invalid-last-characters)
                                                          invalid-last-characters) "))")))
                     ((equal my-char "l")
                      (setf current-mode "Lisp")
                      (setf lisp-code-string "")
                      ;; temporary code before `asm-reader` is taken fully into use.
                      (setf (current-state asm-reader) current-state)
                      (setf (current-lisp-state asm-reader) current-lisp-state)
                      (setf (state-stack asm-reader) state-stack)
                      (setf (ast-string asm-reader) ast-string)
                      (setf (lisp-code-string asm-reader) lisp-code-string)
                      (setf (n-lisp-forms asm-reader) n-lisp-forms)
                      (setf (is-there-code-on-this-line asm-reader) is-there-code-on-this-line)
                      ;; temporary code before `asm-reader` is taken fully into use ends here.
                      (setf asm-reader (new-instruction asm-reader))
                      ;; temporary code before `asm-reader` is taken fully into use.
                      (setf current-state (current-state asm-reader))
                      (setf current-lisp-state (current-lisp-state asm-reader))
                      (setf state-stack (state-stack asm-reader))
                      (setf ast-string (ast-string asm-reader))
                      (setf lisp-code-string (lisp-code-string asm-reader))
                      (setf n-lisp-forms (n-lisp-forms asm-reader))
                      (setf is-there-code-on-this-line (is-there-code-on-this-line asm-reader)))
                      ;; temporary code before `asm-reader` is taken fully into use ends here.
                     ;; otherwise, print error.
                     (t (error "in asm mode undefined control character after #"))))
                  ((equal current-state "inside-lisp-form")
                   (cond
                     ;; is this opening parenthesis?
                     ;; if yes, increment parenthesis count and output (
                     ((equal my-char "(")
                      (incf n-lisp-forms)
                      (setf ast-string (concatenate 'string ast-string "(")))
                     ;; is this closing parenthesis?
                     ;; if yes, output )" if last, otherwise output )
                     ((equal my-char ")")
                      (cond
                        ((eql (decf n-lisp-forms) 0)
                         (setf current-state "closing-parenthesis")
                         (setf ast-string (concatenate 'string ast-string ")\"")))
                        (t (setf ast-string (concatenate 'string ast-string ")")))))
                     ;; is character newline?
                     ;; if yes, mark are in space inside a Lisp form.
                     ((equal my-char (coerce (list #\Newline) 'string))
                      (setf current-state "space-inside-lisp-form"))
                     ;; is character space?
                     ;; if yes, mark are in space inside a Lisp form.
                     ((equal my-char " ")
                      (setf current-state "space-inside-lisp-form"))
                     ;; otherwise output the character.
                     (t (setf ast-string (concatenate 'string ast-string my-char)))))
                  ((equal current-state "space-inside-lisp-form")
                   (cond
                     ;; is this opening parenthesis?
                     ;; if yes, increment parenthesis count and output (
                     ((equal my-char "(")
                      (incf n-lisp-forms)
                      (unless
                        (equal (get-last-character-string ast-string) " ")
                        ;; if last character was not space, output space.
                        (setf ast-string (concatenate 'string ast-string " ")))
                      (setf ast-string (concatenate 'string ast-string "(")))
                     ;; is this closing parenthesis?
                     ;; if yes, output )" if last, otherwise output )
                     ((equal my-char ")")
                      (cond
                        ((eql (decf n-lisp-forms) 0)
                         (setf current-state "closing-parenthesis")
                         (setf ast-string (concatenate 'string ast-string ")\"")))
                        (t (setf ast-string (concatenate 'string ast-string ")")))))
                     ;; is character newline?
                     ;; if yes, do not output anything.
                     ((equal my-char (coerce (list #\Newline) 'string))
                      nil)
                     ;; is character space?
                     ;; if yes, do not output anything.
                     ((equal my-char " ")
                      nil)
                     ;; otherwise output the character and return to inside-lisp-form phase.
                     (t (setf current-state "inside-lisp-form")
                      (unless
                        (equal (get-last-character-string ast-string) " ")
                        ;; if last character was not space, output space.
                        (setf ast-string (concatenate 'string ast-string " ")))
                      (setf ast-string (concatenate 'string ast-string my-char)))))
                  ((equal current-state "inside-instruction")
                   (cond
                     ((equal my-char "#")
                      (error "a whitespace is required between instruction and hash sign"))
                     ((equal my-char "[")
                      (error "cannot begin memory address syntax inside instruction"))
                     ((equal my-char "]")
                      (error "cannot terminate memory address syntax inside instruction"))
                     ;; is character ; ?
                     ;; if yes, don't output anything, begin comment.
                     ((equal my-char ";")
                      (setf current-state "inside-comment"))
                     ;; is character / ?
                     ;; if yes, mark we have a slash.
                     ((equal my-char "/")
                      (push current-state state-stack)
                      (setf current-state "slash"))
                     ;; is character newline?
                     ;; if yes, start a new instruction.
                     ((equal my-char (coerce (list #\Newline) 'string))
                      ;; temporary code before `asm-reader` is taken fully into use.
                      (setf (current-state asm-reader) current-state)
                      (setf (current-lisp-state asm-reader) current-lisp-state)
                      (setf (state-stack asm-reader) state-stack)
                      (setf (ast-string asm-reader) ast-string)
                      (setf (lisp-code-string asm-reader) lisp-code-string)
                      (setf (n-lisp-forms asm-reader) n-lisp-forms)
                      (setf (is-there-code-on-this-line asm-reader) is-there-code-on-this-line)
                      ;; temporary code before `asm-reader` is taken fully into use ends here.
                      (setf asm-reader (new-instruction asm-reader))
                      ;; temporary code before `asm-reader` is taken fully into use.
                      (setf current-state (current-state asm-reader))
                      (setf current-lisp-state (current-lisp-state asm-reader))
                      (setf state-stack (state-stack asm-reader))
                      (setf ast-string (ast-string asm-reader))
                      (setf lisp-code-string (lisp-code-string asm-reader))
                      (setf n-lisp-forms (n-lisp-forms asm-reader))
                      (setf is-there-code-on-this-line (is-there-code-on-this-line asm-reader)))
                      ;; temporary code before `asm-reader` is taken fully into use ends here.
                     ;; is character , ?
                     ;; if yes, mark we are inside space, output "
                     ((equal my-char ",")
                      (push current-state state-stack)
                      (setf current-state "in-space")
                      (setf ast-string (concatenate 'string ast-string "\"")))
                     ;; is character space?
                     ;; if yes, mark we are inside space, output "
                     ((equal my-char " ")
                      (push current-state state-stack)
                      (setf current-state "in-space")
                      (setf ast-string (concatenate 'string ast-string "\"")))
                     ;; otherwise output the character.
                     (t (setf ast-string (concatenate 'string ast-string my-char)))))
                  ((equal current-state "in-space")
                   (cond
                     ;; is character # ?
                     ;; if yes, mark hash sign read, do not output anything.
                     ((equal my-char "#")
                      (push current-state state-stack)
                      (setf current-state "hash-sign-read"))
                     ((equal my-char "(")
                      ;; is this a Lisp form (with parentesis)?
                      ;; if yes, mark we are inside Lisp form, output "(
                      (push current-state state-stack)
                      (setf current-state "inside-lisp-form")
                      (setf n-lisp-forms 1)
                      (unless (equal (get-last-character-string ast-string) " ")
                        ;; if last character was not space, output space.
                        (setf ast-string (concatenate 'string ast-string " ")))
                      (setf ast-string (concatenate 'string ast-string "\"(")))
                     ((equal my-char ")")
                      (error "cannot terminate Lisp form outside a Lisp form"))
                     ;; is this memory address syntax (with square brackets)?
                     ;; if yes, mark we are inside memory address syntax, output "[
                     ((equal my-char "[")
                      (push current-state state-stack)
                      (setf current-state "opening-square-bracket")
                      (unless (equal (get-last-character-string ast-string) " ")
                        ;; if last character was not space, output space.
                        (setf ast-string (concatenate 'string ast-string " ")))
                      (setf ast-string (concatenate 'string ast-string "\"[")))
                     ((equal my-char "]")
                      (error "cannot terminate memory address syntax outside memory address syntax"))
                     ;; is character ; ?
                     ;; if yes, don't output anything, begin comment.
                     ((equal my-char ";")
                      (setf current-state "inside-comment"))
                     ;; is character / ?
                     ;; if yes, mark we have a slash.
                     ((equal my-char "/")
                      (push current-state state-stack)
                      (setf current-state "slash"))
                     ;; is character backslash?
                     ;; if yes, mark we have a backslash in space.
                     ((equal my-char "\\")
                      (push current-state state-stack)
                      (setf current-state "backslash-in-space"))
                     ;; is character newline?
                     ;; if yes, start a new instruction.
                     ((equal my-char (coerce (list #\Newline) 'string))
                      ;; temporary code before `asm-reader` is taken fully into use.
                      (setf (current-state asm-reader) current-state)
                      (setf (current-lisp-state asm-reader) current-lisp-state)
                      (setf (state-stack asm-reader) state-stack)
                      (setf (ast-string asm-reader) ast-string)
                      (setf (lisp-code-string asm-reader) lisp-code-string)
                      (setf (n-lisp-forms asm-reader) n-lisp-forms)
                      (setf (is-there-code-on-this-line asm-reader) is-there-code-on-this-line)
                      ;; temporary code before `asm-reader` is taken fully into use ends here.
                      (setf asm-reader (new-instruction asm-reader))
                      ;; temporary code before `asm-reader` is taken fully into use.
                      (setf current-state (current-state asm-reader))
                      (setf current-lisp-state (current-lisp-state asm-reader))
                      (setf state-stack (state-stack asm-reader))
                      (setf ast-string (ast-string asm-reader))
                      (setf lisp-code-string (lisp-code-string asm-reader))
                      (setf n-lisp-forms (n-lisp-forms asm-reader))
                      (setf is-there-code-on-this-line (is-there-code-on-this-line asm-reader)))
                      ;; temporary code before `asm-reader` is taken fully into use ends here.
                     ;; is character space?
                     ;; if yes, do not output anything.
                     ((equal my-char " ")
                      nil)
                     ;; otherwise mark we are inside parameters, output " and the character.
                     (t
                       (setf current-state "inside-parameters")
                       (unless (equal (get-last-character-string ast-string) " ")
                         ;; if last character was not space, output space.
                         (setf ast-string (concatenate 'string ast-string " ")))
                       (setf ast-string (concatenate 'string ast-string "\"" my-char)))))
                  ((equal current-state "inside-parameters")
                   (cond
                     ((equal my-char "#")
                      (error "a whitespace is required between a parameter and hash sign"))
                     ((equal my-char "(")
                      (error "a whitespace is required between a parameter and a Lisp form"))
                     ((equal my-char ")")
                      (error "cannot terminate Lisp form outside a Lisp form"))
                     ((equal my-char "[")
                      (error "a whitespace is required between a parameter and opening square bracket"))
                     ((equal my-char "]")
                      (error "cannot terminate memory address syntax outside memory address syntax"))
                     ;; is character ; ?
                     ;; if yes, don't output anything, begin comment.
                     ((equal my-char ";")
                      (setf current-state "inside-comment"))
                     ;; is character / ?
                     ;; if yes, mark we have a slash.
                     ((equal my-char "/")
                      (push current-state state-stack)
                      (setf current-state "slash"))
                     ;; is character backslash?
                     ;; if yes, mark we have a backslash inside parameters.
                     ((equal my-char "\\")
                      (push current-state state-stack)
                      (setf current-state "backslash-inside-parameters"))
                     ;; is character newline?
                     ;; if yes, start a new instruction.
                     ((equal my-char (coerce (list #\Newline) 'string))
                      ;; temporary code before `asm-reader` is taken fully into use.
                      (setf (current-state asm-reader) current-state)
                      (setf (current-lisp-state asm-reader) current-lisp-state)
                      (setf (state-stack asm-reader) state-stack)
                      (setf (ast-string asm-reader) ast-string)
                      (setf (lisp-code-string asm-reader) lisp-code-string)
                      (setf (n-lisp-forms asm-reader) n-lisp-forms)
                      (setf (is-there-code-on-this-line asm-reader) is-there-code-on-this-line)
                      ;; temporary code before `asm-reader` is taken fully into use ends here.
                      (setf asm-reader (new-instruction asm-reader))
                      ;; temporary code before `asm-reader` is taken fully into use.
                      (setf current-state (current-state asm-reader))
                      (setf current-lisp-state (current-lisp-state asm-reader))
                      (setf state-stack (state-stack asm-reader))
                      (setf ast-string (ast-string asm-reader))
                      (setf lisp-code-string (lisp-code-string asm-reader))
                      (setf n-lisp-forms (n-lisp-forms asm-reader))
                      (setf is-there-code-on-this-line (is-there-code-on-this-line asm-reader)))
                      ;; temporary code before `asm-reader` is taken fully into use ends here.
                     ;; is character , ?
                     ;; if yes, mark we are in space between parameters, do not output anything.
                     ((equal my-char ",")
                      (push current-state state-stack)
                      (setf current-state "in-space")
                      (setf ast-string (concatenate 'string ast-string "\"")))
                     ;; is character space?
                     ;; if yes, mark we are in space between parameters, do not output anything.
                     ((equal my-char " ")
                      (push current-state state-stack)
                      (setf current-state "in-space")
                      (setf ast-string (concatenate 'string ast-string "\"")))
                     ;; otherwise output the character.
                     (t (setf ast-string (concatenate 'string ast-string my-char)))))
                  ((equal current-state "opening-square-bracket")
                   (cond
                     ((equal my-char "#")
                      (error "memory address syntax must be terminated with a closing square bracket before a hash sign"))
                     ((equal my-char "(")
                      ;; is this a Lisp form (with parentesis)?
                      ;; if yes, mark we are inside Lisp form inside memory address syntax, output " and current character.
                      (push current-state state-stack)
                      (setf current-state "inside-lisp-form-inside-memory-address-syntax")
                      (setf n-lisp-forms 1)
                      (setf ast-string (concatenate 'string ast-string "\"" my-char)))
                     ((equal my-char ")")
                      (error "cannot terminate Lisp form outside a Lisp form"))
                     ((equal my-char "[")
                      (error "cannot begin memory address syntax inside memory address syntax"))
                     ;; is character ] ?
                     ;; if yes, mark we are at the closing square bracket, output ]"
                     ((equal my-char "]")
                      (setf current-state "closing-square-bracket")
                      (setf ast-string (concatenate 'string ast-string "]\"")))
                     ((equal my-char ";")
                      (error "memory address syntax must be terminated with a closing square bracket before a comment"))
                     ;; is character / ?
                     ;; if yes, mark we have a slash.
                     ((equal my-char "/")
                      (push "space-inside-memory-address-syntax" state-stack)
                      (setf current-state "slash"))
                     ;; is character backslash?
                     ;; if yes, mark we have a backslash inside memory address syntax
                     ((equal my-char "\\")
                      (setf current-state "backslash-inside-memory-address-syntax"))
                     ;; is character +
                     ;; if yes, mark we are at plus inside memory address syntax, output +
                     ((equal my-char "+")
                      (setf current-state "plus-inside-memory-address-syntax")
                      (setf ast-string (concatenate 'string ast-string "+")))
                     ;; is character newline?
                     ;; if yes, mark are in space inside memory address syntax, do not output anything.
                     ((equal my-char (coerce (list #\Newline) 'string))
                      (setf current-state "space-inside-memory-address-syntax"))
                     ;; is character space?
                     ((equal my-char " ")
                      (setf current-state "space-inside-memory-address-syntax"))
                     ;; otherwise mark we are inside memory address syntax and output the character.
                     (t
                      (setf current-state "inside-memory-address-syntax")
                      (setf ast-string (concatenate 'string ast-string my-char)))))
                  ((equal current-state "inside-memory-address-syntax")
                   (cond
                     ((equal my-char "#")
                      (error "memory address syntax must be terminated with a closing square bracket before a hash sign"))
                     ((equal my-char "(")
                      ;; is this a Lisp form (with parentesis)?
                      ;; if yes, mark we are inside Lisp form inside memory address syntax, output " and current character.
                      (push current-state state-stack)
                      (setf current-state "inside-lisp-form-inside-memory-address-syntax")
                      (setf n-lisp-forms 1)
                      (setf ast-string (concatenate 'string ast-string "\"" my-char)))
                     ((equal my-char ")")
                      (error "cannot terminate Lisp form outside a Lisp form"))
                     ((equal my-char "[")
                      (error "cannot begin memory address syntax inside memory address syntax"))
                     ;; is character ] ?
                     ;; if yes, mark we are at the closing square bracket, output ]"
                     ((equal my-char "]")
                      (setf current-state "closing-square-bracket")
                      (setf ast-string (concatenate 'string ast-string "]\"")))
                     ((equal my-char ";")
                      (error "memory address syntax must be terminated with a closing square bracket before a comment"))
                     ;; is character / ?
                     ;; if yes, mark we have a slash.
                     ((equal my-char "/")
                      (push "space-inside-memory-address-syntax" state-stack)
                      (setf current-state "slash"))
                     ;; is character backslash?
                     ;; if yes, mark we have a backslash inside memory address syntax
                     ((equal my-char "\\")
                      (setf current-state "backslash-inside-memory-address-syntax"))
                     ;; is character +
                     ;; if yes, mark we are at plus inside memory address syntax, output +
                     ((equal my-char "+")
                      (setf current-state "plus-inside-memory-address-syntax")
                      (setf ast-string (concatenate 'string ast-string "+")))
                     ;; is character newline?
                     ;; if yes, mark are in space inside memory address syntax, do not output anything.
                     ((equal my-char (coerce (list #\Newline) 'string))
                      (setf current-state "space-inside-memory-address-syntax"))
                     ;; is character space?
                     ((equal my-char " ")
                      (setf current-state "space-inside-memory-address-syntax"))
                     ;; otherwise output the character.
                     (t (setf ast-string (concatenate 'string ast-string my-char)))))
                  ((equal current-state "space-inside-memory-address-syntax")
                   (cond
                     ((equal my-char "#")
                      (error "memory address syntax must be terminated with a closing square bracket before a hash sign"))
                     ((equal my-char "(")
                      ;; is this a Lisp form (with parentesis)?
                      ;; if yes, mark we are inside Lisp form, output " and current character.
                      (push current-state state-stack)
                      (setf current-state "inside-lisp-form-inside-memory-address-syntax")
                      (setf n-lisp-forms 1)
                      (setf ast-string (concatenate 'string ast-string "\"(")))
                     ((equal my-char ")")
                      (error "cannot terminate Lisp form outside a Lisp form"))
                     ((equal my-char "[")
                      (error "cannot begin memory address syntax inside memory address syntax"))
                     ;; is character ] ?
                     ;; if yes, mark we are at the closing square bracket, output ]"
                     ((equal my-char "]")
                      (setf current-state "closing-square-bracket")
                      (setf ast-string (concatenate 'string ast-string "]\"")))
                     ((equal my-char ";")
                      (error "memory address syntax must be terminated with a closing square bracket before a comment"))
                     ;; is character / ?
                     ;; if yes, mark we have a slash.
                     ((equal my-char "/")
                      (push current-state state-stack)
                      (setf current-state "slash"))
                     ;; is character backslash?
                     ;; if yes, mark we have a backslash inside memory address syntax
                     ((equal my-char "\\")
                      (setf current-state "backslash-inside-memory-address-syntax"))
                     ;; is character +
                     ;; if yes, mark we are at plus inside memory address syntax, output +
                     ((equal my-char "+")
                      (setf current-state "plus-inside-memory-address-syntax")
                      (setf ast-string (concatenate 'string ast-string "+")))
                     ;; is character newline?
                     ;; if yes, do not output anything.
                     ((equal my-char (coerce (list #\Newline) 'string))
                      nil)
                     ;; is character space?
                     ;; if yes, do not output anything.
                     ((equal my-char " ")
                      nil)
                     ;; otherwise mark that we are inside memory address syntax, output space if needed, and the character.
                     (t
                      (setf current-state "inside-memory-address-syntax")
                      (unless
                        (or
                          (equal (get-last-character-string ast-string) " ")
                          (equal (get-last-character-string ast-string) "["))
                        ;; if last character was not space or [, output space.
                        (setf ast-string (concatenate 'string ast-string " ")))
                      (setf ast-string (concatenate 'string ast-string my-char)))))
                  ((equal current-state "plus-inside-memory-address-syntax")
                   (cond
                     ((equal my-char "#")
                      (error "memory address syntax must be terminated with a closing square bracket before a hash sign"))
                     ((equal my-char "(")
                      ;; is this a Lisp form (with parentesis)?
                      ;; if yes, mark we are inside Lisp form inside memory address syntax, output " and current character.
                      (push current-state state-stack)
                      (setf current-state "inside-lisp-form-inside-memory-address-syntax")
                      (setf n-lisp-forms 1)
                      (setf ast-string (concatenate 'string ast-string "\"(")))
                     ((equal my-char ")")
                      (error "cannot terminate Lisp form outside a Lisp form"))
                     ((equal my-char "[")
                      (error "cannot begin memory address syntax inside memory address syntax"))
                     ;; is character ] ?
                     ;; if yes, mark we are at the closing square bracket, output ]"
                     ((equal my-char "]")
                      (setf current-state "closing-square-bracket")
                      (setf ast-string (concatenate 'string ast-string "]\"")))
                     ((equal my-char ";")
                      (error "memory address syntax must be terminated with a closing square bracket before a comment"))
                     ;; is character / ?
                     ;; if yes, mark we have a slash.
                     ((equal my-char "/")
                      (push current-state state-stack)
                      (setf current-state "slash"))
                     ;; is character backslash?
                     ;; if yes, mark we have a backslash inside memory address syntax
                     ((equal my-char "\\")
                      (push current-state state-stack)
                      (setf current-state "backslash-inside-memory-address-syntax"))
                     ;; is character +
                     ;; if yes, output +
                     ((equal my-char "+")
                      (setf ast-string (concatenate 'string ast-string "+")))
                     ;; is character newline?
                     ;; if yes, do not output anything.
                     ((equal my-char (coerce (list #\Newline) 'string))
                      nil)
                     ;; is character space?
                     ;; if yes, do not output anything.
                     ((equal my-char " ")
                      nil)
                     ;; otherwise mark we are inside memory address syntax, output the character.
                     (t
                      (setf current-state "inside-memory-address-syntax")
                      (setf ast-string (concatenate 'string ast-string my-char)))))
                  ((equal current-state "closing-square-bracket")
                   (cond
                     ((equal my-char "#")
                      (error "a whitespace is required between closing square bracket and hash sign"))
                     ((equal my-char "(")
                      (error "a whitespace is required between closing square bracket and a Lisp form"))
                     ((equal my-char ")")
                      (error "cannot terminate Lisp form outside a Lisp form"))
                     ((equal my-char "[")
                      (error "a whitespace is required between closing square bracket and opening square bracket"))
                     ((equal my-char "]")
                      (error "cannot terminate the same terminate memory address syntax twice"))
                     ;; is character ; ?
                     ;; if yes, don't output anything, begin comment.
                     ((equal my-char ";")
                      (setf current-state "inside-comment"))
                     ;; is character / ?
                     ;; if yes, mark we have a slash.
                     ((equal my-char "/")
                      (setf current-state "slash"))
                     ;; is character newline?
                     ;; if yes, start a new instruction.
                     ((equal my-char (coerce (list #\Newline) 'string))
                      ;; temporary code before `asm-reader` is taken fully into use.
                      (setf (current-state asm-reader) current-state)
                      (setf (current-lisp-state asm-reader) current-lisp-state)
                      (setf (state-stack asm-reader) state-stack)
                      (setf (ast-string asm-reader) ast-string)
                      (setf (lisp-code-string asm-reader) lisp-code-string)
                      (setf (n-lisp-forms asm-reader) n-lisp-forms)
                      (setf (is-there-code-on-this-line asm-reader) is-there-code-on-this-line)
                      ;; temporary code before `asm-reader` is taken fully into use ends here.
                      (setf asm-reader (new-instruction asm-reader))
                      ;; temporary code before `asm-reader` is taken fully into use.
                      (setf current-state (current-state asm-reader))
                      (setf current-lisp-state (current-lisp-state asm-reader))
                      (setf state-stack (state-stack asm-reader))
                      (setf ast-string (ast-string asm-reader))
                      (setf lisp-code-string (lisp-code-string asm-reader))
                      (setf n-lisp-forms (n-lisp-forms asm-reader))
                      (setf is-there-code-on-this-line (is-there-code-on-this-line asm-reader)))
                      ;; temporary code before `asm-reader` is taken fully into use ends here.
                     ;; is character , ?
                     ;; if yes, mark we are in space between parameters, do not output anything.
                     ((equal my-char ",")
                      (setf current-state (pop state-stack)))
                     ;; is character space?
                     ;; if yes, mark we are in space between parameters, do not output anything.
                     ((equal my-char " ")
                      (setf current-state (pop state-stack)))
                     ;; otherwise produce an error.
                     (t (error "a whitespace is required after closing square bracket"))))
                  ((equal current-state "closing-parenthesis")
                   (cond
                     ((equal my-char "#")
                      (error "a whitespace is required between closing parenthesis and hash sign"))
                     ((equal my-char "(")
                      (error "a whitespace is required between closing parenthesis and a Lisp form"))
                     ((equal my-char ")")
                      (error "cannot terminate Lisp form outside a Lisp form"))
                     ((equal my-char "[")
                      (error "a whitespace is required between closing parenthesis and opening square bracket"))
                     ((equal my-char "]")
                      (error "cannot terminate memory address syntax outside memory address syntax"))
                     ;; is character ; ?
                     ;; if yes, don't output anything, begin comment.
                     ((equal my-char ";")
                      (setf current-state "inside-comment"))
                     ;; is character / ?
                     ;; if yes, mark we have a slash.
                     ((equal my-char "/")
                      (setf current-state "slash"))
                     ;; is character newline?
                     ;; if yes, start a new instruction.
                     ((equal my-char (coerce (list #\Newline) 'string))
                      ;; temporary code before `asm-reader` is taken fully into use.
                      (setf (current-state asm-reader) current-state)
                      (setf (current-lisp-state asm-reader) current-lisp-state)
                      (setf (state-stack asm-reader) state-stack)
                      (setf (ast-string asm-reader) ast-string)
                      (setf (lisp-code-string asm-reader) lisp-code-string)
                      (setf (n-lisp-forms asm-reader) n-lisp-forms)
                      (setf (is-there-code-on-this-line asm-reader) is-there-code-on-this-line)
                      ;; temporary code before `asm-reader` is taken fully into use ends here.
                      (setf asm-reader (new-instruction asm-reader))
                      ;; temporary code before `asm-reader` is taken fully into use.
                      (setf current-state (current-state asm-reader))
                      (setf current-lisp-state (current-lisp-state asm-reader))
                      (setf state-stack (state-stack asm-reader))
                      (setf ast-string (ast-string asm-reader))
                      (setf lisp-code-string (lisp-code-string asm-reader))
                      (setf n-lisp-forms (n-lisp-forms asm-reader))
                      (setf is-there-code-on-this-line (is-there-code-on-this-line asm-reader)))
                      ;; temporary code before `asm-reader` is taken fully into use ends here.
                     ;; is character , ?
                     ;; if yes, mark we are in space between parameters, do not output anything.
                     ((equal my-char ",")
                      (setf current-state (pop state-stack)))
                     ;; is character space?
                     ;; if yes, mark we are in space between parameters,  do not output anything.
                     ((equal my-char " ")
                      (setf current-state (pop state-stack)))
                     ;; otherwise produce an error.
                     (t (error "a whitespace is required after closing square bracket"))))
                  ((equal current-state "inside-comment")
                   (cond
                     ;; is character newline?
                     ;; if yes, start a new instruction.
                     ((equal my-char (coerce (list #\Newline) 'string))
                      ;; temporary code before `asm-reader` is taken fully into use.
                      (setf (current-state asm-reader) current-state)
                      (setf (current-lisp-state asm-reader) current-lisp-state)
                      (setf (state-stack asm-reader) state-stack)
                      (setf (ast-string asm-reader) ast-string)
                      (setf (lisp-code-string asm-reader) lisp-code-string)
                      (setf (n-lisp-forms asm-reader) n-lisp-forms)
                      (setf (is-there-code-on-this-line asm-reader) is-there-code-on-this-line)
                      ;; temporary code before `asm-reader` is taken fully into use ends here.
                      (setf asm-reader (new-instruction asm-reader))
                      ;; temporary code before `asm-reader` is taken fully into use.
                      (setf current-state (current-state asm-reader))
                      (setf current-lisp-state (current-lisp-state asm-reader))
                      (setf state-stack (state-stack asm-reader))
                      (setf ast-string (ast-string asm-reader))
                      (setf lisp-code-string (lisp-code-string asm-reader))
                      (setf n-lisp-forms (n-lisp-forms asm-reader))
                      (setf is-there-code-on-this-line (is-there-code-on-this-line asm-reader)))
                      ;; temporary code before `asm-reader` is taken fully into use ends here.
                     ;; otherwise don't output anything.
                     (t nil)))
                  ((equal current-state "slash")
                   (cond
                     ;; is character * ?
                     ;; if yes, mark we are inside C-style comment.
                     ((equal my-char "*")
                      (let
                        ((previous-state (pop state-stack)))
                        ;; is previous state inside instruction?
                        ;; if yes, change it to inside space, output " (to end the instruction).
                        (cond
                          ((equal previous-state "inside-instruction")
                           (push "in-space" state-stack)
                           (setf ast-string (concatenate 'string ast-string "\"")))
                          ;; otherwise keep the previous state as is, do not output anything.
                          (t (push previous-state state-stack))))
                      ;; in any case, mark we are inside C-style comment.
                      (setf current-state "inside-c-comment"))
                     ((equal my-char "/")
                      (let
                        ((previous-state (pop state-stack)))
                        (cond
                          ((equal previous-state "opening-square-bracket")
                           (error "memory address syntax must be terminated with a closing square bracket before a C++-style comment"))
                          ((equal previous-state "inside-memory-address-syntax")
                           (error "memory address syntax must be terminated with a closing square bracket before a C++-style comment"))
                          ((equal previous-state "space-inside-memory-address-syntax")
                           (error "memory address syntax must be terminated with a closing square bracket before a C++-style comment"))
                          ((equal previous-state "plus-inside-memory-address-syntax")
                           (error "memory address syntax must be terminated with a closing square bracket before a C++-style comment"))
                          ;; otherwise mark we're inside a C++-style comment.
                          (t (setf current-state "inside-comment")))))
                     ;; otherwise return to the earlier state, output / and current character.
                     (t (setf current-state (pop state-stack))
                      (setf ast-string (concatenate 'string ast-string "/" my-char)))))
                  ((equal current-state "inside-c-comment")
                   (when
                     ;; is character * ?
                     ;; if yes, mark we have an asterisk inside C-style comment, don't output anything.
                     (equal my-char "*")
                     (setf current-state "asterisk-inside-c-comment")))
                  ((equal current-state "asterisk-inside-c-comment")
                   (cond
                     ;; is character / ?
                     ;; if yes, return to earlier state.
                     ((equal my-char "/")
                      (setf current-state (pop state-stack)))
                     ;; is character _not_ * ?
                     ;; if yes, mark we're inside C-style comment (but not after asterisk), don't output anything.
                     ((not (equal my-char "*"))
                      (setf current-state "inside-c-comment"))
                     ;; otherwise don't output anything.
                     (t nil)))
                  (t (error (concatenate 'string "invalid current-state: " current-state)))))
               ((equal current-mode "Lisp")
                ;; in Lisp mode, read text until #e or #a is reached and eval it.
                (cond
                  ((equal current-lisp-state "hash-sign-read")
                   (setf current-lisp-state "regular")
                   (cond
                     ;; is character e ?
                     ;; if yes, we are done, fix closing parentheses and return.
                     ((equal my-char "e")
                      (setf ast-string (concatenate 'string
                                                   ast-string
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
                                                            ast-string invalid-last-characters)
                                                          invalid-last-characters) ")")))
                     ;; is character a ?
                     ;; if yes, change to asm mode.
                     ((equal my-char "a")
                      (setf current-mode "asm")
                      (setf is-there-code-on-this-line nil)
                      (setf current-state "start-of-line")
                      (setf ast-string (concatenate 'string
                                                   ast-string
                                                   (coerce (list #\Newline) 'string)
                                                   "#a"
                                                   (coerce (list #\Newline) 'string)
                                                   (eval (read-from-string lisp-code-string))
                                                   (coerce (list #\Newline) 'string)
                                                   "#e"
                                                   (coerce (list #\Newline) 'string))))
                     ;; is character l ?
                     ;; if yes, start a new Lisp mode.
                     ((equal my-char "l")
                      (setf is-there-code-on-this-line nil)
                      (setf current-state "start-of-line")
                      (setf ast-string (concatenate 'string
                                                   ast-string
                                                   (coerce (list #\Newline) 'string)
                                                   "#a"
                                                   (coerce (list #\Newline) 'string)
                                                   (eval (read-from-string lisp-code-string))
                                                   (coerce (list #\Newline) 'string)
                                                   "#e"
                                                   (coerce (list #\Newline) 'string)))
                      (setf lisp-code-string ""))
                     ;; otherwise, add # and the character to the Lisp code to be evaluated.
                     (t (setf lisp-code-string (concatenate 'string lisp-code-string "#" my-char)))))
                  ;; is character # ?
                  ;; if yes, mark hash sign read.
                  ((equal current-lisp-state "regular")
                   (cond
                     ((equal my-char "#")
                      (setf current-lisp-state "hash-sign-read"))
                     ;; otherwise add the character to the Lisp code to be evaluated.
                     (t (setf lisp-code-string (concatenate 'string lisp-code-string my-char)))))
                  (t (error (concatenate 'string "invalid current-lisp-state: " current-lisp-state)))))
               (t (error (concatenate 'string "invalid current-mode: " current-mode)))))))

(defun transform-code-to-string-asm (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (transform-code-to-string stream "asm"))

(defun transform-code-to-string-lisp (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (transform-code-to-string stream "Lisp"))

;;; #a is the input which starts the custom reader.
(set-dispatch-macro-character #\# #\a #'transform-code-to-string-asm)
(set-dispatch-macro-character #\# #\l #'transform-code-to-string-lisp)
