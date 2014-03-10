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
                        ((or (equal current-phase "inside-instruction")
                             (equal current-phase "inside-parameters"))

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
                   ;; are we inside memory address syntax? 
                   ;; if yes, don't output anything.
                   ((equal current-phase "inside-memory-address-syntax")
                    (cond
                      ;; is this a space inside memory address syntax?
                      ;; if yes, don't output anything.
                      ((equal my-char " ")
                       nil)
                      ;; is this closing square bracket?
                      ;; if yes, print ]"
                      ((equal my-char "]")
                       (progn
                         (setf current-phase "closing-square-bracket")
                         (setf my-string (concatenate 'string my-string my-char))))
                      ;; otherwise print the character.
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
                   (t (setf my-string (concatenate 'string my-string my-char))))))))
    ;;; #a is the input which starts the custom reader.
    (set-dispatch-macro-character #\# #\a #'transform-code-to-string))

(defun cmpsb-x86 (&rest args)
  (list #xa6))
(defun cmpsw-x86 (&rest args)
  (list #x66 #xa7))
(defun cmpsd-x32-x64 (&rest args)
  (list #xa7))
(defun cmpsq-48-x64 (&rest args)
  (list #x48 #xa7))
(defun cmpsq-49-x64 (&rest args)
  (list #x49 #xa7))
(defun cmpsq-4a-x64 (&rest args)
  (list #x4a #xa7))
(defun cmpsq-4b-x64 (&rest args)
  (list #x4a #xa7))
(defun cmpsq-4c-x64 (&rest args)
  (list #x4c #xa7))
(defun cmpsq-4d-x64 (&rest args)
  (list #x4d #xa7))
(defun cmpsq-4e-x64 (&rest args)
  (list #x4e #xa7))
(defun cmpsq-4f-x64 (&rest args)
  (list #x4f #xa7))
(defun insb-x86 (&rest args)
  (list #x6c))
(defun insw-x86 (&rest args)
  (list #x66 #x6d))
(defun insd-x32-x64 (&rest args)
  (list #x6d))
(defun lodsb-x86 (&rest args)
  (list #xac))
(defun lodsw-x86 (&rest args)
  (list #x66 #xad))
(defun lodsd-x32-x64 (&rest args)
  (list #xad))
(defun lodsq-48-x64 (&rest args)
  (list #x48 #xad))
(defun lodsq-49-x64 (&rest args)
  (list #x49 #xad))
(defun lodsq-4a-x64 (&rest args)
  (list #x4a #xad))
(defun lodsq-4b-x64 (&rest args)
  (list #x4b #xad))
(defun lodsq-4c-x64 (&rest args)
  (list #x4c #xad))
(defun lodsq-4d-x64 (&rest args)
  (list #x4d #xad))
(defun lodsq-4e-x64 (&rest args)
  (list #x4e #xad))
(defun lodsq-4f-x64 (&rest args)
  (list #x4f #xad))
(defun movsb-x86 (&rest args)
  (list #xa4))
(defun movsw-x86 (&rest args)
  (list #x66 #xa5))
(defun movsd-x32-x64 (&rest args)
  (list #xa5))
(defun movsq-48-x64 (&rest args)
  (list #x48 #xa5))
(defun movsq-49-x64 (&rest args)
  (list #x49 #xa5))
(defun movsq-4a-x64 (&rest args)
  (list #x4a #xa5))
(defun movsq-4b-x64 (&rest args)
  (list #x4b #xa5))
(defun movsq-4c-x64 (&rest args)
  (list #x4c #xa5))
(defun movsq-4d-x64 (&rest args)
  (list #x4d #xa5))
(defun movsq-4e-x64 (&rest args)
  (list #x4e #xa5))
(defun movsq-4f-x64 (&rest args)
  (list #x4f #xa5))
(defun outsb-x86 (&rest args)
  (list #x6e))
(defun outsw-x86 (&rest args)
  (list #x66 #x6f))
(defun outsd-x32-x64 (&rest args)
  (list #x6f))
(defun scasb-x86 (&rest args)
  (list #xae))
(defun scasw-x86 (&rest args)
  (list #x66 #xaf))
(defun scasd-x32-x64 (&rest args)
  (list #xaf))
(defun scasq-48-x64 (&rest args)
  (list #x48 #xaf))
(defun scasq-49-x64 (&rest args)
  (list #x49 #xaf))
(defun scasq-4a-x64 (&rest args)
  (list #x4a #xaf))
(defun scasq-4b-x64 (&rest args)
  (list #x4b #xaf))
(defun scasq-4c-x64 (&rest args)
  (list #x4c #xaf))
(defun scasq-4d-x64 (&rest args)
  (list #x4d #xaf))
(defun scasq-4e-x64 (&rest args)
  (list #x4e #xaf))
(defun scasq-4f-x64 (&rest args)
  (list #x4f #xaf))
(defun stosb-x86 (&rest args)
  (list #xaa))
(defun stosw-x86 (&rest args)
  (list #x66 #xab))
(defun stosd-x32-x64 (&rest args)
  (list #xab))
(defun stosq-48-x64 (&rest args)
  (list #x48 #xab))
(defun stosq-49-x64 (&rest args)
  (list #x49 #xab))
(defun stosq-4a-x64 (&rest args)
  (list #x4a #xab))
(defun stosq-4b-x64 (&rest args)
  (list #x4b #xab))
(defun stosq-4c-x64 (&rest args)
  (list #x4c #xab))
(defun stosq-4d-x64 (&rest args)
  (list #x4d #xab))
(defun stosq-4e-x64 (&rest args)
  (list #x4e #xab))
(defun stosq-4f-x64 (&rest args)
  (list #x4f #xab))

(defun in-x32-x64 (arg1 arg2 &rest args)
  (cond
    ((and (equalp arg1 "al") (equalp arg2 "dx"))
     (list #xec))
    ((and (equalp arg1 "ax") (equalp arg2 "dx"))
     (list #x66 #xed))
    ((and (equalp arg1 "eax") (equalp arg2 "dx"))
     (list #xed))
    (t nil)))

(defun out-x32-x64 (arg1 arg2 &rest args)
  (cond
    ((and (equalp arg1 "dx") (equalp arg2 "al"))
     (list #xee))
    ((and (equalp arg1 "dx") (equalp arg2 "ax"))
     (list #x66 #xef))
    ((and (equalp arg1 "dx") (equalp arg2 "eax"))
     (list #xef))
    (t nil)))

(defun rep-repz-x32-x64 (&optional arg1 &rest args)
  (cond
    ((equalp arg1 "cmpsw")
     (list #x66 #xf3 #xa7))
    ((equalp arg1 "insw")
     (list #x66 #xf3 #x6d))
    ((equalp arg1 "lodsw")
     (list #x66 #xf3 #xad))
    ((equalp arg1 "movsw")
     (list #x66 #xf3 #xa5))
    ((equalp arg1 "outsw")
     (list #x66 #xf3 #x6f))
    ((equalp arg1 "scasw")
     (list #x66 #xf3 #xaf))
    ((equalp arg1 "stosw")
     (list #x66 #xf3 #xab))
    ((eq arg1 nil)
     (list #xf3))
    (t (cons #xf3 (funcall (first (gethash arg1 *emit-function-hash-table-x64*)))))))

(defun repnz-x32-x64 (&optional arg1 &rest args)
  (cond
    ((equalp arg1 "cmpsw")
     (list #x66 #xf2 #xa7))
    ((equalp arg1 "insw")
     (list #x66 #xf2 #x6d))
    ((equalp arg1 "lodsw")
     (list #x66 #xf2 #xad))
    ((equalp arg1 "movsw")
     (list #x66 #xf2 #xa5))
    ((equalp arg1 "outsw")
     (list #x66 #xf2 #x6f))
    ((equalp arg1 "scasw")
     (list #x66 #xf2 #xaf))
    ((equalp arg1 "stosw")
     (list #x66 #xf2 #xab))
    ((eq arg1 nil)
     (list #xf2))
    (t (cons #xf2 (funcall (first (gethash arg1 *emit-function-hash-table-x64*)))))))

(defun cs-x86 (&rest args)
  (list #x2e))
(defun ds-x86 (&rest args)
  (list #x3e))
(defun es-x86 (&rest args)
  (list #x26))
(defun fs-x86 (&rest args)
  (list #x64))
(defun gs-x86 (&rest args)
  (list #x65))
(defun ss-x86 (&rest args)
  (list #x36))
(defun clc-x86 (&rest args)
  (list #xf8))
(defun cld-x86 (&rest args)
  (list #xfc))
(defun cli-x86 (&rest args)
  (list #xfa))
(defun cmc-x86 (&rest args)
  (list #xf5))
(defun hlt-x86 (&rest args)
  (list #xf4))
(defun nop-x86 (&rest args)
  (list #x90))
(defun stc-x86 (&rest args)
  (list #xf9))
(defun std-x86 (&rest args)
  (list #xfd))
(defun sti-x86 (&rest args)
  (list #xfb))

(defparameter *emit-function-hash-table-x64* (make-hash-table :test 'equalp))
;;; segment registers.
(setf (gethash "cs:"   *emit-function-hash-table-x64*) (list #'cs-x86))
(setf (gethash "ds:"   *emit-function-hash-table-x64*) (list #'ds-x86))
(setf (gethash "es:"   *emit-function-hash-table-x64*) (list #'es-x86))
(setf (gethash "fs:"   *emit-function-hash-table-x64*) (list #'fs-x86))
(setf (gethash "gs:"   *emit-function-hash-table-x64*) (list #'gs-x86))
(setf (gethash "ss:"   *emit-function-hash-table-x64*) (list #'ss-x86))
;;; instructions in alphabetical order.
(setf (gethash "clc"   *emit-function-hash-table-x64*) (list #'clc-x86))
(setf (gethash "cld"   *emit-function-hash-table-x64*) (list #'cld-x86))
(setf (gethash "cli"   *emit-function-hash-table-x64*) (list #'cli-x86))
(setf (gethash "cmc"   *emit-function-hash-table-x64*) (list #'cmc-x86))
(setf (gethash "cmpsb" *emit-function-hash-table-x64*) (list #'cmpsb-x86))
(setf (gethash "cmpsd" *emit-function-hash-table-x64*) (list #'cmpsd-x32-x64))
(setf (gethash "cmpsq" *emit-function-hash-table-x64*) (list #'cmpsq-48-x64 #'cmpsq-49-x64 #'cmpsq-4a-x64 #'cmpsq-4b-x64 #'cmpsq-4c-x64 #'cmpsq-4d-x64 #'cmpsq-4e-x64 #'cmpsq-4f-x64))
(setf (gethash "cmpsw" *emit-function-hash-table-x64*) (list #'cmpsw-x86))
(setf (gethash "hlt"   *emit-function-hash-table-x64*) (list #'hlt-x86))
(setf (gethash "in"    *emit-function-hash-table-x64*) (list #'in-x32-x64))
(setf (gethash "insb"  *emit-function-hash-table-x64*) (list #'insb-x86))
(setf (gethash "insd"  *emit-function-hash-table-x64*) (list #'insd-x32-x64))
(setf (gethash "insw"  *emit-function-hash-table-x64*) (list #'insw-x86))
(setf (gethash "lodsb" *emit-function-hash-table-x64*) (list #'lodsb-x86))
(setf (gethash "lodsd" *emit-function-hash-table-x64*) (list #'lodsd-x32-x64))
(setf (gethash "lodsq" *emit-function-hash-table-x64*) (list #'lodsq-48-x64 #'lodsq-49-x64 #'lodsq-4a-x64 #'lodsq-4b-x64 #'lodsq-4c-x64 #'lodsq-4d-x64 #'lodsq-4e-x64 #'lodsq-4f-x64))
(setf (gethash "lodsw" *emit-function-hash-table-x64*) (list #'lodsw-x86))
(setf (gethash "nop"   *emit-function-hash-table-x64*) (list #'nop-x86))
(setf (gethash "rep"   *emit-function-hash-table-x64*) (list #'rep-repz-x32-x64))
(setf (gethash "repe"  *emit-function-hash-table-x64*) (list #'rep-repz-x32-x64))
(setf (gethash "repne" *emit-function-hash-table-x64*) (list #'repnz-x32-x64))
(setf (gethash "repnz" *emit-function-hash-table-x64*) (list #'repnz-x32-x64))
(setf (gethash "repz"  *emit-function-hash-table-x64*) (list #'rep-repz-x32-x64))
(setf (gethash "out"   *emit-function-hash-table-x64*) (list #'out-x32-x64))
(setf (gethash "outsb" *emit-function-hash-table-x64*) (list #'outsb-x86))
(setf (gethash "outsd" *emit-function-hash-table-x64*) (list #'outsd-x32-x64))
(setf (gethash "outsw" *emit-function-hash-table-x64*) (list #'outsw-x86))
(setf (gethash "scasb" *emit-function-hash-table-x64*) (list #'scasb-x86))
(setf (gethash "scasd" *emit-function-hash-table-x64*) (list #'scasd-x32-x64))
(setf (gethash "scasq" *emit-function-hash-table-x64*) (list #'scasq-48-x64 #'scasq-49-x64 #'scasq-4a-x64 #'scasq-4b-x64 #'scasq-4c-x64 #'scasq-4d-x64 #'scasq-4e-x64 #'scasq-4f-x64))
(setf (gethash "scasw" *emit-function-hash-table-x64*) (list #'scasw-x86))
(setf (gethash "stc"   *emit-function-hash-table-x64*) (list #'stc-x86))
(setf (gethash "std"   *emit-function-hash-table-x64*) (list #'std-x86))
(setf (gethash "sti"   *emit-function-hash-table-x64*) (list #'sti-x86))
(setf (gethash "stosb" *emit-function-hash-table-x64*) (list #'stosb-x86))
(setf (gethash "stosd" *emit-function-hash-table-x64*) (list #'stosd-x32-x64))
(setf (gethash "stosq" *emit-function-hash-table-x64*) (list #'stosq-48-x64 #'stosq-49-x64 #'stosq-4a-x64 #'stosq-4b-x64 #'stosq-4c-x64 #'stosq-4d-x64 #'stosq-4e-x64 #'stosq-4f-x64))
(setf (gethash "stosw" *emit-function-hash-table-x64*) (list #'stosw-x86))
(setf (gethash "movsb" *emit-function-hash-table-x64*) (list #'movsb-x86))
(setf (gethash "movsd" *emit-function-hash-table-x64*) (list #'movsd-x32-x64))
(setf (gethash "movsq" *emit-function-hash-table-x64*) (list #'movsq-48-x64 #'movsq-49-x64 #'movsq-4a-x64 #'movsq-4b-x64 #'movsq-4c-x64 #'movsq-4d-x64 #'movsq-4e-x64 #'movsq-4f-x64))
(setf (gethash "movsw" *emit-function-hash-table-x64*) (list #'movsw-x86))

(defparameter *modrm-reg-hash-table-x64* (make-hash-table :test 'equalp))
(setf (gethash "al"   *modrm-reg-hash-table*) #b000)
(setf (gethash "cl"   *modrm-reg-hash-table*) #b001)
(setf (gethash "dl"   *modrm-reg-hash-table*) #b010)
(setf (gethash "bl"   *modrm-reg-hash-table*) #b011)
(setf (gethash "ah"   *modrm-reg-hash-table*) #b100)
(setf (gethash "ch"   *modrm-reg-hash-table*) #b101)
(setf (gethash "dh"   *modrm-reg-hash-table*) #b110)
(setf (gethash "bh"   *modrm-reg-hash-table*) #b111)
(setf (gethash "ax"   *modrm-reg-hash-table*) #b000)
(setf (gethash "cx"   *modrm-reg-hash-table*) #b001)
(setf (gethash "dx"   *modrm-reg-hash-table*) #b010)
(setf (gethash "bx"   *modrm-reg-hash-table*) #b011)
(setf (gethash "sp"   *modrm-reg-hash-table*) #b100)
(setf (gethash "bp"   *modrm-reg-hash-table*) #b101)
(setf (gethash "si"   *modrm-reg-hash-table*) #b110)
(setf (gethash "di"   *modrm-reg-hash-table*) #b111)
(setf (gethash "eax"  *modrm-reg-hash-table*) #b000)
(setf (gethash "ecx"  *modrm-reg-hash-table*) #b001)
(setf (gethash "edx"  *modrm-reg-hash-table*) #b010)
(setf (gethash "ebx"  *modrm-reg-hash-table*) #b011)
(setf (gethash "esp"  *modrm-reg-hash-table*) #b100)
(setf (gethash "ebp"  *modrm-reg-hash-table*) #b101)
(setf (gethash "esi"  *modrm-reg-hash-table*) #b110)
(setf (gethash "edi"  *modrm-reg-hash-table*) #b111)
(setf (gethash "rax"  *modrm-reg-hash-table*) #b000)
(setf (gethash "rcx"  *modrm-reg-hash-table*) #b001)
(setf (gethash "rdx"  *modrm-reg-hash-table*) #b010)
(setf (gethash "rbx"  *modrm-reg-hash-table*) #b011)
(setf (gethash "rsp"  *modrm-reg-hash-table*) #b100)
(setf (gethash "rbp"  *modrm-reg-hash-table*) #b101)
(setf (gethash "rsi"  *modrm-reg-hash-table*) #b110)
(setf (gethash "rdi"  *modrm-reg-hash-table*) #b111)
(setf (gethash "mmx0" *modrm-reg-hash-table*) #b000)
(setf (gethash "mmx1" *modrm-reg-hash-table*) #b001)
(setf (gethash "mmx2" *modrm-reg-hash-table*) #b010)
(setf (gethash "mmx3" *modrm-reg-hash-table*) #b011)
(setf (gethash "mmx4" *modrm-reg-hash-table*) #b100)
(setf (gethash "mmx5" *modrm-reg-hash-table*) #b101)
(setf (gethash "mmx6" *modrm-reg-hash-table*) #b110)
(setf (gethash "mmx7" *modrm-reg-hash-table*) #b111)
(setf (gethash "xmm0" *modrm-reg-hash-table*) #b000)
(setf (gethash "xmm1" *modrm-reg-hash-table*) #b001)
(setf (gethash "xmm2" *modrm-reg-hash-table*) #b010)
(setf (gethash "xmm3" *modrm-reg-hash-table*) #b011)
(setf (gethash "xmm4" *modrm-reg-hash-table*) #b100)
(setf (gethash "xmm5" *modrm-reg-hash-table*) #b101)
(setf (gethash "xmm6" *modrm-reg-hash-table*) #b110)
(setf (gethash "xmm7" *modrm-reg-hash-table*) #b111)
(setf (gethash "ymm0" *modrm-reg-hash-table*) #b000)
(setf (gethash "ymm1" *modrm-reg-hash-table*) #b001)
(setf (gethash "ymm2" *modrm-reg-hash-table*) #b010)
(setf (gethash "ymm3" *modrm-reg-hash-table*) #b011)
(setf (gethash "ymm4" *modrm-reg-hash-table*) #b100)
(setf (gethash "ymm5" *modrm-reg-hash-table*) #b101)
(setf (gethash "ymm6" *modrm-reg-hash-table*) #b110)
(setf (gethash "ymm7" *modrm-reg-hash-table*) #b111)

(defparameter *sib-scale-hash-table-x64* (make-hash-table :test 'equalp))
(setf (gethash "1" *modrm-reg-hash-table*) #b00)
(setf (gethash "2" *modrm-reg-hash-table*) #b01)
(setf (gethash "4" *modrm-reg-hash-table*) #b10)
(setf (gethash "8" *modrm-reg-hash-table*) #b11)

(defparameter *sreg2-hash-table-x64* (make-hash-table :test 'equalp))
(setf (gethash "es" *sreg2-hash-table-x64*) #b00)
(setf (gethash "cs" *sreg2-hash-table-x64*) #b01)
(setf (gethash "ss" *sreg2-hash-table-x64*) #b10)
(setf (gethash "ds" *sreg2-hash-table-x64*) #b11)

(defparameter *old-low-reg8-list*  (list  "al"  "cl"  "dl"  "bl"))
(defparameter *old-high-reg8-list* (list  "ah"  "ch"  "dh"  "bh"))
(defparameter *new-low-reg8-list*  (list "spl" "bpl" "sil" "dil" "r8l" "r9l" "r10l" "r11l" "r12l" "r13l" "r14l" "r15l"))
(defparameter *old-reg8-list* (append *old-low-reg8-list* *old-high-reg8-list*))
(defparameter *low-reg8-list* (append *old-low-reg8-list* *new-low-reg8-list*))
(defparameter *reg8-list* (append *old-low-reg8-list* *old-high-reg8-list* *new-low-reg8-list*))

(defparameter *old-reg16-list* (list  "ax"  "cx"   "dx"   "bx"   "sp"   "bp"   "si"   "di"))
(defparameter *new-reg16-list* (list "r8w" "r9w" "r10w" "r11w" "r12w" "r13w" "r14w" "r15w"))
(defparameter *reg16-list* (append *old-reg16-list* *new-reg16-list*))

(defparameter *old-reg32-list* (list "eax" "ecx"  "edx"  "ebx"  "esp"  "ebp"  "esi"  "edi"))
(defparameter *new-reg32-list* (list "r8d" "r9d" "r10d" "r11d" "r12d" "r13d" "r14d" "r15d"))
(defparameter *reg32-list* (append *old-reg32-list* *new-reg32-list*))

(defparameter *old-reg64-list* (list "rax" "rcx" "rdx" "rbx" "rsp" "rbp" "rsi" "rdi"))
(defparameter *new-reg64-list* (list  "r8"  "r9" "r10" "r11" "r12" "r13" "r14" "r15"))
(defparameter *reg64-list* (append *old-reg64-list* *new-reg64-list*))

(defparameter *test-code-x64*
  #a
  cs:
  ds:
  es:
  fs:
  gs:
  ss:
  clc
  cld
  cli
  cmc
  cmpsb
  cmpsd
  cmpsq
  cmpsw
  hlt
  in    al,dx
  in    ax,dx
  in    eax,dx
  insb
  insd
  insw
  lodsb
  lodsd
  lodsq
  lodsw
  nop
  out   dx,al
  out   dx,ax
  out   dx,eax
  outsb
  outsd
  outsw
  rep
  rep   cmpsb
  rep   cmpsd
  rep   cmpsq
  rep   cmpsw
  rep   insb
  rep   insd
  rep   insw
  rep   lodsb
  rep   lodsd
  rep   lodsq
  rep   lodsw
  rep   movsb
  rep   movsd
  rep   movsq
  rep   movsw
  rep   outsb
  rep   outsd
  rep   outsw
  rep   scasb
  rep   scasd
  rep   scasq
  rep   scasw
  rep   stosb
  rep   stosd
  rep   stosq
  rep   stosw
  repnz cmpsb
  repnz cmpsd
  repnz cmpsq
  repnz cmpsw
  repnz insb
  repnz insd
  repnz insw
  repnz lodsb
  repnz lodsd
  repnz lodsq
  repnz lodsw
  repnz movsb
  repnz movsd
  repnz movsq
  repnz movsw
  repnz outsb
  repnz outsd
  repnz outsw
  repnz scasb
  repnz scasd
  repnz scasq
  repnz scasw
  repnz stosb
  repnz stosd
  repnz stosq
  repnz stosw
  scasb
  scasd
  scasq
  scasw
  stc
  std
  sti
  stosb
  stosd
  stosq
  stosw
  movsb
  movsd
  movsq
  movsw
  #)

(defparameter *example-code-x64*
  #a
  mul   rax           ; rdx:rax = rax^2.
  (
   mov   rbp,rsp        ; create the stack frame
   lea   rdi,[ rbx + 4*rax + testmsg1 ] ; load effective address.
   )
  #)

(defparameter *example-code-cli-sti*
  #a
  cli
  sti
  #)

(defparameter *alt-mov-reg64-reg64-push-pop*
  ;; clean, does not modify flags.
  #a
  push  arg2
  pop   arg1
  #)

(defparameter *alt-mov-reg64-reg64-lea*
  ;; clean, does not modify flags.
  #a
  lea   arg1,[arg2]
  #)

(defparameter *alt-add-reg64-reg64-lea*
  ;; clean, does not modify flags.
  ;; overflow?
  #a
  lea   arg1,[arg1+arg1]
  #)

(defparameter *alt-xchg-reg64-reg64-push-pop-1*
  ;; clean, does not modify flags.
  #a
  push  arg1
  push  arg2
  pop   arg1
  pop   arg2
  #)

(defparameter *alt-xchg-reg64-reg64-push-pop-2*
  ;; clean, does not modify flags.
  #a
  push  arg2
  push  arg1
  pop   arg2
  pop   arg1
  #)

(defparameter *alt-zero-reg-xor*
  ;; modifies flags.
  #a
  xor   arg1,arg1
  #)

(defparameter *alt-zero-reg-sub*
  ;; modifies flags.
  #a
  sub   arg1,arg1
  #)

(defparameter *alt-add-reg-reg-adc*
  ;; clean, modifies flags identically.
  #a
  clc
  adc   arg1,arg2
  #)

(defparameter *alt-sub-reg-reg-sbb*
  ;; clean, modifies flags identically.
  #a
  clc
  sbb   arg1,arg2
  #)

(defparameter *alt-cmp-reg64-any-push-sub-pop*
  ;; clean, modifies flags identically.
  #a
  push  arg1
  sub   arg1,arg2
  pop   arg1
  #)

(defparameter *alt-or-and-reg-reg-test*
  #a
  test  arg1,arg1
  #)

(defparameter *alt-test-reg-reg-or*
  #a
  or    arg1,arg1
  #)

(defparameter *alt-test-reg-reg-and*
  #a
  and   arg1,arg1
  #)

(defun create-syntax-tree (my-string)
  "This function converts a string produced by transform-code-to-string into a syntax tree."
  (read-from-string my-string))

(defun emit-binary-code-list (syntax-tree my-hash-table)
  "This function converts syntax tree to a list of lists of binary code bytes,
   the bytes of each instruction are on their list."
  (mapcar #'(lambda (x)
              (apply
                (first (gethash (first x) my-hash-table))
                (rest x)))
          (eval syntax-tree)))

(defun emit-binary-code (syntax-tree my-hash-table)
  "This function produces a single list of binary code bytes."
  (apply #'append (emit-binary-code-list syntax-tree my-hash-table)))

(defun emit-binary-code-and-print-hex (syntax-tree my-hash-table)
  "This function converts syntax tree to a string of hexadecimal bytes."
  (print-hex (emit-binary-code syntax-tree my-hash-table)))

(defun assemble (code my-hash-table)
  "This function assembles code."
  (emit-binary-code (create-syntax-tree code) my-hash-table))

(defun assemble-and-print-hex (code my-hash-table)
  "This function assembles code and prints in a hexadecimal string."
  (print-hex (assemble code my-hash-table)))

(defun assemble-x64 (code)
  "This function assembles x86-64 (x64) code."
  (assemble code *emit-function-hash-table-x64*))

(defun assemble-x64-and-print-hex (code)
  "This function assembles x86-64 (x64) code and prints in a hexadecimal string."
  (print-hex (assemble code *emit-function-hash-table-x64*)))

(defun print-hex (my-number)
  (format nil "~x" my-number))

(defun print-hex-list (my-list)
  (mapcar #'(lambda (x) (print-hex x)) my-list))

(defun string-to-function (my-string)
  "This fnuction converts a string to a function.
   http://stackoverflow.com/questions/2940267/call-function-based-on-a-string/2940347#2940347"
  (symbol-function (find-symbol (string-upcase my-string))))
