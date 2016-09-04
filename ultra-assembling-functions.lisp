;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

;;; call tree for emit-code functions:
;;;
;;; function/macro                                          type of input                   type of output                      comments
;;; 1 `defun assemble-x64`/`defun assemble-arm` etc.        `code`                          single list of binary code bytes    architecture-specific wrapper function.
;;;   2 `defun assemble`                                    `code`, `my-hash-table`         single list of binary code bytes    simple wrapper function.
;;;     3 `defun emit-binary-code`                          `syntax-tree`, `my-hash-table`  single list of binary code bytes    appends binary code by `apply #'append`.
;;;       4 `defun emit-binary-code-list`                   `syntax-tree`, `my-hash-table`  list of lists of binary code bytes, `mapcar #'(lambda (x) ` ...
;;;                                                                                         one list for each instruction.      `(emit-binary-code-for-one-instruction`
;;;         5 `defun emit-binary-code-for-one-instruction`  `syntax-list`, `my-hash-table`  list of lists of binary code bytes  calls `emit` for each
;;;                                                                                                                             `instruction-instance` inside
;;;                                                                                                                             `loop` ... `collect`, then uses
;;;                                                                                                                             `reduce` ...  `funcall` with
;;;                                                                                                                             `emit-function-selector-function-list`
;;;                                                                                                                             to select the requested encoding of
;;;                                                                                                                             each instruction.
;;;
;;; function/macro                                          type of input                   type of output                      comments
;;; 1 `defun assemble-alternatives-x64` etc.                `code`                          a list of lists of lists of         architecture-specific
;;;                                                                                         binary code bytes.
;;;                                                                                         outermost list contains a list for
;;;                                                                                         each instruction.
;;;                                                                                         each instruction's own list
;;;                                                                                         contains a list for each encoding.
;;;   2 `defun assemble-alternatives`                       `code`, `my-hash-table`         a list of lists of lists of         removes duplicate encodings.
;;;                                                                                         binary code bytes (same as above).
;;;     3 `defun get-all-encodings-for-syntax-tree`         `syntax-tree`, `my-hash-table`  a list of lists of lists of         calls
;;;                                                                                         binary code bytes.                  `emit-all-encodings-for-one-instruction`
;;;                                                                                                                             inside `loop` ... `collect`, then
;;;                                                                                                                             removes `nil` encodings.
;;;       4 `defun emit-all-encodings-for-one-instruction`  `syntax-list`, `my-hash-table`  list of lists of binary code bytes  calls `emit` for each
;;;                                                                                                                             `instruction-instance` inside
;;;                                                                                                                             `loop` ... `collect`.
;;;
;;; assembling modes
;;; #1 simple               functions as a regular assembler.
;;; #2 steganographic       encodes given message into binary code according to given steganographic encoding flags/rules.
;;; #3 constraint-based     uses constraint-based programming in binary code generation.

(in-package :ultraelf)

(defparameter *global-offset* 0)
(defparameter $ 0)

(defun create-syntax-tree (my-list)
  "This recursive function converts a string produced by transform-code-to-string into a syntax tree.
   Input argument my-list can be a string, in that case it is first stored into a list.
   usage examples:
   X64> (create-syntax-tree #a nop #e)
   (LIST '(\"nop\"))
   X64> (create-syntax-tree #a mov ax,bx #a shl bx,1 #e)
   (LIST '(\"mov\" \"ax\" \"bx\") '(\"shl\" \"bx\" \"1\"))
   The syntax tree can be `eval`ed:
   X64> (eval (create-syntax-tree #a nop #e))
   ((\"nop\"))
   X64> (eval (create-syntax-tree #a mov ax,bx #a shl bx,1 #e))
   ((\"mov\" \"ax\" \"bx\") (\"shl\" \"bx\" \"1\"))
   To get all encodings of each instruction of a given syntax tree, use function `get-all-encodings-for-syntax-tree`.
   To get one encoding for each instruction of a given syntax tree, use function `emit-binary-code-list`."
  (unless (listp my-list)
    (setf my-list (list my-list)))
  ;; is last element of the list a string?
  ;; if yes, read it to convert it to a list and append the resulting list to earlier elements.
  ;; otherwise use CONS to create (LIST element-1 element-2 ... ) .
  (loop for i below (length my-list)
        do (when (stringp (nth i my-list))
             (if (eql i 0)
               (setf my-list (create-syntax-tree (append
                                                   (rest (read-from-string (nth i my-list)))
                                                   (subseq my-list (1+ i)))))
               (setf my-list (create-syntax-tree (append
                                                   (subseq my-list 0 i)
                                                   (rest (read-from-string (nth i my-list)))
                                                   (subseq my-list (1+ i))))))))
  (if (eql (first my-list) 'list)
    my-list
    (cons 'list my-list)))

(defun create-and-eval-syntax-tree (my-list)
  (eval (create-syntax-tree my-list)))

(defun convert-string-to-symbol-if-symbol-exists (my-string)
  "This function converts a string into a symbol, if such symbol exists.
   Otherwise this function converts the string into an instance of `unknown` class."
  (cond
    ((numberp my-string)
     ;; An immediate (a number), probably produced by `eval`.
     (make-instance 'immediate :name (write-to-string my-string) :value my-string))
    ((equal my-string "$")
     ;; An immediate (current address).
     (make-instance 'immediate :name (write-to-string *global-offset*) :value *global-offset*))
    ((and
       (equal (coerce (list (elt my-string  0)) 'string) "(")
       (equal (coerce (list (elt my-string  (1- (length my-string )))) 'string) ")"))
     ;; A string with a Lisp form to be evaluated.
     (convert-string-to-symbol-if-symbol-exists (eval (read-from-string my-string))))
    ((boundp (intern (string-upcase my-string)))
     ;; An existing symbol (such as a mnemonic or a register).
     (symbol-value (intern (string-upcase my-string))))
    ;; Something else. For now, we'll assume that it's a number.
    (t (make-instance 'immediate :name my-string :value (parse-number my-string)))))

(defun emit-all-binary-codes-for-one-instruction (syntax-list my-hash-table &key (prefix-list nil) (skip-errors t))
  "This function converts a syntax list of one instruction to a list of lists of binary code bytes,
   `emit-function-selector-function` can be eg. `#'first` or `#'(lambda (x) (first (last x)))`."
  (let*
    ((instruction-instances-list (gethash (first syntax-list) my-hash-table))
     (binary-code
       ;; call each function of `emit-function-selector-function`, beginning from the leftmost function.
       ;; this is usually used to select or filter encodings.
       ;; eliminate duplicate encodings.
       (remove-duplicates
         ;; eliminate invalid encodings (`nil`).
         (remove nil
                 ;; attempt encoding with each instruction instance.
                 (loop for instruction-instance in instruction-instances-list
                       ;; append all encodings to a list.
                       append (cond
                                ((equal (first (code-format instruction-instance)) "prefix")
                                 ;; this is a prefix, such as `rep`/`repe`/`repz` or `repne`/`repnz`.
                                 (emit-all-binary-codes-for-one-instruction
                                   (rest syntax-list)
                                   my-hash-table
                                   :prefix-list (append prefix-list (list (parse-number (second (code-format instruction-instance)))))
                                   :skip-errors skip-errors))
                                ;; encoding with error handling.
                                (skip-errors (handler-case
                                               ;; call `emit` method of the instruction instance ...
                                               (funcall #'emit
                                                        instruction-instance
                                                        ;; current prefix-list.
                                                        prefix-list
                                                        ;; ... convert each argument string to a symbol,
                                                        ;; if such a symbol exists, and give the list
                                                        ;; of these symbols as an argument to the `emit` method.
                                                        (loop for arg in (rest syntax-list)
                                                              collect (convert-string-to-symbol-if-symbol-exists arg)))
                                               ;; if `common-lisp:simple-error` is produced, return `nil`.
                                               (common-lisp:simple-error ()
                                                                         nil)))
                                ;; encoding without error handling.
                                (t (funcall #'emit
                                            ;; call `emit` method of the instruction instance ...
                                            instruction-instance
                                            ;; current prefix-list.
                                            prefix-list
                                            ;; ... convert each argument string to a symbol,
                                            ;; if such a symbol exists, and give the list
                                            ;; of these symbols as an argument to the `emit` method.
                                            (loop for arg in (rest syntax-list)
                                                  collect (convert-string-to-symbol-if-symbol-exists arg)))))))
         :test #'equal)))
    (when (boundp '*global-offset*)
      (progn
        (incf *global-offset* (length binary-code))
        (setf $ *global-offset*)))
    binary-code))

(defun emit-binary-code-for-one-instruction
  (syntax-list my-hash-table &key (prefix-list nil) (emit-function-selector-function (list #'sort-sublists-shortest-first #'first)) (skip-errors t))
  "This function converts a syntax list of one instruction to a list of lists of binary code bytes,
   `emit-function-selector-function` can be eg. `#'first` or `#'(lambda (x) (first (last x)))`."
  (let*
    ((instruction-instances-list (gethash (first syntax-list) my-hash-table))
     (emit-function-selector-function-list (if (listp emit-function-selector-function)
                                             emit-function-selector-function
                                             (list emit-function-selector-function)))
     (binary-code
       ;; call each function of `emit-function-selector-function`, beginning from the leftmost function.
       ;; this is usually used to select or filter encodings.
       (reduce #'funcall
               (nreverse emit-function-selector-function-list)
               :from-end t
               :initial-value
               ;; eliminate duplicate encodings.
               (remove-duplicates
                 ;; eliminate invalid encodings (`nil`).
                 (remove nil
                         ;; attempt encoding with each instruction instance.
                         (loop for instruction-instance in instruction-instances-list
                               ;; collect all encodings to a list.
                               collect (cond
                                         ;; this is a prefix, such as `rep`/`repe`/`repz` or `repne`/`repnz`.
                                         ((and
                                            (equal (first (code-format instruction-instance)) "prefix")
                                            (not (null (rest syntax-list))))
                                          (emit-binary-code-for-one-instruction
                                            (rest syntax-list)
                                            my-hash-table
                                            :prefix-list (append prefix-list (list (parse-number (second (code-format instruction-instance)))))
                                            :emit-function-selector-function emit-function-selector-function
                                            :skip-errors skip-errors))
                                         ((equal (first (code-format instruction-instance)) "prefix")
                                          ;; no other mnemonics after this prefix.
                                          ;; just return the prefix list.
                                          (append prefix-list (list (parse-number (second (code-format instruction-instance))))))
                                         ;; encoding with error handling.
                                         (skip-errors (handler-case
                                                        ;; call `emit` method of the instruction instance ...
                                                        (append prefix-list (get-list (funcall #'emit
                                                                                               instruction-instance
                                                                                               ;; current prefix-list.
                                                                                               prefix-list
                                                                                               ;; ... convert each argument string to a symbol,
                                                                                               ;; if such a symbol exists, and give the list
                                                                                               ;; of these symbols as an argument to the `emit` method.
                                                                                               (loop for arg in (rest syntax-list)
                                                                                                     collect (convert-string-to-symbol-if-symbol-exists arg)))))
                                                        ;; if `common-lisp:simple-error` is produced, return `nil`.
                                                        (common-lisp:simple-error ()
                                                                                  nil)))
                                         ;; encoding without error handling.
                                         (t (append prefix-list (get-list (funcall #'emit
                                                                                   ;; call `emit` method of the instruction instance ...
                                                                                   instruction-instance
                                                                                   ;; current prefix-list.
                                                                                   prefix-list
                                                                                   ;; ... convert each argument string to a symbol,
                                                                                   ;; if such a symbol exists, and give the list
                                                                                   ;; of these symbols as an argument to the `emit` method.
                                                                                   (loop for arg in (rest syntax-list)
                                                                                         collect (convert-string-to-symbol-if-symbol-exists arg)))))))))
                 :test #'equal))))
    (when (boundp '*global-offset*)
      (progn
        (incf *global-offset* (length binary-code))
        (setf $ *global-offset*)))
    binary-code))

(defun get-nth (n)
  "This function returns a function that gets the nth value of a list."
  #'(lambda (my-list) (nth n my-list)))

(defun get-all-encodings-for-syntax-tree (syntax-tree my-hash-table &key (skip-errors t) (zero-global-offset t))
  "This function converts syntax tree to a list of lists of lists of binary code bytes,
   the encodings of each instruction on their own list,
   the bytes of each encoding on their own list.
   Please note that by default `*global-offset*` and `$` are zeroed after each instruction variant."
  (let
    ((syntax-list-of-lists syntax-tree))
    (mapcar #'(lambda (syntax-list)
                ;; eliminate duplicate encodings.
                (remove-duplicates
                  ;; eliminate invalid encodings (`nil`).
                  (remove nil
                          (loop for emit-function-i below (length (gethash (first syntax-list) my-hash-table))
                                append (progn
                                         (cond (zero-global-offset
                                                 (setf *global-offset* 0)
                                                 (setf $ 0)))
                                         (emit-all-binary-codes-for-one-instruction
                                           syntax-list
                                           my-hash-table
                                           :skip-errors skip-errors))))
                  :test #'equal))
            syntax-list-of-lists)))

(defun get-all-encodings-for-syntax-tree-and-print-hex (syntax-tree my-hash-table &key (skip-errors t))
  "This function converts syntax tree to a list of strings of hexadecimal bytes."
  (print-hex (get-all-encodings-for-syntax-tree syntax-tree my-hash-table :skip-errors skip-errors)))

(defun emit-binary-code-list (syntax-tree my-hash-table &key (emit-function-selector-function (list #'sort-sublists-shortest-first #'first)) (skip-errors t))
  "This function converts syntax tree to a list of lists of binary code bytes,
   the bytes of each instruction are on their own list.
   `emit-function-selector-function` can be eg. `#'first` or `#'(lambda (x) (first (last x)))`.
   TODO: Fix bug in this function!"
  (mapcar #'get-list
          (mapcar #'(lambda (x) ; Works, but the lists are erroneous, `mapcar #'get-list` is used to fix it.
                      (emit-binary-code-for-one-instruction
                        x
                        my-hash-table
                        :emit-function-selector-function emit-function-selector-function
                        :skip-errors skip-errors))
                  syntax-tree)))

(defun emit-binary-code (syntax-tree my-hash-table &key (emit-function-selector-function (list #'sort-sublists-shortest-first #'first)) (skip-errors t))
  "This function produces a single list of binary code bytes."
  (setf *global-offset* 0)
  (setf $ 0)
  (apply #'append (emit-binary-code-list
                    syntax-tree
                    my-hash-table
                    :emit-function-selector-function emit-function-selector-function
                    :skip-errors skip-errors)))

(defun emit-binary-code-and-print-hex (syntax-tree my-hash-table &key (emit-function-selector-function (list #'sort-sublists-shortest-first #'first)) (skip-errors t))
  "This function converts syntax tree to a string of hexadecimal bytes."
  (print-hex (emit-binary-code syntax-tree my-hash-table :emit-function-selector-function emit-function-selector-function :skip-errors skip-errors)))

(defun assemble (code my-hash-table &key (emit-function-selector-function (list #'sort-sublists-shortest-first #'first)) (skip-errors t))
  "This function assembles code."
  (emit-binary-code
    (create-and-eval-syntax-tree code)
    my-hash-table
    :emit-function-selector-function emit-function-selector-function
    :skip-errors skip-errors))

(defun assemble-and-print-hex (code my-hash-table &key (emit-function-selector-function (list #'sort-sublists-shortest-first #'first)) (skip-errors t))
  "This function assembles code and prints in a hexadecimal string."
  (print-hex (assemble
               code my-hash-table
               :emit-function-selector-function emit-function-selector-function
               :skip-errors skip-errors)))

(defun assemble-alternatives (code my-hash-table &key (skip-errors t) (zero-global-offset t))
  "This function assembles code, all alternatives.
   Duplicates are removed.
   Please note that by default `*global-offset*` and `$` are zeroed after each instruction variant."
  (mapcar #'(lambda (x)
              (remove-duplicates x :test #'equal))
          (get-all-encodings-for-syntax-tree
            (create-and-eval-syntax-tree code)
            my-hash-table
            :skip-errors skip-errors
            :zero-global-offset zero-global-offset)))

(defun assemble-alternatives-and-print-hex (code my-hash-table &key (skip-errors t) (zero-global-offset t))
  "This function assembles code, all alternatives, and prints in a hexadecimal string.
   Please note that by default `*global-offset*` and `$` are zeroed after each instruction variant."
  (print-hex
    (get-all-encodings-for-syntax-tree
      (create-and-eval-syntax-tree code)
      my-hash-table
      :skip-errors skip-errors
      :zero-global-offset zero-global-offset)))

(defun string-to-function (my-string)
  "This function converts a string to a function.
   http://stackoverflow.com/questions/2940267/call-function-based-on-a-string/2940347#2940347"
  (symbol-function (find-symbol (string-upcase my-string))))

(defun check-args (req-operands given-operands)
  "This functions checks that given operands match required operands."
  (cond
    ((and
       (eql (length req-operands) 1)
       (equal (first req-operands) "void"))
     (unless (null given-operands)
       (error "void operand type requires exactly 0 input arguments.")))
    ((eql (length req-operands) (length given-operands))
     (loop for i below (length req-operands)
           if (notany #'(lambda (x)
                          (equal x (nth i req-operands)))
                      (allowed-targets (nth i given-operands)))
           do (error "instruction's and operand's allowed targets do not match.")))
    (t (error "number of required operands and number of given arguments do not match."))))

(defun get-msg-bit (msg msg-i &key (filler 0))
  "This function returns the bit 0 (the lowest bit) of the corresponding element.
   `msg` should be a sequence of zeros and ones.
   Bits higher than bit 0 of `msg` are ignored.
   If `(nth msg-i msg)` is `nil`, then `filler` is returned."
  (let
    ((msg-bit (nth msg-i msg)))
    (if (null msg-bit)
      filler
      (logand (nth msg-i msg) 1))))
