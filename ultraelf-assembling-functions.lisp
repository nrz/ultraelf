;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defparameter *global-offset* 0)
(defparameter $ 0)

(defun create-syntax-tree (my-list)
  "This recursive function converts a string produced by transform-code-to-string into a syntax tree.
   Input argument my-list can be a string, in that case it is first stored into a list.
   usage examples:
   (create-syntax-tree #a nop #e)
   (create-syntax-tree #a mov ax,bx #a shl bx,1 #e)"
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
                                                   (subseq my-list 0 (1- i))
                                                   (rest (read-from-string (nth i my-list)))
                                                   (subseq my-list (1+ i))))))))
  (if (eql (first my-list) 'list)
    my-list
    (cons 'list my-list)))

(defun convert-string-to-symbol-if-symbol-exists (my-string)
  "This function converts a string into a symbol, if such symbol exists.
   Otherwise this function converts the string into an instance of `unknown` class."
  (cond
    ((equal my-string "$")
     (setf $ (make-instance 'address :name (write-to-string *global-offset*) :value *global-offset*)))
    ((boundp (intern (string-upcase my-string)))
     (symbol-value (intern (string-upcase my-string))))
    (t (make-instance 'unknown :name my-string :value (parse-number my-string)))))

(defun emit-binary-code-for-one-instruction (syntax-list my-hash-table &key (emit-function-selector-function #'first))
  "This function converts a syntax list of one instruction to a list of binary code bytes,
   `emit-function-selector-function` can be eg. `#'first` or `#'(lambda (x) (first (last x)))`."
  (let*
    ((emit-functions-list (gethash (first syntax-list) my-hash-table))
     (binary-code (apply (funcall emit-function-selector-function emit-functions-list)
                         (loop for arg in (rest syntax-list) collect (convert-string-to-symbol-if-symbol-exists arg)))))
    (when (boundp '*global-offset*)
      (progn
        (incf *global-offset* (length binary-code))
        (setf $ (make-instance 'address :name (write-to-string *global-offset*) :value *global-offset*))))
    binary-code))

(defun get-nth (n)
  "This function returns a function that gets the nth value of a list."
  #'(lambda (my-list) (nth n my-list)))

(defun get-all-encodings-for-syntax-tree (syntax-tree my-hash-table)
  "This function converts syntax tree to a list of lists of lists of binary code bytes,
   the encodings of each instruction on their own list,
   the bytes of each encoding on their own list."
  (let
    ((syntax-list-of-lists (eval syntax-tree)))
    (loop for syntax-list in syntax-list-of-lists
          collect (loop for emit-function-i below (length (gethash (first syntax-list) my-hash-table))
                        collect (emit-binary-code-for-one-instruction syntax-list my-hash-table :emit-function-selector-function (get-nth emit-function-i))))))

(defun get-all-encodings-for-syntax-tree-and-print-hex (syntax-tree my-hash-table)
  "This function converts syntax tree to a list of strings of hexadecimal bytes."
  (print-hex (get-all-encodings-for-syntax-tree syntax-tree my-hash-table)))

(defun get-all-encodings-for-x64-syntax-tree (syntax-tree)
  "This function converts x64 syntax tree to a list of lists of lists of binary code bytes,
   the encodings of each instruction on their own list,
   the bytes of each encoding on their own list."
  (get-all-encodings-for-syntax-tree syntax-tree *emit-function-hash-table-x64*))

(defun get-all-encodings-for-x64-syntax-tree-and-print-hex (syntax-tree)
  "This function converts x64 syntax tree to a list of strings of hexadecimal bytes."
  (print-hex (get-all-encodings-for-syntax-tree syntax-tree *emit-function-hash-table-x64*)))

(defun emit-binary-code-list (syntax-tree my-hash-table &key (emit-function-selector-function #'first))
  "This function converts syntax tree to a list of lists of binary code bytes,
   the bytes of each instruction are on their own list.
   `emit-function-selector-function` can be eg. `#'first` or `#'(lambda (x) (first (last x)))`."
  (mapcar #'(lambda (x)
              (emit-binary-code-for-one-instruction x my-hash-table))
          (eval syntax-tree)))

(defun emit-binary-code (syntax-tree my-hash-table)
  "This function produces a single list of binary code bytes."
  (defparameter *global-offset* 0)
  (defparameter $ (make-instance 'address :name (write-to-string *global-offset*) :value *global-offset*))
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

(defun assemble-alternatives (code my-hash-table)
  "This function assembles code, all alternatives.
   Duplicates are removed."
  (mapcar #'(lambda (x)
              (remove-duplicates x :test #'equal))
          (get-all-encodings-for-syntax-tree (create-syntax-tree code) my-hash-table)))

(defun assemble-alternatives-and-print-hex (code my-hash-table)
  "This function assembles code, all alternatives, and prints in a hexadecimal string."
  (print-hex (get-all-encodings-for-syntax-tree (create-syntax-tree code) my-hash-table)))

(defun assemble-alternatives-x64 (code)
  "This function assembles x86-64 (x64) code, all alternatives."
  (assemble-alternatives code *emit-function-hash-table-x64*))

(defun assemble-alternatives-x64-and-print-hex (code)
  "This function assembles x86-64 (x64) code, all alternatives, and prints in a hexadecimal string."
  (print-hex (assemble-alternatives code *emit-function-hash-table-x64*)))

(defun string-to-function (my-string)
  "This function converts a string to a function.
   http://stackoverflow.com/questions/2940267/call-function-based-on-a-string/2940347#2940347"
  (symbol-function (find-symbol (string-upcase my-string))))
