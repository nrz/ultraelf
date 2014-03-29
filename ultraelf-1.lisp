;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun create-syntax-tree (my-list)
  "This recursive function converts a string produced by transform-code-to-string into a syntax tree.
   Input argument my-list can be a string, in that case it is first stored into a list."
  (if (stringp my-list)
    (setf my-list (list my-list)))
  ;; is last element of the list a string?
  ;; if yes, read it to convert it to a list and append the resulting list to earlier elements.
  ;; otherwise use CONS to create (LIST element-1 element-2 ... ) .
  (if (stringp (first (last my-list)))
    (create-syntax-tree (append (butlast my-list) (rest (read-from-string (first (last my-list))))))
    (cons 'list my-list)))

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
