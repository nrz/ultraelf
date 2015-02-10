;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun convert-number-to-immediate (my-number)
  (if (numberp my-number)
    (make-instance 'immediate :name (write-to-string my-number) :value my-number)
    my-number))

(defun emit-little-endian-number-in-n-bytes (my-number n-bytes)
  (let
    ((current-byte 0))
    (if (not (numberp my-number))
      (setf my-number (value my-number)))
    (cond
      ((>= my-number (expt 2 (* 8 n-bytes)))
       (error "the value is too positive for given number of bytes"))
      ((< my-number (* -1 (expt 2 (1- (* 8 n-bytes)))))
       (error "the value is too negative for given number of bytes"))
      (t (if (< my-number 0)
           ;; convert negative values to positive values.
           (incf my-number (expt 2 (* 8 n-bytes))))
         (loop for i below n-bytes collect
               (progn (setf current-byte (logand my-number 255))
                      (setf my-number (ash my-number -8))
                      current-byte))))))

(defun emit-sign-extended-byte-for-n-bytes (my-number n-bytes)
  (if (not (numberp my-number))
    (setf my-number (value my-number)))
  (cond
    ((< my-number -128)
     (error "number too negative to be encoded in a sign-extended byte"))
    ((>= my-number (expt 2 (* 8 n-bytes)))
     (error "number too positive to be encoded in a sign-extended byte"))
    ((and
       (> my-number 127)
       (< my-number (- (expt 2 (* 8 n-bytes)) 128)))
     (error "number in a positive range that cannot be encoded in a sign-extended byte"))
    (t (list (if (< my-number 0)
               ;; convert negative values to positive values.
               (+ my-number 256)
               ;; positive value need only AND with 0xff.
               (logand my-number #xff))))))
