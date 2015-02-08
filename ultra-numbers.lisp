;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun convert-number-to-immediate (my-number)
  (if (numberp my-number)
    (make-instance 'immediate :name (write-to-string my-number) :value my-number)
    my-number))

(defun emit-byte (arg1)
  (let
    ((immediate (convert-number-to-immediate arg1)))
    (list (cond
            ((fits-in-unsigned-byte immediate)
             (value immediate))
            ((fits-in-signed-byte immediate)
             (+ (value immediate) 256))
            (t (error "value does not fit in a byte"))))))

(defun emit-little-endian-word (arg1)
  (let
    ((immediate (convert-number-to-immediate arg1)))
    (cond
      ((fits-in-unsigned-word immediate)
       (list
         (logand (value immediate) 255)
         (ash (value immediate) -8)))
      ((fits-in-signed-word immediate)
       (list
         (logand (+ (value immediate) 65536) 255)
         (ash (+ (value immediate) 65536) -8)))
      (t (error "value does not fit in a word")))))
