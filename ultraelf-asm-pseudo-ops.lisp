;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defvar *global-offset*)

(defun bits-pseudo-op (arg1 &rest args)
  "`bits` or `[bits` pseudo-op affects the encoding used. Use `[bits 64]` for x86-64."
  (defparameter *bits* arg1)
  nil)

(defun align-pseudo-op (arg1 &rest args)
  "`align` aligns the code by emitting enough nop's (0x90)."
  (if (or
        (eql *global-offset* 0)
        (eql (mod *global-offset* arg1) 0))
    nil
    (loop for i from 1 to (- arg1 (mod *global-offset* arg1))
          collect #x90)))

(defun db (&rest args)
  "`db` defines a byte."
  (apply #'append (loop for my-string in args collect (string-to-8-bit-little-endian my-string))))

(defun dw (&rest args)
  "`dw` defines a word (2 bytes)."
  (apply #'append (loop for my-string in args collect (string-to-16-bit-little-endian my-string))))

(defun dd (&rest args)
  "`dd` defines a doubleword (4 bytes)."
  (apply #'append (loop for my-string in args collect (string-to-32-bit-little-endian my-string))))

(defun dq (&rest args)
  "`dq` defines a quadword (8 bytes)."
  (apply #'append (loop for my-string in args collect (string-to-64-bit-little-endian my-string))))

(defun global-pseudo-op (arg1 &rest args)
  "`global` pseudo-op defines the entry point (?) of the code."
  (defparameter *entry-point* arg1)
  nil)

(defun section-pseudo-op (arg1 &rest args)
  "`section` pseudo-op starts a new section."
  (defparameter *current-section* arg1)
  nil)
