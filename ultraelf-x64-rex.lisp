;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun emit-high-rex (&rest args)
  "This function emits a high REX prefix:
   0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e or 0x4f.
   Can be chosen randomly or at will."
  (list #x48))

(defun emit-even-rex (&rest args)
  "This function emits an even REX prefix:
   0x40, 0x42, 0x44, 0x46, 0x48, 0x4a, 0x4c or 0x4e.
   Can be chosen randomly or at will."
  (list #x40))

(defun emit-odd-rex (&rest args)
  "This function emits an odd REX prefix:
   0x41, 0x43, 0x45, 0x47, 0x49, 0x4b, 0x4d or 0x4f.
   Can be chosen randomly or at will."
  (list #x41))

(defun emit-low-odd-rex (&rest args)
  "This function emits low odd REX prefix:
   0x41, 0x43, 0x45 or 0x47.
   Can be chosen randomly or at will."
  (list #x41))

(defun emit-high-even-rex (&rest args)
  "This function emits high even REX prefix:
   0x48, 0x4a, 0x4c or 0x4e.
   Can be chosen randomly or at will."
  (list #x48))

(defun emit-high-odd-rex (&rest args)
  "This function emits high even REX prefix:
   0x49, 0x4b, 0x4d or 0x4f.
   Can be chosen randomly or at will."
  (list #x49))
