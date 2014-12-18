;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun emit-low-rex ()
  "This function emits a low REX prefix:
   0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46 or 0x47.
   Can be chosen randomly or at will."
  (list #x40))

(defun emit-high-rex ()
  "This function emits a high REX prefix:
   0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e or 0x4f.
   Can be chosen randomly or at will."
  (list #x48))

(defun emit-even-rex ()
  "This function emits an even REX prefix:
   0x40, 0x42, 0x44, 0x46, 0x48, 0x4a, 0x4c or 0x4e.
   Can be chosen randomly or at will."
  (list #x40))

(defun emit-odd-rex ()
  "This function emits an odd REX prefix:
   0x41, 0x43, 0x45, 0x47, 0x49, 0x4b, 0x4d or 0x4f.
   Can be chosen randomly or at will."
  (list #x41))

(defun emit-low-odd-rex ()
  "This function emits low odd REX prefix:
   0x41, 0x43, 0x45 or 0x47.
   Can be chosen randomly or at will."
  (list #x41))

(defun emit-high-odd-rex ()
  "This function emits high even REX prefix:
   0x49, 0x4b, 0x4d or 0x4f.
   Can be chosen randomly or at will."
  (list #x49))

(defun emit-low-even-rex ()
  "This function emits low even REX prefix:
   0x40, 0x42, 0x44 or 0x46.
   Can be chosen randomly or at will."
  (list #x40))

(defun emit-high-even-rex ()
  "This function emits high even REX prefix:
   0x48, 0x4a, 0x4c or 0x4e.
   Can be chosen randomly or at will."
  (list #x48))

(defun emit-0x48-or-0x4a-rex ()
  "This function emits a REX prefix 0x48 or 0x4a.
   Can be chosen randomly or at will."
  (list #x48))

(defun emit-rex-byte (rex.w rex.r rex.x rex.b)
  "This function emits a REX prefix as requested."
  (list (logior #x40
                rex.b
                (ash rex.x 1)
                (ash rex.r 2)
                (ash rex.w 3))))
