;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun one-operand-x64 (first-byte-base reg-byte-base arg1 &optional arg2) 
  (let*
    ((modrm (gethash arg1 *modrm-reg-hash-table-x64*)))
    (cond
      ((or
         (equal (gethash arg1 *reg-type-hash-table-x64*) "old-8-bit-low-reg")
         (equal (gethash arg1 *reg-type-hash-table-x64*) "old-8-bit-high-reg"))
       (list first-byte-base (logior reg-byte-base modrm)))
      ((equal (gethash arg1 *reg-type-hash-table-x64*) "old-16-bit-reg")
       (list #x66 (1+ first-byte-base) (logior reg-byte-base modrm)))
      ((equal (gethash arg1 *reg-type-hash-table-x64*) "old-32-bit-reg")
       (list (1+ first-byte-base) (logior reg-byte-base modrm)))
      ((equal (gethash arg1 *reg-type-hash-table-x64*) "old-64-bit-reg")
       (append (emit-high-even-rex) (list (1+ first-byte-base) (logior reg-byte-base modrm))))
      ((equal (gethash arg1 *reg-type-hash-table-x64*) "new-8-bit-low-reg-l")
       (append (emit-even-rex) (list first-byte-base (logior reg-byte-base modrm))))
      ((equal (gethash arg1 *reg-type-hash-table-x64*) "new-8-bit-low-reg-b")
       (append (emit-odd-rex) (list first-byte-base (logior reg-byte-base modrm))))
      ((equal (gethash arg1 *reg-type-hash-table-x64*) "new-16-bit-reg")
       (append (list #x66) (emit-low-odd-rex) (list (1+ first-byte-base) (logior reg-byte-base modrm))))
      ((equal (gethash arg1 *reg-type-hash-table-x64*) "new-32-bit-reg")
       (append (emit-low-odd-rex) (list (1+ first-byte-base) (logior reg-byte-base modrm))))
      ((equal (gethash arg1 *reg-type-hash-table-x64*) "new-64-bit-reg")
       (append (emit-high-odd-rex) (list (1+ first-byte-base) (logior reg-byte-base modrm))))
      (t nil))))
