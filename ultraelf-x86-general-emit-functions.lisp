;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun get-base-index-and-scale (memory-address-syntax)
  "This function converts memory address syntax to a list of base, index and scale.
   Memory address syntax must start with [ and end with ] and it must not contain spaces."
  (let*
    ((base-index-and-scale (list "" "" ""))
     (list-index 0)
     (memory-address-syntax-list
       (cdr
         (split-string-into-list-of-strings
           (get-string-without-last-character memory-address-syntax)))))
    (loop for my-char in memory-address-syntax-list
          do (cond
               ((equal my-char "+")
                (incf list-index))
               ((equal my-char "*")
                (incf list-index))
               (t (setf (nth list-index base-index-and-scale)
                        (concatenate 'string (nth list-index base-index-and-scale) my-char)))))
    ;; if all base, index and scale are defined, fix the order before returning it.
    ;; otherwise set scale to 1 and return the list.
    (if (> (length (nth 2 base-index-and-scale)) 0)
      (return-from get-base-index-and-scale (list (nth 0 base-index-and-scale)
                                                  (nth 2 base-index-and-scale)
                                                  (nth 1 base-index-and-scale)))
      (return-from get-base-index-and-scale (list (nth 0 base-index-and-scale)
                                                  (nth 1 base-index-and-scale)
                                                  "1")))))

(defun emit-sib-byte (base index scale)
  "This function emits SIB byte."
  (list (logior
          (gethash base *modrm-reg-hash-table-x64*)
          (ash (gethash index *modrm-reg-hash-table-x64*) 3)
          (ash (gethash scale *sib-scale-hash-table-x64*) 6))))

(defun emit-sib-byte-for-memory-address-syntax (memory-address-syntax)
  "This function emits SIB byte for a memory address syntax."
  (let*
    ((base-index-and-scale nil))
    (setf base-index-and-scale (get-base-index-and-scale memory-address-syntax))
    (emit-sib-byte (nth 0 base-index-and-scale)
                   (nth 1 base-index-and-scale)
                   (nth 2 base-index-and-scale))))

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
