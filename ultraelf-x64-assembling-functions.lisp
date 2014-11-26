;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun get-all-encodings-for-x64-syntax-tree (syntax-tree)
  "This function converts x64 syntax tree to a list of lists of lists of binary code bytes,
   the encodings of each instruction on their own list,
   the bytes of each encoding on their own list."
  (get-all-encodings-for-syntax-tree syntax-tree *emit-function-hash-table-x64*))

(defun get-all-encodings-for-x64-syntax-tree-and-print-hex (syntax-tree)
  "This function converts x64 syntax tree to a list of strings of hexadecimal bytes."
  (print-hex (get-all-encodings-for-syntax-tree syntax-tree *emit-function-hash-table-x64*)))

(defun assemble-x64 (code)
  "This function assembles x86-64 (x64) code."
  (assemble code *emit-function-hash-table-x64*))

(defun assemble-x64-and-print-hex (code)
  "This function assembles x86-64 (x64) code and prints in a hexadecimal string."
  (print-hex (assemble code *emit-function-hash-table-x64*)))

(defun assemble-alternatives-x64 (code)
  "This function assembles x86-64 (x64) code, all alternatives."
  (assemble-alternatives code *emit-function-hash-table-x64*))

(defun assemble-alternatives-x64-and-print-hex (code)
  "This function assembles x86-64 (x64) code, all alternatives, and prints in a hexadecimal string."
  (print-hex (assemble-alternatives code *emit-function-hash-table-x64*)))

(defun get-list (my-list)
  "This function goes through list wrapped in a list... until the last list is found.
   Works nice for &rest args passed through one or more functions."
  (if (and
        (listp (first my-list))
        (not (null my-list)))
    (get-list (first my-list))
    my-list))

(defun emit-with-format-and-operands (code-format operands &rest args)
  "This function emits code (list of binary code bytes) for one instruction variant."
  (let
    ((my-list (get-list args)))
    (cond
      ((equal (first code-format) "[")
       (unless (null my-list)
         (error "[ encoding requires exactly 0 arguments."))
       ;; The encoding of this variant is constant, so just convert
       ;; the rest elements (hexadecimal numbers) to numbers in a list.
       (loop for code-string in (rest code-format)
             append (cond
                      ((equal code-string "o16")
                       (list #x66))
                      ((equal code-string "o32")
                       nil)
                      ((equal code-string "o64")
                       (emit-high-rex))
                      ((equal code-string "repe")
                       ;; a string instruction (not REPE itself!).
                       nil)
                      ((equal code-string "wait")
                       ;; FWAIT instruction or prefix.
                       (list #x9b))
                      ((equal code-string "mustrep")
                       ;; force REP prefix.
                       ;; ultraELF assumes REP prefix as a part of instruction,
                       ;; so eg. `rep xsha1` produces f3 f3 0f a6 c8 .
                       (list #xf3))
                      ((equal code-string "mustrepne")
                       ;; force REPNZ prefix.
                       ;; ultraELF assumes REPNZ prefix as a part of instruction.
                       ;; currently `"mustrepne"` flag is not in use (NASM 2.11.06).
                       (list #xf2))
                      ((equal code-string "np")
                       ;; no SSE prefix (LFENCE/MFENCE).
                       nil)
                      (t (list (parse-integer code-string :radix 16))))))
      ((equal (first code-format) "[m:")
       ;; This variant has one operand.
       ;; The operand is encoded in the r/m field.
       ;; An extension of the opcode is in the reg field.
       (unless (eql (length my-list) 1)
         (error "[m encoding requires exactly 1 argument."))
       (let
         ((arg1 (first my-list)))
         (loop for code-string in (rest code-format)
               append (cond
                        ((equal code-string "o16")
                         (list #x66))
                        ((equal code-string "o32")
                         nil)
                        ((equal code-string "o64")
                         (emit-high-rex))
                        ((equal code-string "wait")
                         ;; FWAIT instruction or prefix.
                         (list #x9b))
                        ((equal code-string "np")
                         ;; no SSE prefix (LFENCE/MFENCE).
                         nil)
                        ((equal code-string "/0")
                         (emit-modrm (modrm.mod arg1)
                                     0 ; extension encoded in reg field.
                                     (modrm.r/m arg1)))
                        ((equal code-string "/1")
                         (emit-modrm (modrm.mod arg1)
                                     1 ; extension encoded in reg field.
                                     (modrm.r/m arg1)))
                        ((equal code-string "/2")
                         (emit-modrm (modrm.mod arg1)
                                     2 ; extension encoded in reg field.
                                     (modrm.r/m arg1)))
                        ((equal code-string "/3")
                         (emit-modrm (modrm.mod arg1)
                                     3 ; extension encoded in reg field.
                                     (modrm.r/m arg1)))
                        ((equal code-string "/4")
                         (emit-modrm (modrm.mod arg1)
                                     4 ; extension encoded in reg field.
                                     (modrm.r/m arg1)))
                        ((equal code-string "/5")
                         (emit-modrm (modrm.mod arg1)
                                     5 ; extension encoded in reg field.
                                     (modrm.r/m arg1)))
                        ((equal code-string "/6")
                         (emit-modrm (modrm.mod arg1)
                                     6 ; extension encoded in reg field.
                                     (modrm.r/m arg1)))
                        ((equal code-string "/7")
                         (emit-modrm (modrm.mod arg1)
                                     7 ; extension encoded in reg field.
                                     (modrm.r/m arg1)))
                        ((equal code-string "hle")
                         ;; instruction takes XRELEASE with or without lock.
                         nil)
                        ((equal code-string "hlexr")
                         ;; instruction takes XACQUIRE/XRELEASE with or without lock.
                         nil)
                        ((equal code-string "hlenl")
                         ;; instruction takes XACQUIRE/XRELEASE with lock only.
                         nil)
                        (t (list (parse-integer code-string :radix 16)))))))
      (t (error "encoding not yet implemented")))))
