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

(defun handle-nasm-code-format (code-format &rest args)
  "This function handler one code-string (from NASM's `insns.dat`) and returns a list."
  (let
    ((arg1 (first (get-list args))))
    (loop for code-string in code-format
          append (cond
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
                   ;; register-only encodings.
                   ;; there are currently 18 encodings of this type in use.
                   ;; to check `insns.dat` for new encodings of this type:
                   ;; `$ grep '[0-9a-f]+' insns.dat | sed 's/\(^.*\)\([\t ][0-9a-f][0-9a-f]+\)\(.*$\)/\2/g' | sed 's/[\t ]*//g' | sort | uniq`
                   ((equal code-string "40+r")
                    (list (+ #x40 (modrm.r/m arg1))))
                   ((equal code-string "48+r")
                    (list (+ #x48 (modrm.r/m arg1))))
                   ((equal code-string "50+r")
                    (list (+ #x50 (modrm.r/m arg1))))
                   ((equal code-string "58+r")
                    (list (+ #x58 (modrm.r/m arg1))))
                   ((equal code-string "70+r")
                    (list (+ #x70 (modrm.r/m arg1))))
                   ((equal code-string "71+r")
                    (list (+ #x71 (modrm.r/m arg1))))
                   ((equal code-string "80+r")
                    (list (+ #x80 (modrm.r/m arg1))))
                   ((equal code-string "90+r")
                    (list (+ #x90 (modrm.r/m arg1))))
                   ((equal code-string "b0+r")
                    (list (+ #xb0 (modrm.r/m arg1))))
                   ((equal code-string "b8+r")
                    (list (+ #xb8 (modrm.r/m arg1))))
                   ((equal code-string "c0+r")
                    (list (+ #xc0 (modrm.r/m arg1))))
                   ((equal code-string "c8+r")
                    (list (+ #xc8 (modrm.r/m arg1))))
                   ((equal code-string "d0+r")
                    (list (+ #xd0 (modrm.r/m arg1))))
                   ((equal code-string "d8+r")
                    (list (+ #xd8 (modrm.r/m arg1))))
                   ((equal code-string "e0+r")
                    (list (+ #xe0 (modrm.r/m arg1))))
                   ((equal code-string "e8+r")
                    (list (+ #xe8 (modrm.r/m arg1))))
                   ((equal code-string "f0+r")
                    (list (+ #xf0 (modrm.r/m arg1))))
                   ((equal code-string "f8+r")
                    (list (+ #xf8 (modrm.r/m arg1))))
                   ((equal code-string "o16")
                    (list #x66))
                   ((equal code-string "o32")
                    nil)
                   ((equal code-string "o64")
                    (emit-high-rex))
                   ((equal code-string "hle")
                    ;; instruction takes XRELEASE with or without lock.
                    nil)
                   ((equal code-string "hlexr")
                    ;; instruction takes XACQUIRE/XRELEASE with or without lock.
                    nil)
                   ((equal code-string "hlenl")
                    ;; instruction takes XACQUIRE/XRELEASE with lock only.
                    nil)
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
                   ((equal code-string "repe")
                    ;; a string instruction (not REPE itself!).
                    nil)
                   ((equal code-string "wait")
                    ;; FWAIT instruction or prefix.
                    (list #x9b))
                   (t (list (parse-integer code-string :radix 16)))))))

(defun emit-with-format-and-operands-x64 (code-format operands &rest args)
  "This function emits code (list of binary code bytes) for one x64 instruction variant."
  (let
    ((my-list (get-list args)))
    (cond
      ((equal (first code-format) "[")
       (unless (null my-list)
         (error "[ encoding requires exactly 0 arguments."))
       ;; The encoding of this variant is constant, so just convert
       ;; the rest elements (hexadecimal numbers) to numbers in a list.
       (handle-nasm-code-format (rest code-format)))
      ((equal (first code-format) "[m:")
       ;; This variant has one 'memory' (can be register too) operand.
       ;; The operand is encoded in the r/m field.
       ;; An extension of the opcode is in the reg field.
       (unless (eql (length my-list) 1)
         (error "[m: encoding requires exactly 1 argument."))
       (when (notany #'(lambda (x) (equal x (first operands))) (allowed-targets (first my-list)))
         (error "instruction's and operand's allowed targets do not match."))
       (handle-nasm-code-format (rest code-format) my-list))
      ((equal (first code-format) "[r:")
       ;; This variant has one register operand.
       ;; The operand is encoded in the r/m field.
       ;; An extension of the opcode is in the reg field.
       (unless (eql (length my-list) 1)
         (error "[r: encoding requires exactly 1 argument."))
       (when (notany #'(lambda (x) (equal x (first operands))) (allowed-targets (first my-list)))
         (error "instruction's and operand's allowed targets do not match."))
       (handle-nasm-code-format (rest code-format) my-list))
      (t (error "encoding not yet implemented")))))
