;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :x64)

(defun get-all-encodings-for-x64-syntax-tree (syntax-tree &key (skip-errors t))
  "This function converts x64 syntax tree to a list of lists of lists of binary code bytes,
   the encodings of each instruction on their own list,
   the bytes of each encoding on their own list."
  (get-all-encodings-for-syntax-tree syntax-tree *x64-instruction-variants-hash-table* :skip-errors skip-errors))

(defun get-all-encodings-for-x64-syntax-tree-and-print-hex (syntax-tree &key (skip-errors t))
  "This function converts x64 syntax tree to a list of strings of hexadecimal bytes."
  (print-hex (get-all-encodings-for-syntax-tree syntax-tree *x64-instruction-variants-hash-table* :skip-errors skip-errors)))

(defun assemble-x64 (code &key (skip-errors t))
  "This function assembles x86-64 (x64) code."
  (assemble code *x64-instruction-variants-hash-table* :skip-errors skip-errors))

(defun assemble-x64-and-print-hex (code &key (skip-errors t))
  "This function assembles x86-64 (x64) code and prints in a hexadecimal string."
  (print-hex (assemble code *x64-instruction-variants-hash-table* :skip-errors skip-errors)))

(defun assemble-alternatives-x64 (code &key (skip-errors t))
  "This function assembles x86-64 (x64) code, all alternatives."
  (assemble-alternatives code *x64-instruction-variants-hash-table* :skip-errors skip-errors))

(defun assemble-alternatives-x64-and-print-hex (code &key (skip-errors t))
  "This function assembles x86-64 (x64) code, all alternatives, and prints in a hexadecimal string."
  (print-hex (assemble-alternatives code *x64-instruction-variants-hash-table* :skip-errors skip-errors)))

(defun emit-rex (encoding-type n-operands &key given-operands (rex-w-value 0) (rex-r-value 0) (rex-x-value 0) (rex-b-value 0))
  "This function emits REX according to encoding type and the operands.
   rex-w-value : 0 for default operand size, 1 for 64-bit operand size.
   `emit-rex` does not handle steganographic or variable encoding in any
   particular way, it just encodes a REX byte with required bits as needed
   and optional bits according to `rex-w-value`, `rex-r-value`,
   `rex-x-value` and `rex-b-value. Using default values (0) for all REX
   bits produces REX bytes identical to those produced by NASM.
   For steganographic or variable encoding `emit-rex` must be called with
   appropriate values for the mentioned keyword arguments."
  (let*
    ((my-args (get-list given-operands))
     (arg1 (first my-args))
     (arg2 (second my-args)))
    (cond
      ((eql n-operands 0)
       (emit-rex-byte rex-w-value   ; operand size.
                      rex-r-value   ; TODO: encode here 1 bit of data!
                      rex-x-value   ; TODO: encode here 1 bit of data!
                      rex-b-value)) ; TODO: encode here 1 bit of data!
      ((eql n-operands 1)
       (emit-rex-byte rex-w-value    ; operand size.
                      rex-r-value    ; number of arguments should be checked already.
                      0              ; extension of the SIB index field, this should be
                      (rex-b arg1))) ; checked when implementing SIB!
      ((eql n-operands 2)
       (cond
         ((equal encoding-type "[mr:")
          (emit-rex-byte rex-w-value    ; operand size.
                         (rex-r arg2)   ; rex-r augments reg field, so it's from arg2 in `[mr:`.
                         0              ; extension of the SIB index field, this should be
                         (rex-b arg1))) ; rex-b augments r/m field, so it's from arg1 in `[mr:`.
         ((equal encoding-type "[rm:")
          (emit-rex-byte rex-w-value      ; operand size.
                         (rex-r arg1)     ; rex-r augments reg field, so it's from arg1 in `[rm:`.
                         0                ; extension of the SIB index field, this should be
                         (rex-b arg2))))) ; rex-b augments r/m field, so it's from arg2 in `[mr:`.
      (t (error "encoding not yet implemented")))))

(defun handle-nasm-code-format-x64 (code-format req-operands &key given-operands msg (rex-w-value 0) (rex-r-value 0) (rex-b-value 0))
  "This function handles one code-string (from NASM's `insns.dat`) and returns the following:
   0. the encoding as a list
   1. number of bits of `msg` encoded."
  (let*
    ((encoding-type (first code-format))
     (my-args (get-list given-operands))
     (arg1 (first my-args))  ; nil if list is too short.
     (arg2 (second my-args)) ; nil if list is too short.
     (do-args-require-rex (some #'needs-rex my-args))
     (do-args-work-with-rex (every #'works-with-rex my-args))
     (is-rex-already-encoded nil)
     (msg-i 0) ; index to message sequence.
     (n-operands (cond
                   ((and (eql (length req-operands) 1)
                         (equal (first req-operands) "void"))
                    0)
                   (t (length req-operands)))))
    (when
      (and do-args-require-rex (not do-args-work-with-rex))
      (error "impossible combination of given arguments: some need REX and some don't work with REX"))
    (loop for code-string in (rest code-format)
          ;; before `"o32"`, `"o64"` or `"o64nw"` there can be:
          ;; `"66"`, `"f2"`, `"f3"`, `"hle"`, `"hlenl"`, `"hlexr"`, `"mustrep"`, `"mustrepne"`,
          ;; `"norexb"`, `"norexr"`, `"norexw"`, `"norexx"`,
          ;; `"np"`, `"repe"`, `"wait"`.
          append (cond
                   ((equal code-string "66")
                    ;; in SIMD instructions used as a an instruction modifier,
                    ;; encoded before possible REX.
                    (list #x66))
                   ((equal code-string "f2")
                    ;; in SIMD instructions used as a an instruction modifier,
                    ;; encoded before possible REX.
                    (list #xf2))
                   ((equal code-string "f3")
                    ;; in SIMD instructions used as a an instruction modifier,
                    ;; encoded before possible REX.
                    (list #xf3))
                   ((equal code-string "hle")
                    ;; instruction takes XRELEASE with or without lock.
                    nil)
                   ((equal code-string "hlenl")
                    ;; instruction takes XACQUIRE/XRELEASE with lock only.
                    nil)
                   ((equal code-string "hlexr")
                    ;; instruction takes XACQUIRE/XRELEASE with or without lock.
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
                   ((equal code-string "norexb")
                    ;; invalid with REX.B.
                    nil)
                   ((equal code-string "norexr")
                    ;; invalid with REX.R.
                    nil)
                   ((equal code-string "norexw")
                    ;; invalid with REX.W.
                    nil)
                   ((equal code-string "norexx")
                    ;; invalid with REX.X.
                    nil)
                   ((equal code-string "np")
                    ;; no SSE prefix (LFENCE/MFENCE).
                    nil)
                   ((equal code-string "repe")
                    ;; a string instruction (not REPE itself!).
                    nil)
                   ((equal code-string "wait")
                    ;; FWAIT instruction or prefix.
                    (list #x9b))
                   ((equal code-string "o16")
                    (list #x66))
                   ((equal code-string "o32")
                    (cond
                      ((eql n-operands 0)
                       nil)
                      ((eql n-operands 1)
                       (cond
                         (do-args-require-rex
                           (setf is-rex-already-encoded t)
                           (emit-rex encoding-type n-operands :given-operands my-args :rex-r-value rex-r-value))
                         (t nil)))
                      ((eql n-operands 2)
                       (cond
                         (do-args-require-rex
                           (setf is-rex-already-encoded t)
                           (emit-rex encoding-type n-operands :given-operands my-args :rex-r-value rex-r-value))
                         (t nil)))
                      ((eql n-operands 3)
                       (error "o32 encoding of 3 operands in not yet implemented"))
                      (t (error "over 3 operands is an error"))))
                   ((equal code-string "o64")
                    (cond
                      ((eql n-operands 0)
                       (setf is-rex-already-encoded t)
                       (emit-rex encoding-type n-operands :given-operands my-args :rex-w-value 1 :rex-r-value rex-r-value)) ; 64-bit operand size!
                      ((eql n-operands 1)
                       (setf is-rex-already-encoded t)
                       (emit-rex encoding-type n-operands :given-operands my-args :rex-w-value 1 :rex-r-value rex-r-value)) ; 64-bit operand size!
                      ((eql n-operands 2)
                       (error "o64 encoding of 2 operands in not yet implemented"))
                      ((eql n-operands 3)
                       (error "o64 encoding of 3 operands in not yet implemented"))
                      (t (error "over 3 operands is an error"))))
                   ((equal code-string "o64nw") ; fixed 64-bit address size, REX on extensions only.
                    (cond
                      ((eql n-operands 0)
                       (error "o64nw with n-operands 0 is an error"))
                      ((eql n-operands 1)
                       (cond
                         (do-args-require-rex
                           (setf is-rex-already-encoded t)
                           ;; here in REX.W it's possible to encode 1 bit of data because default operand size in `o64nw` is 64 bits,
                           ;; and REX.W is encoded this way:
                           ;; 0 (default operand size) or 1 (64-bit operand size).
                           ;; 64-bit operand size is the default, so it's possible to encode 1 bit of information!!!
                           (emit-rex encoding-type n-operands :given-operands my-args :rex-w-value rex-w-value :rex-r-value rex-r-value))
                         (t nil)))
                      ((eql n-operands 2)
                       (error "o64nw encoding of 2 operands in not yet implemented"))
                      ((eql n-operands 3)
                       (error "o64nw encoding of 3 operands in not yet implemented"))
                      (t (error "over 3 operands is an error"))))
                   ;; something else, now we possibly need REX if it's not present yet.
                   (t (append (when 
                                (and
                                  do-args-require-rex
                                  (not is-rex-already-encoded))
                                (progn
                                  (setf is-rex-already-encoded t)
                                  (cond
                                    ((eql n-operands 1)
                                     (emit-rex encoding-type n-operands :given-operands my-args :rex-r-value rex-r-value))
                                    ((eql n-operands 2)
                                     (emit-rex encoding-type n-operands :given-operands my-args :rex-r-value rex-r-value)) ; TODO: check if REX.W be used to encode data here!
                                    (t (error "encoding not yet implemented")))))
                              (cond ((equal code-string "/r")
                                     (cond
                                       ((equal encoding-type "[mr:")
                                        ;; ok, arg1 goes to r/m field and arg2 goes to reg field.
                                        (emit-modrm (modrm-mod arg1)
                                                    (modrm-reg arg2)
                                                    (modrm-r/m arg1)))
                                       ((equal encoding-type "[rm:")
                                        ;; ok, arg1 goes to reg field and arg2 goes to r/m field.
                                        (emit-modrm (modrm-mod arg2)
                                                    (modrm-reg arg1)
                                                    (modrm-r/m arg2)))
                                       (t (error "encoding not yet implemented"))))
                                    ((equal code-string "/0")
                                     (emit-modrm (modrm-mod arg1)
                                                 0 ; extension encoded in reg field.
                                                 (modrm-r/m arg1)))
                                    ((equal code-string "/1")
                                     (emit-modrm (modrm-mod arg1)
                                                 1 ; extension encoded in reg field.
                                                 (modrm-r/m arg1)))
                                    ((equal code-string "/2")
                                     (emit-modrm (modrm-mod arg1)
                                                 2 ; extension encoded in reg field.
                                                 (modrm-r/m arg1)))
                                    ((equal code-string "/3")
                                     (emit-modrm (modrm-mod arg1)
                                                 3 ; extension encoded in reg field.
                                                 (modrm-r/m arg1)))
                                    ((equal code-string "/4")
                                     (emit-modrm (modrm-mod arg1)
                                                 4 ; extension encoded in reg field.
                                                 (modrm-r/m arg1)))
                                    ((equal code-string "/5")
                                     (emit-modrm (modrm-mod arg1)
                                                 5 ; extension encoded in reg field.
                                                 (modrm-r/m arg1)))
                                    ((equal code-string "/6")
                                     (emit-modrm (modrm-mod arg1)
                                                 6 ; extension encoded in reg field.
                                                 (modrm-r/m arg1)))
                                    ((equal code-string "/7")
                                     (emit-modrm (modrm-mod arg1)
                                                 7 ; extension encoded in reg field.
                                                 (modrm-r/m arg1)))
                                    ;; register-only encodings.
                                    ;; there are currently 18 encodings of this type in use.
                                    ;; to check `insns.dat` for new encodings of this type:
                                    ;; `$ grep '[0-9a-f]+' insns.dat | sed 's/\(^.*\)\([\t ][0-9a-f][0-9a-f]+\)\(.*$\)/\2/g' | sed 's/[\t ]*//g' | sort | uniq`
                                    ((equal code-string "40+r")
                                     (list (+ #x40 (modrm-r/m arg1))))
                                    ((equal code-string "48+r")
                                     (list (+ #x48 (modrm-r/m arg1))))
                                    ((equal code-string "50+r")
                                     (list (+ #x50 (modrm-r/m arg1))))
                                    ((equal code-string "58+r")
                                     (list (+ #x58 (modrm-r/m arg1))))
                                    ((equal code-string "70+r")
                                     (list (+ #x70 (modrm-r/m arg1))))
                                    ((equal code-string "71+r")
                                     (list (+ #x71 (modrm-r/m arg1))))
                                    ((equal code-string "80+r")
                                     (list (+ #x80 (modrm-r/m arg1))))
                                    ((equal code-string "90+r")
                                     (list (+ #x90 (modrm-r/m arg1))))
                                    ((equal code-string "b0+r")
                                     (list (+ #xb0 (modrm-r/m arg1))))
                                    ((equal code-string "b8+r")
                                     (list (+ #xb8 (modrm-r/m arg1))))
                                    ((equal code-string "c0+r")
                                     (list (+ #xc0 (modrm-r/m arg1))))
                                    ((equal code-string "c8+r")
                                     (list (+ #xc8 (modrm-r/m arg1))))
                                    ((equal code-string "d0+r")
                                     (list (+ #xd0 (modrm-r/m arg1))))
                                    ((equal code-string "d8+r")
                                     (list (+ #xd8 (modrm-r/m arg1))))
                                    ((equal code-string "e0+r")
                                     (list (+ #xe0 (modrm-r/m arg1))))
                                    ((equal code-string "e8+r")
                                     (list (+ #xe8 (modrm-r/m arg1))))
                                    ((equal code-string "f0+r")
                                     (list (+ #xf0 (modrm-r/m arg1))))
                                    ((equal code-string "f8+r")
                                     (list (+ #xf8 (modrm-r/m arg1))))
                                    (t (list (parse-integer code-string :radix 16))))))))))

(defun emit-with-format-and-operands-x64 (code-format req-operands &key given-operands msg)
  "This function emits code (list of binary code bytes) for one x64 instruction variant."
  (let*
    ((my-args (get-list given-operands))
     (my-operands (get-list req-operands)))
    (check-args my-operands my-args)
    (cond
      ((equal (first code-format) "[")
       ;; The encoding of this variant is constant, so just convert
       ;; the rest elements (hexadecimal numbers) to numbers in a list.
       (handle-nasm-code-format-x64 code-format my-operands :msg msg))
      ((equal (first code-format) "[m:")
       ;; This variant has one 'memory' (can be register too) operand.
       ;; The operand is encoded in the r/m field.
       ;; An extension of the opcode is in the reg field.
       (handle-nasm-code-format-x64 code-format my-operands :given-operands given-operands :msg msg))
      ((equal (first code-format) "[r:")
       ;; This variant has one register operand.
       ;; The operand is encoded in the r/m field.
       ;; An extension of the opcode is in the reg field.
       (handle-nasm-code-format-x64 code-format my-operands :given-operands given-operands :msg msg))
      ((equal (first code-format) "[mr:")
       ;; This variant has one memory operand and one register operand.
       ;; The operands are encoded in corresponding ModRM fields.
       (handle-nasm-code-format-x64 code-format my-operands :given-operands given-operands :msg msg))
      ((equal (first code-format) "[rm:")
       ;; This variant has one register operand and one memory operand.
       ;; The operands are encoded in corresponding ModRM fields.
       (handle-nasm-code-format-x64 code-format my-operands :given-operands given-operands :msg msg))
      (t (error "encoding not yet implemented")))))
