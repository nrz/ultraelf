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

(defun assemble-x64 (code &key (emit-function-selector-function (list #'sort-sublists-shortest-first #'first)) (skip-errors t))
  "This function assembles x86-64 (x64) code."
  (assemble code *x64-instruction-variants-hash-table* :emit-function-selector-function emit-function-selector-function :skip-errors skip-errors))

(defun assemble-x64-and-print-hex (code &key (emit-function-selector-function (list #'sort-sublists-shortest-first #'first)) (skip-errors t))
  "This function assembles x86-64 (x64) code and prints in a hexadecimal string.
   Please note that by default `*global-offset*` and `$` are zeroed after each instruction variant."
  (print-hex (assemble code *x64-instruction-variants-hash-table* :emit-function-selector-function emit-function-selector-function :skip-errors skip-errors)))

(defun assemble-alternatives-x64 (code &key (skip-errors t) (zero-global-offset t))
  "This function assembles x86-64 (x64) code, all alternatives.
   Please note that by default `*global-offset*` and `$` are zeroed after each instruction variant."
  (assemble-alternatives
    code
    *x64-instruction-variants-hash-table*
    :skip-errors skip-errors
    :zero-global-offset zero-global-offset))

(defun assemble-alternatives-x64-and-print-hex (code &key (skip-errors t) (zero-global-offset t))
  "This function assembles x86-64 (x64) code, all alternatives, and prints in a hexadecimal string.
   Please note that by default `*global-offset*` and `$` are zeroed after each instruction variant."
  (print-hex
    (assemble-alternatives
      code
      *x64-instruction-variants-hash-table*
      :skip-errors skip-errors
      :zero-global-offset zero-global-offset)))

(defun emit-xx-plus-r (encoding-type given-operands code-string)
  "This function emits code for `xx+r`, such as `90+r`."
  (let*
    ((my-args (get-list given-operands))
     (arg1 (first my-args))  ; nil if list is too short.
     (arg2 (second my-args)) ; nil if list is too short.
     (base (parse-integer (subseq code-string 0 2) :radix 16)))
    (cond
      ((equal encoding-type "[-r:")
       (list (+ base (modrm-r/m arg2))))
      ((equal encoding-type "[r:")
       (list (+ base (modrm-r/m arg1))))
      ((equal encoding-type "[r-:")
       (list (+ base (modrm-r/m arg1))))
      ((equal encoding-type "[ri:")
       (list (+ base (modrm-r/m arg1))))
      (t (error (concatenate 'string "xx+r encoding for encoding type " encoding-type " not yet implemented"))))))

(defun handle-nasm-code-format-x64-wrapper
  (code-format
    req-operands
    given-operands
    &key
    encoded-bytes
    is-rex-already-encoded
    (instruction-length-in-bytes 0))
  "This is simple wrapper function that calls `handle-nasm-code-format-x64` with
   the hardcoded parameters (so that `handle-nasm-code-format-x64` can be made simpler):
   `rex-w-value` `nil`
   `rex-r-value` `nil`
   `rex-x-value` `nil`
   `rex-b-value` `nil`
   `encoded-bytes` `nil`
   `instruction-length-in-bytes` 0"
  (handle-nasm-code-format-x64
    code-format
    req-operands
    given-operands
    nil           ; `rex-w-value`
    nil           ; `rex-r-value`
    nil           ; `rex-x-value`
    nil           ; `rex-b-value`
    encoded-bytes
    is-rex-already-encoded
    instruction-length-in-bytes))

(defun handle-nasm-code-format-x64
  (code-format
    req-operands
    given-operands
    rex-w-value
    rex-r-value
    rex-x-value
    rex-b-value
    encoded-bytes
    is-rex-already-encoded
    instruction-length-in-bytes)
  "This function handles one NASM's `insns.dat` code-string and returns the encoding as a list of lists (possible encodings)."
  (labels
    ((emit-rex-byte
       (code-format
         req-operands
         given-operands
         rex-w-value
         rex-r-value
         rex-x-value
         rex-b-value
         encoded-bytes
         is-rex-already-encoded
         instruction-length-in-bytes)
       ;; This macro emits a REX prefix as requested."
       (let*
         ((encoding-type (first code-format)))
         (cond
           ((null rex-w-value)
            (return-from handle-nasm-code-format-x64
                         (append
                           (handle-nasm-code-format-x64
                             code-format
                             req-operands
                             given-operands
                             0 ; `rex-w-value`
                             rex-r-value
                             rex-x-value
                             rex-b-value
                             encoded-bytes
                             is-rex-already-encoded
                             instruction-length-in-bytes)
                           (handle-nasm-code-format-x64
                             code-format
                             req-operands
                             given-operands
                             1 ; `rex-w-value`
                             rex-r-value
                             rex-x-value
                             rex-b-value
                             encoded-bytes
                             is-rex-already-encoded
                             instruction-length-in-bytes))))
           ((null rex-r-value)
            (return-from handle-nasm-code-format-x64
                         (append
                           (handle-nasm-code-format-x64
                             code-format
                             req-operands
                             given-operands
                             rex-w-value
                             0 ; `rex-r-value`
                             rex-x-value
                             rex-b-value
                             encoded-bytes
                             is-rex-already-encoded
                             instruction-length-in-bytes)
                           (handle-nasm-code-format-x64
                             code-format
                             req-operands
                             given-operands
                             rex-w-value
                             1 ; `rex-r-value`
                             rex-x-value
                             rex-b-value
                             encoded-bytes
                             is-rex-already-encoded
                             instruction-length-in-bytes))))
           ((null rex-x-value)
            (return-from handle-nasm-code-format-x64
                         (append
                           (handle-nasm-code-format-x64
                             code-format
                             req-operands
                             given-operands
                             rex-w-value
                             rex-r-value
                             0 ; `rex-x-value`
                             rex-b-value
                             encoded-bytes
                             is-rex-already-encoded
                             instruction-length-in-bytes)
                           (handle-nasm-code-format-x64
                             code-format
                             req-operands
                             given-operands
                             rex-w-value
                             rex-r-value
                             1 ; `rex-x-value`
                             rex-b-value
                             encoded-bytes
                             is-rex-already-encoded
                             instruction-length-in-bytes))))
           ((null rex-b-value)
            (return-from handle-nasm-code-format-x64
                         (append
                           (handle-nasm-code-format-x64
                             code-format
                             req-operands
                             given-operands
                             rex-w-value
                             rex-r-value
                             rex-x-value
                             0 ; `rex-b-value`
                             encoded-bytes
                             is-rex-already-encoded
                             instruction-length-in-bytes)
                           (handle-nasm-code-format-x64
                             code-format
                             req-operands
                             given-operands
                             rex-w-value
                             rex-r-value
                             rex-x-value
                             1 ; `rex-b-value`
                             encoded-bytes
                             is-rex-already-encoded
                             instruction-length-in-bytes))))
           (t (return-from handle-nasm-code-format-x64
                           (handle-nasm-code-format-x64-wrapper
                             (cons encoding-type (nthcdr 2 code-format)) ; `code-format`
                             req-operands
                             given-operands
                             :encoded-bytes (append
                                              encoded-bytes
                                              (list
                                                (logior #x40
                                                        rex-b-value
                                                        (ash rex-x-value 1)
                                                        (ash rex-r-value 2)
                                                        (ash rex-w-value 3))))
                             :is-rex-already-encoded t
                             :instruction-length-in-bytes (1+ instruction-length-in-bytes)))))))
     (emit-rex
       (code-format
         req-operands
         rex-w-value
         rex-r-value
         rex-x-value
         rex-b-value
         encoded-bytes
         is-rex-already-encoded
         instruction-length-in-bytes
         given-operands)
       ;; This macro emits REX according to encoding type and the operands.
       ;; `rex-w-value`: 0 for default operand size, 1 for 64-bit operand size.
       ;; `emit-rex` does not handle steganographic or variable encoding in any
       ;; particular way, it just encodes a REX byte with required bits as needed
       ;; and optional bits according to `rex-w-value`, `rex-r-value`,
       ;; `rex-x-value` and `rex-b-value`. Using default values (0) for all REX
       ;; bits produces REX bytes identical to those produced by NASM.
       ;; Steganographic encoding and variable encoding are done in ultraELF by
       ;; selecting the specific encoding from the list of all encodings that
       ;; fulfill the defined criteria (encoding length etc.)."
       (let*
         ((my-args (get-list given-operands))
          (arg1 (first my-args))
          (arg2 (second my-args))
          (encoding-type (first code-format))
          (n-operands (cond
                        ((and (eql (length req-operands) 1)
                              (equal (first req-operands) "void"))
                         0)
                        (t (length req-operands)))))
         (cond
           ((eql n-operands 0)
            (emit-rex-byte
              code-format
              req-operands
              given-operands
              rex-w-value
              rex-r-value
              rex-x-value
              rex-b-value
              encoded-bytes
              is-rex-already-encoded
              instruction-length-in-bytes))
           ((eql n-operands 1)
            (emit-rex-byte
              code-format
              req-operands
              given-operands
              rex-w-value
              rex-r-value
              rex-x-value
              (rex-b arg1)
              encoded-bytes
              is-rex-already-encoded
              instruction-length-in-bytes))
           ((eql n-operands 2)
            (cond
              ((equal encoding-type "[r-:")
               (emit-rex-byte
                 code-format
                 req-operands
                 given-operands
                 rex-w-value  ; operand size.
                 (rex-r arg1) ; number of arguments should be checked already.
                 rex-x-value  ; extension of the SIB index field, this should be
                 (rex-b arg1) ; checked when implementing SIB!
                 encoded-bytes
                 is-rex-already-encoded
                 instruction-length-in-bytes))
              ((equal encoding-type "[-r:")
               (emit-rex-byte
                 code-format
                 req-operands
                 given-operands
                 rex-w-value  ; operand size.
                 (rex-r arg2) ; number of arguments should be checked already.
                 rex-x-value  ; extension of the SIB index field, this should be
                 (rex-b arg2) ; checked when implementing SIB!
                 encoded-bytes
                 is-rex-already-encoded
                 instruction-length-in-bytes))
              ((equal encoding-type "[mi:")
               (emit-rex-byte
                 code-format
                 req-operands
                 given-operands
                 rex-w-value  ; operand size.
                 (rex-r arg1) ; number of arguments should be checked already.
                 rex-x-value  ; extension of the SIB index field, this should be
                 (rex-b arg1) ; checked when implementing SIB!
                 encoded-bytes
                 is-rex-already-encoded
                 instruction-length-in-bytes))
              ((equal encoding-type "[mr:")
               (emit-rex-byte
                 code-format
                 req-operands
                 given-operands
                 rex-w-value  ; operand size.
                 (rex-r arg2) ; rex-r augments reg field, so it's from arg2 in `[mr:`.
                 rex-x-value  ; extension of the SIB index field, this should be
                 (rex-b arg1) ; rex-b augments r/m field, so it's from arg1 in `[mr:`.
                 encoded-bytes
                 is-rex-already-encoded
                 instruction-length-in-bytes))
              ((equal encoding-type "[ri:")
               (emit-rex-byte
                 code-format
                 req-operands
                 given-operands
                 rex-w-value  ; operand size.
                 (rex-r arg1) ; number of arguments should be checked already.
                 rex-x-value  ; extension of the SIB index field, this should be
                 (rex-b arg1) ; checked when implementing SIB!
                 encoded-bytes
                 is-rex-already-encoded
                 instruction-length-in-bytes))
              ((equal encoding-type "[rm:")
               (emit-rex-byte
                 code-format
                 req-operands
                 given-operands
                 rex-w-value  ; operand size.
                 (rex-r arg1) ; rex-r augments reg field, so it's from arg1 in `[rm:`.
                 rex-x-value  ; extension of the SIB index field, this should be
                 (rex-b arg2) ; rex-b augments r/m field, so it's from arg2 in `[mr:`.
                 encoded-bytes
                 is-rex-already-encoded
                 instruction-length-in-bytes))
              ((equal encoding-type "[-i:")
               (emit-rex-byte
                 code-format
                 req-operands
                 given-operands
                 rex-w-value  ; operand size.
                 (rex-r arg1) ; number of arguments should be checked already.
                 rex-x-value  ; extension of the SIB index field, this should be
                 (rex-b arg1) ; checked when implementing SIB!
                 encoded-bytes
                 is-rex-already-encoded
                 instruction-length-in-bytes))
              (t (error (concatenate 'string "encoding of encoding-type " encoding-type " not yet implemented")))))
           (t (error (concatenate 'string "encoding of n-operands " (write-to-string n-operands) " not yet implemented")))))))
    (macrolet
      ((emit-and-update-instruction-length (&body body)
         ;; This macro increments `instruction-length-in-bytes` by the size of `body`, and then returns `body`.
         `(progn
            (incf instruction-length-in-bytes (length ,@body))
            ,@body)))
      (let*
        ((encoding-type (first code-format))
          (my-args (get-list given-operands))
          (arg1 (first my-args))  ; nil if list is too short.
          (arg2 (second my-args)) ; nil if list is too short.
          (arg3 (third my-args))  ; nil if list is too short.
          (arg4 (fourth my-args)) ; nil if list is too short.
          (do-args-require-rex (some #'needs-rex my-args))
          (do-args-work-with-rex (every #'works-with-rex my-args))
          (n-processed-code-strings 0)
          (n-operands (cond
                        ((and (eql (length req-operands) 1)
                              (equal (first req-operands) "void"))
                         0)
                        (t (length req-operands)))))
        (when
          (and do-args-require-rex (not do-args-work-with-rex))
          (error "impossible combination of given arguments: some need REX and some don't work with REX"))
        ;; `handle-nasm-code-format-x64` should somehow call itself recursively and return multiple encodings, if needed.
        ;; For example different but functionally identical REX encodings provide a situation with multiple encodings.
        ;; Producing multiple encodings in `handle-nasm-code-format-x64` is not yet implemented.
        (loop for code-string in (rest code-format)
          ;; before `"o32"`, `"o64"` or `"o64nw"` there can be:
          ;; `"66"`, `"f2"`, `"f3"`, `"hle"`, `"hlenl"`, `"hlexr"`, `"mustrep"`, `"mustrepne"`,
          ;; `"norexb"`, `"norexr"`, `"norexw"`, `"norexx"`,
          ;; `"np"`, `"repe"`, `"wait"`.
          do
          (setf encoded-bytes (append encoded-bytes (cond
                                                      ((equal code-string "rex!")
                                                       ;; `"rex!"` is a code-string used internally by ultraELF to force encoding of REX byte now.
                                                       (emit-rex
                                                         (cons encoding-type (nthcdr n-processed-code-strings (rest code-format)))
                                                         req-operands
                                                         rex-w-value
                                                         rex-r-value
                                                         rex-x-value
                                                         rex-b-value
                                                         encoded-bytes
                                                         is-rex-already-encoded
                                                         instruction-length-in-bytes
                                                         my-args))
                                                      ((equal code-string "66")
                                                       ;; in SIMD instructions used as a an instruction modifier,
                                                       ;; encoded before possible REX.
                                                       (emit-and-update-instruction-length (list #x66)))
                                                      ((equal code-string "f2")
                                                       ;; in SIMD instructions used as a an instruction modifier,
                                                       ;; encoded before possible REX.
                                                       (emit-and-update-instruction-length (list #xf2)))
                                                      ((equal code-string "f3")
                                                       ;; in SIMD instructions used as a an instruction modifier,
                                                       ;; encoded before possible REX.
                                                       (emit-and-update-instruction-length (list #xf3)))
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
                                                       (emit-and-update-instruction-length (list #xf3)))
                                                      ((equal code-string "mustrepne")
                                                       ;; force REPNZ prefix.
                                                       ;; ultraELF assumes REPNZ prefix as a part of instruction.
                                                       ;; currently `"mustrepne"` flag is not in use (NASM 2.11.09-rc1).
                                                       (emit-and-update-instruction-length (list #xf2)))
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
                                                       (emit-and-update-instruction-length (list #x9b)))
                                                      ((equal code-string "o16")
                                                       (setf rex-w-value 0) ; This is compulsory, because if REX.W is not 0, 0x66 prefix is ignored.
                                                       (emit-and-update-instruction-length (list #x66)))
                                                      ((equal code-string "o32")
                                                       (setf rex-w-value 0) ; This is compulsory, because if REX.W is not 0, 0x66 prefix is ignored.
                                                       (cond
                                                         ((eql n-operands 0)
                                                          nil)
                                                         ((eql n-operands 1)
                                                          (cond
                                                            (do-args-require-rex
                                                              (emit-and-update-instruction-length
                                                                (emit-rex
                                                                  (cons encoding-type (nthcdr n-processed-code-strings (rest code-format)))
                                                                  req-operands
                                                                  rex-w-value
                                                                  rex-r-value
                                                                  rex-x-value
                                                                  rex-b-value
                                                                  encoded-bytes
                                                                  is-rex-already-encoded
                                                                  instruction-length-in-bytes
                                                                  my-args)))
                                                            (t nil)))
                                                         ((eql n-operands 2)
                                                          (cond
                                                            (do-args-require-rex
                                                              (emit-and-update-instruction-length
                                                                (emit-rex
                                                                  (cons encoding-type (nthcdr n-processed-code-strings (rest code-format)))
                                                                  req-operands
                                                                  rex-w-value
                                                                  rex-r-value
                                                                  rex-x-value
                                                                  rex-b-value
                                                                  encoded-bytes
                                                                  is-rex-already-encoded
                                                                  instruction-length-in-bytes
                                                                  my-args)))
                                                            (t nil)))
                                                         ((eql n-operands 3)
                                                          (error "o32 encoding of 3 operands in not yet implemented"))
                                                         (t (error "over 3 operands is an error"))))
                                                      ((equal code-string "o64")
                                                       (cond
                                                         ((eql n-operands 0)
                                                          (emit-and-update-instruction-length
                                                            (emit-rex
                                                              (cons encoding-type (nthcdr n-processed-code-strings (rest code-format)))
                                                              req-operands
                                                              1 ; 64-bit operand size!
                                                              rex-r-value
                                                              rex-x-value
                                                              rex-b-value
                                                              encoded-bytes
                                                              is-rex-already-encoded
                                                              instruction-length-in-bytes
                                                              my-args)))
                                                         ((eql n-operands 1)
                                                          (emit-and-update-instruction-length
                                                            (emit-rex
                                                              (cons encoding-type (nthcdr n-processed-code-strings (rest code-format)))
                                                              req-operands
                                                              1 ; 64-bit operand size!
                                                              rex-r-value
                                                              rex-x-value
                                                              rex-b-value
                                                              encoded-bytes
                                                              is-rex-already-encoded
                                                              instruction-length-in-bytes
                                                              my-args)))
                                                         ((eql n-operands 2)
                                                          (emit-and-update-instruction-length
                                                            (emit-rex
                                                              (cons encoding-type (nthcdr n-processed-code-strings (rest code-format)))
                                                              req-operands
                                                              1 ; 64-bit operand size!
                                                              rex-r-value
                                                              rex-x-value
                                                              rex-b-value
                                                              encoded-bytes
                                                              is-rex-already-encoded
                                                              instruction-length-in-bytes
                                                              my-args)))
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
                                                              ;; here in REX.W it's possible to encode 1 bit of data because default operand size in `o64nw` is 64 bits,
                                                              ;; and REX.W is encoded this way:
                                                              ;; 0 (default operand size) or 1 (64-bit operand size).
                                                              ;; 64-bit operand size is the default, so it's possible to encode 1 bit of information!!!
                                                              (emit-and-update-instruction-length
                                                                (emit-rex
                                                                  (cons encoding-type (nthcdr n-processed-code-strings (rest code-format)))
                                                                  req-operands
                                                                  rex-w-value
                                                                  rex-r-value
                                                                  rex-x-value
                                                                  rex-b-value
                                                                  encoded-bytes
                                                                  is-rex-already-encoded
                                                                  instruction-length-in-bytes
                                                                  my-args)))
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
                                                                     (cond
                                                                       ((eql n-operands 1)
                                                                        (emit-and-update-instruction-length
                                                                          (emit-rex
                                                                            ;; `"rex!"` is needed below to force encoding of REX byte here.
                                                                            ;; `"rex!"` is a `code-string` that is internal to ultraELF.
                                                                            (cons encoding-type (cons "rex!" (nthcdr n-processed-code-strings (rest code-format))))
                                                                            req-operands
                                                                            rex-w-value
                                                                            rex-r-value
                                                                            rex-x-value
                                                                            rex-b-value
                                                                            encoded-bytes
                                                                            is-rex-already-encoded
                                                                            instruction-length-in-bytes
                                                                            my-args)))
                                                                       ((eql n-operands 2)
                                                                        (emit-and-update-instruction-length
                                                                          (emit-rex
                                                                            ;; `"rex!"` is needed below to force encoding of REX byte here.
                                                                            ;; `"rex!"` is a `code-string` that is internal to ultraELF.
                                                                            (cons encoding-type (cons "rex!" (nthcdr n-processed-code-strings (rest code-format))))
                                                                            req-operands
                                                                            rex-w-value
                                                                            rex-r-value
                                                                            rex-x-value
                                                                            rex-b-value
                                                                            encoded-bytes
                                                                            is-rex-already-encoded
                                                                            instruction-length-in-bytes
                                                                            my-args)))
                                                                       (t (error "encoding not yet implemented")))))
                                                                 (cond ((equal code-string "/r")
                                                                        (cond
                                                                          ((equal encoding-type "[mr:")
                                                                           ;; ok, arg1 goes to r/m field and arg2 goes to reg field.
                                                                           (emit-and-update-instruction-length (emit-modrm (modrm-mod arg1)
                                                                                                                           (modrm-reg arg2)
                                                                                                                           (modrm-r/m arg1))))
                                                                          ((equal encoding-type "[rm:")
                                                                           ;; ok, arg1 goes to reg field and arg2 goes to r/m field.
                                                                           (emit-and-update-instruction-length (emit-modrm (modrm-mod arg2)
                                                                                                                           (modrm-reg arg1)
                                                                                                                           (modrm-r/m arg2))))
                                                                          (t (error "encoding not yet implemented"))))
                                                                       ((equal code-string "/0")
                                                                        (emit-and-update-instruction-length (emit-modrm (modrm-mod arg1)
                                                                                                                        0 ; extension encoded in reg field.
                                                                                                                        (modrm-r/m arg1))))
                                                                       ((equal code-string "/1")
                                                                        (emit-and-update-instruction-length (emit-modrm (modrm-mod arg1)
                                                                                                                        1 ; extension encoded in reg field.
                                                                                                                        (modrm-r/m arg1))))
                                                                       ((equal code-string "/2")
                                                                        (emit-and-update-instruction-length (emit-modrm (modrm-mod arg1)
                                                                                                                        2 ; extension encoded in reg field.
                                                                                                                        (modrm-r/m arg1))))
                                                                       ((equal code-string "/3")
                                                                        (emit-and-update-instruction-length (emit-modrm (modrm-mod arg1)
                                                                                                                        3 ; extension encoded in reg field.
                                                                                                                        (modrm-r/m arg1))))
                                                                       ((equal code-string "/4")
                                                                        (emit-and-update-instruction-length (emit-modrm (modrm-mod arg1)
                                                                                                                        4 ; extension encoded in reg field.
                                                                                                                        (modrm-r/m arg1))))
                                                                       ((equal code-string "/5")
                                                                        (emit-and-update-instruction-length (emit-modrm (modrm-mod arg1)
                                                                                                                        5 ; extension encoded in reg field.
                                                                                                                        (modrm-r/m arg1))))
                                                                       ((equal code-string "/6")
                                                                        (emit-and-update-instruction-length (emit-modrm (modrm-mod arg1)
                                                                                                                        6 ; extension encoded in reg field.
                                                                                                                        (modrm-r/m arg1))))
                                                                       ((equal code-string "/7")
                                                                        (emit-and-update-instruction-length (emit-modrm (modrm-mod arg1)
                                                                                                                        7 ; extension encoded in reg field.
                                                                                                                        (modrm-r/m arg1))))
                                                                       ;; register-only encodings.
                                                                       ;; there are currently 18 encodings of this type in use.
                                                                       ;; to check `insns.dat` for new encodings of this type:
                                                                       ;; `$ sed 's/.*\([\t ][0-9a-f][0-9a-f]+\).*/\1/g;tx;d;:x;s/[\t ]*//g' insns.dat | sort | uniq`
                                                                       ((and
                                                                          (eql (length code-string) 4) ; "eg. `"90+r"`.
                                                                          (equal (subseq code-string 2) "+r"))
                                                                        (emit-and-update-instruction-length (emit-xx-plus-r encoding-type given-operands code-string)))
                                                                       ((equal code-string "ib")
                                                                        (cond
                                                                          ((eql n-operands 0)
                                                                           (error "ib encoding for 0 operands is an error"))
                                                                          ((eql n-operands 1)
                                                                           (error "ib encoding for 1 operand not yet implemented"))
                                                                          ((eql n-operands 2)
                                                                           (emit-and-update-instruction-length (emit-number-in-n-bytes arg2 1)))
                                                                          ((eql n-operands 3)
                                                                           (emit-and-update-instruction-length (emit-number-in-n-bytes arg3 1)))
                                                                          ((eql n-operands 4)
                                                                           (emit-and-update-instruction-length (emit-number-in-n-bytes arg4 1)))
                                                                          (t (error "over 4 operands is an error"))))
                                                                       ((equal code-string "iw")
                                                                        (cond
                                                                          ((eql n-operands 0)
                                                                           (error "iw encoding for 0 operands is an error"))
                                                                          ((eql n-operands 1)
                                                                           (error "iw encoding for 1 operand not yet implemented"))
                                                                          ((eql n-operands 2)
                                                                           (emit-and-update-instruction-length (emit-number-in-n-bytes arg2 2)))
                                                                          ((eql n-operands 3)
                                                                           (emit-and-update-instruction-length (emit-number-in-n-bytes arg3 2)))
                                                                          ((eql n-operands 4)
                                                                           (emit-and-update-instruction-length (emit-number-in-n-bytes arg4 2)))
                                                                          (t (error "over 4 operands is an error"))))
                                                                       ((equal code-string "id")
                                                                        (cond
                                                                          ((eql n-operands 0)
                                                                           (error "id encoding for 0 operands is an error"))
                                                                          ((eql n-operands 1)
                                                                           (error "id encoding for 1 operand not yet implemented"))
                                                                          ((eql n-operands 2)
                                                                           (emit-and-update-instruction-length (emit-number-in-n-bytes arg2 4)))
                                                                          ((eql n-operands 3)
                                                                           (emit-and-update-instruction-length (emit-number-in-n-bytes arg3 4)))
                                                                          ((eql n-operands 4)
                                                                           (emit-and-update-instruction-length (emit-number-in-n-bytes arg4 4)))
                                                                          (t (error "over 4 operands is an error"))))
                                                                       ((equal code-string "iq")
                                                                        (cond
                                                                          ((eql n-operands 0)
                                                                           (error "iq encoding for 0 operands is an error"))
                                                                          ((eql n-operands 1)
                                                                           (error "iq encoding for 1 operand not yet implemented"))
                                                                          ((eql n-operands 2)
                                                                           (emit-and-update-instruction-length (emit-number-in-n-bytes arg2 8)))
                                                                          ((eql n-operands 3)
                                                                           (emit-and-update-instruction-length (emit-number-in-n-bytes arg3 8)))
                                                                          ((eql n-operands 4)
                                                                           (emit-and-update-instruction-length (emit-number-in-n-bytes arg4 8)))
                                                                          (t (error "over 4 operands is an error"))))
                                                                       ((equal code-string "ib,s")
                                                                        (cond
                                                                          ((eql n-operands 0)
                                                                           (error "ib,s encoding for 0 operands is an error"))
                                                                          ((eql n-operands 1)
                                                                           (cond
                                                                             ((equal (first req-operands) "imm8")
                                                                              (emit-and-update-instruction-length (emit-sign-extended-byte-for-n-bytes arg1 8)))
                                                                             (t (error "ib,s encoding for 1 operand not yet implemented"))))
                                                                          ((eql n-operands 2)
                                                                           (emit-and-update-instruction-length (emit-sign-extended-byte-for-n-bytes arg2 (/ (reg-size arg1) 8))))
                                                                          ((eql n-operands 3)
                                                                           (error "ib,s encoding for 3 operands not yet implemented"))
                                                                          ((eql n-operands 4)
                                                                           (error "ib,s encoding for 4 operands not yet implemented"))
                                                                          (t (error "over 4 operands is an error"))))
                                                                       ((equal code-string "id,s")
                                                                        (cond
                                                                          ((eql n-operands 0)
                                                                           (error "id,s encoding for 0 operands is an error"))
                                                                          ((eql n-operands 1)
                                                                           (cond
                                                                             ((or
                                                                                (equal (first req-operands) "imm32")
                                                                                (equal (first req-operands) "imm64"))
                                                                              (emit-and-update-instruction-length (emit-sign-extended-dword-for-n-bytes arg1 8)))
                                                                             (t (error "id,s encoding for 1 operand not yet implemented"))))
                                                                          ((eql n-operands 2)
                                                                           (cond
                                                                             ((or
                                                                                (equal (first req-operands) "reg64")
                                                                                (equal (first req-operands) "rm64")
                                                                                (equal (first req-operands) "reg_rax"))
                                                                              (emit-and-update-instruction-length (emit-sign-extended-dword-for-n-bytes arg2 8)))
                                                                             (t (error "id,s encoding for 2 operands not yet implemented"))))
                                                                          ((eql n-operands 3)
                                                                           (error "id,s encoding for 3 operands not yet implemented"))
                                                                          ((eql n-operands 4)
                                                                           (error "id,s encoding for 4 operands not yet implemented"))
                                                                          (t (error "over 4 operands is an error"))))
                                                                       ((equal code-string "rel8")
                                                                        (if (is-immediate arg1)
                                                                          (let*
                                                                            ((current-address (+ *global-offset* instruction-length-in-bytes))
                                                                             (rel-address (- (value arg1) (1+ current-address))))
                                                                            (cond
                                                                              ((and
                                                                                 (<= rel-address 127)
                                                                                 (>= rel-address 0))
                                                                               ;; OK, this is a relative jump 0..127 bytes forward.
                                                                               (emit-and-update-instruction-length (list rel-address)))
                                                                              ((and
                                                                                 (>= rel-address -128)
                                                                                 (<= rel-address -1))
                                                                               ;; OK, this is a relative jump 1..128 bytes backward.
                                                                               (emit-and-update-instruction-length (list (+ rel-address 256))))
                                                                              (t (error "jump of out range, must be within -128..+127 bytes"))))
                                                                          (error "the first argument for a relative jump must be an address")))
                                                                       ((equal code-string "a16")
                                                                        nil)
                                                                       ((equal code-string "a32")
                                                                        (emit-and-update-instruction-length (list #x67)))
                                                                       ((equal code-string "a64")
                                                                        nil)
                                                                       ((equal code-string "nof3")
                                                                        nil)
                                                                       ((equal code-string "adf")
                                                                        nil)
                                                                       ((equal code-string "odf")
                                                                        nil)
                                                                       (t (emit-and-update-instruction-length (list (parse-integer code-string :radix 16))))))))))
          (incf n-processed-code-strings)))))
  (list encoded-bytes))

(defun emit-with-format-and-operands-x64 (code-format req-operands given-operands)
  "This function emits code (list of binary code bytes) for one x64 instruction variant."
  (let*
    ((encoding-type (first code-format))
     (my-args (get-list given-operands))
     (my-operands (get-list req-operands)))
    (check-args my-operands my-args)
    (cond
      ((equal encoding-type "[")
       ;; The encoding of this variant is constant, so just convert
       ;; the rest elements (hexadecimal numbers) to numbers in a list.
       (handle-nasm-code-format-x64-wrapper code-format my-operands nil))
      ((equal encoding-type "[--:")
       ;; The operands and encoding of this variant are fixed.
       (handle-nasm-code-format-x64-wrapper code-format my-operands given-operands))
      ((equal encoding-type "[-i:")
       ;; One fixed operand and one immediate operand.
       (handle-nasm-code-format-x64-wrapper code-format my-operands given-operands))
      ((equal encoding-type "[-r:")
       ;; One fixed operand and one register operand.
       (handle-nasm-code-format-x64-wrapper code-format my-operands given-operands))
      ((equal encoding-type "[m:")
       ;; This variant has one 'memory' (can be register too) operand.
       ;; The operand is encoded in the r/m field.
       ;; An extension of the opcode is in the reg field.
       (handle-nasm-code-format-x64-wrapper code-format my-operands given-operands))
      ((equal encoding-type "[i:")
       ;; This variant has one immediate operand.
       (handle-nasm-code-format-x64-wrapper code-format my-operands given-operands))
      ((equal encoding-type "[r:")
       ;; This variant has one register operand.
       ;; The operand is encoded in the r/m field.
       ;; An extension of the opcode is in the reg field.
       (handle-nasm-code-format-x64-wrapper code-format my-operands given-operands))
      ((equal encoding-type "[i-:")
       ;; One immediate operand and one fixed operand.
       (handle-nasm-code-format-x64-wrapper code-format my-operands given-operands))
      ((equal encoding-type "[r-:")
       ;; One register operand and one fixed operand.
       (handle-nasm-code-format-x64-wrapper code-format my-operands given-operands))
      ((equal encoding-type "[mi:")
       ;; One memory operand and one immediate operand.
       ;; The operands are encoded in corresponding ModRM fields.
       (handle-nasm-code-format-x64-wrapper code-format my-operands given-operands))
      ((equal encoding-type "[mr:")
       ;; One memory operand and one register operand.
       ;; The operands are encoded in corresponding ModRM fields.
       (handle-nasm-code-format-x64-wrapper code-format my-operands given-operands))
      ((equal encoding-type "[ri:")
       ;; One register operand and one immediate operand.
       ;; The operands are encoded in corresponding ModRM fields.
       (handle-nasm-code-format-x64-wrapper code-format my-operands given-operands))
      ((equal encoding-type "[rm:")
       ;; One register operand and one memory operand.
       ;; The operands are encoded in corresponding ModRM fields.
       (handle-nasm-code-format-x64-wrapper code-format my-operands given-operands))
      (t (error (concatenate 'string "encoding type " encoding-type " not yet implemented"))))))
