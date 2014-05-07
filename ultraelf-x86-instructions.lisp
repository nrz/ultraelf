;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

;; segment registers.
(defun cs-x86 (&rest args)
  (list #x2e))
(defun ds-x86 (&rest args)
  (list #x3e))
(defun es-x86 (&rest args)
  (list #x26))
(defun fs-x86 (&rest args)
  (list #x64))
(defun gs-x86 (&rest args)
  (list #x65))
(defun ss-x86 (&rest args)
  (list #x36))

(defun adc-reg-rm-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-reg-rm-x64 #x10 arg1 arg2 arg3))

(defun adc-rm-reg-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-rm-reg-x64 #x10 arg1 arg2 arg3))

(defun adc-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-x64 #x10 arg1 arg2 arg3))

(defun add-reg-rm-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-reg-rm-x64 #x00 arg1 arg2 arg3))

(defun add-rm-reg-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-rm-reg-x64 #x00 arg1 arg2 arg3))

(defun add-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-x64 #x00 arg1 arg2 arg3))

(defun and-reg-rm-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-reg-rm-x64 #x20 arg1 arg2 arg3))

(defun and-rm-reg-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-rm-reg-x64 #x20 arg1 arg2 arg3))

(defun and-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-x64 #x20 arg1 arg2 arg3))

(defun clc-x86 (&rest args)
  (list #xf8))
(defun cld-x86 (&rest args)
  (list #xfc))
(defun cli-x86 (&rest args)
  (list #xfa))
(defun cmc-x86 (&rest args)
  (list #xf5))

(defun cmp-reg-rm-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-reg-rm-x64 #x38 arg1 arg2 arg3))

(defun cmp-rm-reg-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-rm-reg-x64 #x38 arg1 arg2 arg3))

(defun cmp-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-x64 #x38 arg1 arg2 arg3))

(defun cmpsb-x86 (&rest args)
  (list #xa6))
(defun cmpsw-x86 (&rest args)
  (list #x66 #xa7))
(defun cmpsd-x32-x64 (&rest args)
  (list #xa7))
(defun cmpsq-48-x64 (&rest args)
  (list #x48 #xa7))
(defun cmpsq-49-x64 (&rest args)
  (list #x49 #xa7))
(defun cmpsq-4a-x64 (&rest args)
  (list #x4a #xa7))
(defun cmpsq-4b-x64 (&rest args)
  (list #x4a #xa7))
(defun cmpsq-4c-x64 (&rest args)
  (list #x4c #xa7))
(defun cmpsq-4d-x64 (&rest args)
  (list #x4d #xa7))
(defun cmpsq-4e-x64 (&rest args)
  (list #x4e #xa7))
(defun cmpsq-4f-x64 (&rest args)
  (list #x4f #xa7))

(defun dec-x64 (arg1 &optional arg2 &rest args)
  (one-operand-x64 #xfe #xc8 arg1 arg2))

(defun hlt-x86 (&rest args)
  (list #xf4))

(defun in-x32-x64 (arg1 arg2 &rest args)
  (cond
    ((and (equalp arg1 "al") (equalp arg2 "dx"))
     (list #xec))
    ((and (equalp arg1 "ax") (equalp arg2 "dx"))
     (list #x66 #xed))
    ((and (equalp arg1 "eax") (equalp arg2 "dx"))
     (list #xed))
    (t nil)))

(defun inc-x64 (arg1 &optional arg2 &rest args)
  (one-operand-x64 #xfe #xc0 arg1 arg2))

(defun insb-x86 (&rest args)
  (list #x6c))
(defun insw-x86 (&rest args)
  (list #x66 #x6d))
(defun insd-x32-x64 (&rest args)
  (list #x6d))

(defun lea-x64 (arg1 arg2 &rest args)
  (cond
    ((and
       (equal (gethash arg1 *reg-type-hash-table-x64*) "old-16-bit-reg")
       (equal (gethash arg2 *reg-type-hash-table-x64*) "register-indirect-without-SIB"))
     (append (list #x66 #x8d) (emit-modrm-byte-for-indirect-without-SIB arg2 arg1)))
    ((and
       (equal (gethash arg1 *reg-type-hash-table-x64*) "old-32-bit-reg")
       (equal (gethash arg2 *reg-type-hash-table-x64*) "register-indirect-without-SIB"))
     (append (list #x8d) (emit-modrm-byte-for-indirect-without-SIB arg2 arg1)))
    ((and
       (equal (gethash arg1 *reg-type-hash-table-x64*) "old-64-bit-reg")
       (equal (gethash arg2 *reg-type-hash-table-x64*) "register-indirect-without-SIB"))
     (append (emit-0x48-or-0x4a-rex) (list #x8d) (emit-modrm-byte-for-indirect-without-SIB arg2 arg1)))
    (t nil)))

(defun lodsb-x86 (&rest args)
  (list #xac))
(defun lodsw-x86 (&rest args)
  (list #x66 #xad))
(defun lodsd-x32-x64 (&rest args)
  (list #xad))
(defun lodsq-48-x64 (&rest args)
  (list #x48 #xad))
(defun lodsq-49-x64 (&rest args)
  (list #x49 #xad))
(defun lodsq-4a-x64 (&rest args)
  (list #x4a #xad))
(defun lodsq-4b-x64 (&rest args)
  (list #x4b #xad))
(defun lodsq-4c-x64 (&rest args)
  (list #x4c #xad))
(defun lodsq-4d-x64 (&rest args)
  (list #x4d #xad))
(defun lodsq-4e-x64 (&rest args)
  (list #x4e #xad))
(defun lodsq-4f-x64 (&rest args)
  (list #x4f #xad))

(defun mov-reg-rm-x64 (arg1 arg2 &optional arg3 &rest args)
  (let*
    ((arg1-reg-type (gethash arg1 *reg-type-hash-table-x64*))
     (arg2-reg-type (gethash arg2 *reg-type-hash-table-x64*)))
    (cond
      ((and
         (or
           (equal arg1-reg-type "old-8-bit-low-reg")
           (equal arg1-reg-type "old-8-bit-high-reg"))
         (or
           (equal arg2-reg-type "old-8-bit-low-reg")
           (equal arg2-reg-type "old-8-bit-high-reg")))
       ;; 0x8a can also be used, requires reverse order in ModRM.
       ;; 0x88: mov r/m8, r8
       ;; 0x8a: mov r8, r/m8
       (cons #x8a (emit-modrm-byte-for-reg-reg arg2 arg1)))
      ((and (equal arg1-reg-type "old-16-bit-reg")
            (equal arg2-reg-type "old-16-bit-reg"))
       ;; 0x8b can also be used, requires reverse order in ModRM.
       ;; 0x89: mov r/m16, r16
       ;; 0x8b: mov r16, r/m16
       (append (list #x66 #x8b) (emit-modrm-byte-for-reg-reg arg2 arg1)))
      ((and (equal arg1-reg-type "old-32-bit-reg")
            (equal arg2-reg-type "old-32-bit-reg"))
       ;; 0x8b can also be used, requires reverse order in ModRM.
       ;; 0x89: mov r/m32, r32
       ;; 0x8b: mov r32, r/m32
       (cons #x8b (emit-modrm-byte-for-reg-reg arg2 arg1)))
      ((and
         (or
           (equal arg1-reg-type "old-8-bit-low-reg")
           (equal arg1-reg-type "old-8-bit-high-reg"))
         (equal arg2-reg-type "register-indirect-without-SIB"))
       (cons #x8a (emit-modrm-byte-for-indirect-without-SIB arg2 arg1)))
      ((and (equal arg1-reg-type "old-16-bit-reg")
            (equal arg2-reg-type "register-indirect-without-SIB"))
       (append (list #x66 #x8b) (emit-modrm-byte-for-indirect-without-SIB arg2 arg1)))
      ((and (equal arg1-reg-type "old-32-bit-reg")
            (equal arg2-reg-type "register-indirect-without-SIB"))
       (cons #x8b (emit-modrm-byte-for-indirect-without-SIB arg2 arg1)))
      (t nil))))

(defun mov-rm-reg-x64 (arg1 arg2 &optional arg3 &rest args)
  (let*
    ((arg1-reg-type (gethash arg1 *reg-type-hash-table-x64*))
     (arg2-reg-type (gethash arg2 *reg-type-hash-table-x64*)))
    (cond
      ((and
         (or
           (equal arg1-reg-type "old-8-bit-low-reg")
           (equal arg1-reg-type "old-8-bit-high-reg"))
         (or
           (equal arg2-reg-type "old-8-bit-low-reg")
           (equal arg2-reg-type "old-8-bit-high-reg")))
       ;; 0x8a can also be used, requires reverse order in ModRM.
       ;; 0x88: mov r/m8, r8
       ;; 0x8a: mov r8, r/m8
       (cons #x88 (emit-modrm-byte-for-reg-reg arg1 arg2)))
      ((and (equal arg1-reg-type "old-16-bit-reg")
            (equal arg2-reg-type "old-16-bit-reg"))
       ;; 0x8b can also be used, requires reverse order in ModRM.
       ;; 0x89: mov r/m16, r16
       ;; 0x8b: mov r16, r/m16
       (append (list #x66 #x89) (emit-modrm-byte-for-reg-reg arg1 arg2)))
      ((and (equal arg1-reg-type "old-32-bit-reg")
            (equal arg2-reg-type "old-32-bit-reg"))
       ;; 0x8b can also be used, requires reverse order in ModRM.
       ;; 0x89: mov r/m32, r32
       ;; 0x8b: mov r32, r/m32
       (cons #x89 (emit-modrm-byte-for-reg-reg arg1 arg2)))
      ((and
         (equal arg1-reg-type "register-indirect-without-SIB")
         (or
           (equal arg2-reg-type "old-8-bit-low-reg")
           (equal arg2-reg-type "old-8-bit-high-reg")))
       (cons #x88 (emit-modrm-byte-for-indirect-without-SIB arg1 arg2)))
      ((and (equal arg1-reg-type "register-indirect-without-SIB")
            (equal arg2-reg-type "old-16-bit-reg"))
       (append (list #x66 #x89) (emit-modrm-byte-for-indirect-without-SIB arg1 arg2)))
      ((and (equal arg1-reg-type "register-indirect-without-SIB")
            (equal arg2-reg-type "old-32-bit-reg"))
       (cons #x89 (emit-modrm-byte-for-indirect-without-SIB arg1 arg2)))
      (t nil))))

(defun mov-x64 (arg1 arg2 &optional arg3 &rest args)
  ;; Following the logic used in YASM:
  ;; Use `defun mov-rm-reg-x64` always when you can.
  ;; Use `defun mov-reg-rm-x64` only if target is a register indirect without SIB.
  (if (equal (gethash arg2 *reg-type-hash-table-x64*) "register-indirect-without-SIB")
    (mov-reg-rm-x64 arg1 arg2 arg3 args)
    (mov-rm-reg-x64 arg1 arg2 arg3 args)))

(defun movsb-x86 (&rest args)
  (list #xa4))
(defun movsw-x86 (&rest args)
  (list #x66 #xa5))
(defun movsd-x32-x64 (&rest args)
  (list #xa5))
(defun movsq-48-x64 (&rest args)
  (list #x48 #xa5))
(defun movsq-49-x64 (&rest args)
  (list #x49 #xa5))
(defun movsq-4a-x64 (&rest args)
  (list #x4a #xa5))
(defun movsq-4b-x64 (&rest args)
  (list #x4b #xa5))
(defun movsq-4c-x64 (&rest args)
  (list #x4c #xa5))
(defun movsq-4d-x64 (&rest args)
  (list #x4d #xa5))
(defun movsq-4e-x64 (&rest args)
  (list #x4e #xa5))
(defun movsq-4f-x64 (&rest args)
  (list #x4f #xa5))

(defun neg-x64 (arg1 &optional arg2 &rest args)
  (one-operand-x64 #xf6 #xd8 arg1 arg2))

(defun nop-x86 (&rest args)
  (list #x90))

(defun not-x64 (arg1 &optional arg2 &rest args)
  (one-operand-x64 #xf6 #xd0 arg1 arg2))

(defun or-reg-rm-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-reg-rm-x64 #x08 arg1 arg2 arg3))

(defun or-rm-reg-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-rm-reg-x64 #x08 arg1 arg2 arg3))

(defun or-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-x64 #x08 arg1 arg2 arg3))

(defun out-x32-x64 (arg1 arg2 &rest args)
  (cond
    ((and (equalp arg1 "dx") (equalp arg2 "al"))
     (list #xee))
    ((and (equalp arg1 "dx") (equalp arg2 "ax"))
     (list #x66 #xef))
    ((and (equalp arg1 "dx") (equalp arg2 "eax"))
     (list #xef))
    (t nil)))

(defun outsb-x86 (&rest args)
  (list #x6e))
(defun outsw-x86 (&rest args)
  (list #x66 #x6f))
(defun outsd-x32-x64 (&rest args)
  (list #x6f))

(defun pop-x64 (arg1 &optional arg2)
  (let*
    ((r/m (r/m (symbol-value (intern (string-upcase arg1))))))
    (cond
      ((equal (gethash arg1 *reg-type-hash-table-x64*) "old-16-bit-reg")
       (list #x66 (logior #x58 r/m)))
      ((equal (gethash arg1 *reg-type-hash-table-x64*) "old-64-bit-reg")
       (list (logior #x58 r/m)))
      ((equal (gethash arg1 *reg-type-hash-table-x64*) "new-16-bit-reg")
       (append (list #x66) (emit-odd-rex) (list (logior #x58 r/m))))
      ((equal (gethash arg1 *reg-type-hash-table-x64*) "new-64-bit-reg")
       (append (emit-odd-rex) (list (logior #x58 r/m))))
      (t nil))))

(defun push-x64 (arg1 &optional arg2)
  (let*
    ((r/m (r/m (symbol-value (intern (string-upcase arg1))))))
    (cond
      ((equal (gethash arg1 *reg-type-hash-table-x64*) "old-16-bit-reg")
       (list #x66 (logior #x50 r/m)))
      ((equal (gethash arg1 *reg-type-hash-table-x64*) "old-64-bit-reg")
       (list (logior #x50 r/m)))
      ((equal (gethash arg1 *reg-type-hash-table-x64*) "new-16-bit-reg")
       (append (list #x66) (emit-odd-rex) (list (logior #x50 r/m))))
      ((equal (gethash arg1 *reg-type-hash-table-x64*) "new-64-bit-reg")
       (append (emit-odd-rex) (list (logior #x50 r/m))))
      (t nil))))

(defun rcl-1-x64 (arg1 &optional arg2 &rest args)
  (one-operand-x64 #xd0 #xd0 arg1 arg2))

(defun rcl-cl-x64 (arg1 &optional arg2 &rest args)
  (one-operand-x64 #xd2 #xd0 arg1 arg2))

(defun rcr-1-x64 (arg1 &optional arg2 &rest args)
  (one-operand-x64 #xd0 #xd8 arg1 arg2))

(defun rcr-cl-x64 (arg1 &optional arg2 &rest args)
  (one-operand-x64 #xd2 #xd8 arg1 arg2))

(defun rep-repz-x32-x64 (&optional arg1 &rest args)
  (cond
    ((equalp arg1 "cmpsw")
     (list #x66 #xf3 #xa7))
    ((equalp arg1 "insw")
     (list #x66 #xf3 #x6d))
    ((equalp arg1 "lodsw")
     (list #x66 #xf3 #xad))
    ((equalp arg1 "movsw")
     (list #x66 #xf3 #xa5))
    ((equalp arg1 "outsw")
     (list #x66 #xf3 #x6f))
    ((equalp arg1 "scasw")
     (list #x66 #xf3 #xaf))
    ((equalp arg1 "stosw")
     (list #x66 #xf3 #xab))
    ((eq arg1 nil)
     (list #xf3))
    (t (cons #xf3 (funcall (first (gethash arg1 *emit-function-hash-table-x64*)))))))

(defun repnz-x32-x64 (&optional arg1 &rest args)
  (cond
    ((equalp arg1 "cmpsw")
     (list #x66 #xf2 #xa7))
    ((equalp arg1 "insw")
     (list #x66 #xf2 #x6d))
    ((equalp arg1 "lodsw")
     (list #x66 #xf2 #xad))
    ((equalp arg1 "movsw")
     (list #x66 #xf2 #xa5))
    ((equalp arg1 "outsw")
     (list #x66 #xf2 #x6f))
    ((equalp arg1 "scasw")
     (list #x66 #xf2 #xaf))
    ((equalp arg1 "stosw")
     (list #x66 #xf2 #xab))
    ((eq arg1 nil)
     (list #xf2))
    (t (cons #xf2 (funcall (first (gethash arg1 *emit-function-hash-table-x64*)))))))

(defun rol-1-x64 (arg1 &optional arg2 &rest args)
  (one-operand-x64 #xd0 #xc0 arg1 arg2))

(defun rol-cl-x64 (arg1 &optional arg2 &rest args)
  (one-operand-x64 #xd2 #xc0 arg1 arg2))

(defun ror-1-x64 (arg1 &optional arg2 &rest args)
  (one-operand-x64 #xd0 #xc8 arg1 arg2))

(defun ror-cl-x64 (arg1 &optional arg2 &rest args)
  (one-operand-x64 #xd2 #xc8 arg1 arg2))

(defun sar-1-x64 (arg1 &optional arg2 &rest args)
  (one-operand-x64 #xd0 #xf8 arg1 arg2))

(defun sar-cl-x64 (arg1 &optional arg2 &rest args)
  (one-operand-x64 #xd2 #xf8 arg1 arg2))

(defun sbb-reg-rm-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-reg-rm-x64 #x18 arg1 arg2 arg3))

(defun sbb-rm-reg-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-rm-reg-x64 #x18 arg1 arg2 arg3))

(defun sbb-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-x64 #x18 arg1 arg2 arg3))

(defun scasb-x86 (&rest args)
  (list #xae))
(defun scasw-x86 (&rest args)
  (list #x66 #xaf))
(defun scasd-x32-x64 (&rest args)
  (list #xaf))
(defun scasq-48-x64 (&rest args)
  (list #x48 #xaf))
(defun scasq-49-x64 (&rest args)
  (list #x49 #xaf))
(defun scasq-4a-x64 (&rest args)
  (list #x4a #xaf))
(defun scasq-4b-x64 (&rest args)
  (list #x4b #xaf))
(defun scasq-4c-x64 (&rest args)
  (list #x4c #xaf))
(defun scasq-4d-x64 (&rest args)
  (list #x4d #xaf))
(defun scasq-4e-x64 (&rest args)
  (list #x4e #xaf))
(defun scasq-4f-x64 (&rest args)
  (list #x4f #xaf))

(defun shl-1-x64 (arg1 &optional arg2 &rest args)
  (one-operand-x64 #xd0 #xe0 arg1 arg2))

(defun shl-cl-x64 (arg1 &optional arg2 &rest args)
  (one-operand-x64 #xd2 #xe0 arg1 arg2))

(defun shr-1-x64 (arg1 &optional arg2 &rest args)
  (one-operand-x64 #xd0 #xe8 arg1 arg2))

(defun shr-cl-x64 (arg1 &optional arg2 &rest args)
  (one-operand-x64 #xd2 #xe8 arg1 arg2))

(defun stc-x86 (&rest args)
  (list #xf9))
(defun std-x86 (&rest args)
  (list #xfd))
(defun sti-x86 (&rest args)
  (list #xfb))
(defun stosb-x86 (&rest args)
  (list #xaa))
(defun stosw-x86 (&rest args)
  (list #x66 #xab))
(defun stosd-x32-x64 (&rest args)
  (list #xab))
(defun stosq-48-x64 (&rest args)
  (list #x48 #xab))
(defun stosq-49-x64 (&rest args)
  (list #x49 #xab))
(defun stosq-4a-x64 (&rest args)
  (list #x4a #xab))
(defun stosq-4b-x64 (&rest args)
  (list #x4b #xab))
(defun stosq-4c-x64 (&rest args)
  (list #x4c #xab))
(defun stosq-4d-x64 (&rest args)
  (list #x4d #xab))
(defun stosq-4e-x64 (&rest args)
  (list #x4e #xab))
(defun stosq-4f-x64 (&rest args)
  (list #x4f #xab))

(defun sub-reg-rm-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-reg-rm-x64 #x28 arg1 arg2 arg3))

(defun sub-rm-reg-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-rm-reg-x64 #x28 arg1 arg2 arg3))

(defun sub-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-x64 #x28 arg1 arg2 arg3))

(defun syscall-x64 (&rest args)
  (list #x0f #x05))

(defun xor-reg-rm-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-reg-rm-x64 #x30 arg1 arg2 arg3))

(defun xor-rm-reg-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-rm-reg-x64 #x30 arg1 arg2 arg3))

(defun xor-x64 (arg1 arg2 &optional arg3 &rest args)
  (arithmetic-x64 #x30 arg1 arg2 arg3))
