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
          (r/m base)
          (ash (r/m index) 3)
          (ash (gethash scale *sib-scale-hash-table-x64*) 6))))

(defun emit-sib-byte-for-memory-address-syntax (memory-address-syntax)
  "This function emits SIB byte for a memory address syntax."
  (let*
    ((base-index-and-scale nil))
    (setf base-index-and-scale (get-base-index-and-scale memory-address-syntax))
    (emit-sib-byte (nth 0 base-index-and-scale)
                   (nth 1 base-index-and-scale)
                   (nth 2 base-index-and-scale))))

(defun emit-modrm-byte (mod regmem reg)
  "This function emits ModRM byte."
  (list (logior (r/m regmem)
                (ash (r/m reg) 3)
                (ash mod 6))))

(defun emit-modrm-byte-for-arithmetic-rm-imm (opcode-base mod arg1)
  "This function emits ModRM (?) byte for r/m,imm for arithmetic instructions."
  (list (logior (r/m arg1)
                opcode-base
                (ash mod 6))))

(defun emit-modrm-byte-for-arithmetic-reg-imm (opcode-base arg1)
  "This function emits ModRM (?) byte for reg,imm for arithmetic instructions."
  (emit-modrm-byte-for-arithmetic-rm-imm opcode-base #b11 arg1))

(defun emit-modrm-byte-for-reg-reg (arg1 arg2)
  "This function emits ModRM for reg,reg."
  (emit-modrm-byte #b11 arg1 arg2))

(defun emit-modrm-byte-for-indirect-without-SIB (arg1 arg2)
  "This function emits ModRM for simple indirect addressing
   (without SIB), using only base. No index, no displacement."
  (emit-modrm-byte #b00 arg1 arg2))

(defun one-operand-x64 (opcode-base reg-byte-base arg1 &optional arg2)
  (cond
    ;; al, cl, dl, bl, ah, ch, dh, bh.
    ((and
       (is-reg arg1)
       (not (needs-rex arg1))
       (eql (reg-size arg1) 8))
     (list opcode-base (logior reg-byte-base (r/m arg1))))
    ;; ax, cx, dx, bx, ax, cx, dx, bx, sp, bp, si, di.
    ((and
       (is-reg arg1)
       (not (needs-rex arg1))
       (eql (reg-size arg1) 16))
     (list #x66 (1+ opcode-base) (logior reg-byte-base (r/m arg1))))
    ;; eax, ecx, edx, ebx, eax, ecx, edx, ebx, esp, ebp, esi, edi.
    ((and
       (is-reg arg1)
       (not (needs-rex arg1))
       (eql (reg-size arg1) 32))
     (list (1+ opcode-base) (logior reg-byte-base (r/m arg1))))
    ;; rax, rcx, rdx, rbx, rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi.
    ((and
       (is-reg arg1)
       (eql (rex.r arg1) 0)
       (eql (reg-size arg1) 64))
     (append (emit-high-even-rex) (list (1+ opcode-base) (logior reg-byte-base (r/m arg1)))))
    ;; spl, bpl, sil, dil.
    ((and
       (is-reg arg1)
       (needs-rex arg1)
       (eql (rex.r arg1) 0)
       (eql (reg-size arg1) 8))
     (append (emit-even-rex) (list opcode-base (logior reg-byte-base (r/m arg1)))))
    ;; r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b.
    ((and
       (is-reg arg1)
       (needs-rex arg1)
       (eql (rex.r arg1) 1)
       (eql (reg-size arg1) 8))
     (append (emit-odd-rex) (list opcode-base (logior reg-byte-base (r/m arg1)))))
    ;; r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w.
    ((and
       (is-reg arg1)
       (needs-rex arg1)
       (eql (reg-size arg1) 16))
     (append (list #x66) (emit-low-odd-rex) (list (1+ opcode-base) (logior reg-byte-base (r/m arg1)))))
    ;; r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d.
    ((and
       (is-reg arg1)
       (needs-rex arg1)
       (eql (reg-size arg1) 32))
     (append (emit-low-odd-rex) (list (1+ opcode-base) (logior reg-byte-base (r/m arg1)))))
    ;; r8, r9, r10, r11, r12, r13, r14, r15.
    ((and
       (is-reg arg1)
       (needs-rex arg1)
       (eql (reg-size arg1) 64))
     (append (emit-high-odd-rex) (list (1+ opcode-base) (logior reg-byte-base (r/m arg1)))))
    (t nil)))

(defun arithmetic-al-imm8-x86 (opcode-base arg1)
  "This function uses AL-specific encoding to encode instruction al,imm8."
  (list (+ opcode-base 4) (parse-integer (name arg1))))

(defun arithmetic-ax-imm16-x64 (opcode-base arg1)
  "This function uses AX-specific encoding to encode instruction ax,imm16."
  (let*
    ((imm16 (parse-integer (name arg1))))
    (list #x66 (+ opcode-base 5) (logand imm16 #xff) (ash imm16 -8))))

(defun arithmetic-eax-imm32-x64 (opcode-base arg1)
  "This function uses EAX-specific encoding to encode instruction eax,imm32."
  (let*
    ((imm32 (parse-integer (name arg1))))
    (list (+ opcode-base 5)
          (logand imm32 #xff)
          (logand (ash imm32 -8) #xff)
          (logand (ash imm32 -16) #xff)
          (logand (ash imm32 -24) #xff))))

(defun arithmetic-rax-imm32-x64 (opcode-base arg1)
  "This function uses RAX-specific encoding to encode instruction rax,imm32."
  (let*
    ((imm32 (parse-integer (name arg1))))
    (append (emit-high-rex)
            (list (+ opcode-base 5)
                  (logand imm32 #xff)
                  (logand (ash imm32 -8) #xff)
                  (logand (ash imm32 -16) #xff)
                  (logand (ash imm32 -24) #xff)))))

(defun arithmetic-rm8-imm8-x64 (opcode-base arg1 arg2 &optional arg3)
  (cond
    ((and
       (is-reg arg1)
       (not (needs-rex arg1))
       (eql (reg-size arg1) 8))
     (append
       (list #x80)
       (emit-modrm-byte-for-arithmetic-reg-imm opcode-base arg1)
       (string-to-8-bit-little-endian (name arg2))))
    ((and
       (is-register-indirect arg1)
       (not (needs-sib arg1))
       (parse-integer (name arg2) :junk-allowed t))
     (append
       (list #x80)
       (emit-modrm-byte-for-arithmetic-rm-imm opcode-base #x00 arg1)
       (string-to-8-bit-little-endian (name arg2))))
    ((and
       (is-register-indirect arg1)
       (not (needs-sib arg1))
       (equalp (name arg2) "byte"))
     (append
       (list #x80)
       (emit-modrm-byte-for-arithmetic-rm-imm opcode-base #x00 arg1)
       (string-to-8-bit-little-endian (name arg3))))
    (t nil)))

(defun arithmetic-rm16-imm16-x64 (opcode-base arg1 arg2 &optional arg3)
  (cond
    ((and
       (is-reg arg1)
       (not (needs-rex arg1))
       (eql (reg-size arg1) 16))
     (append
       (list #x66 #x81)
       (emit-modrm-byte-for-arithmetic-reg-imm opcode-base arg1)
       (string-to-16-bit-little-endian (name arg2))))
    ((and
       (is-register-indirect arg1)
       (not (needs-sib arg1))
       (parse-integer (name arg2) :junk-allowed t))
     (append
       (list #x66 #x81)
       (emit-modrm-byte-for-arithmetic-rm-imm opcode-base #x00 arg1)
       (string-to-16-bit-little-endian (name arg2))))
    ((and
       (is-register-indirect arg1)
       (not (needs-sib arg1))
       (equalp (name arg2) "word"))
     (append
       (list #x80)
       (emit-modrm-byte-for-arithmetic-rm-imm opcode-base #x00 arg1)
       (string-to-16-bit-little-endian arg3)))
    (t nil)))

(defun arithmetic-rm32-imm32-x64 (opcode-base arg1 arg2 &optional arg3)
  (cond
    ((and
       (is-reg arg1)
       (not (needs-rex arg1))
       (eql (reg-size arg1) 32))
     (append
       (list #x81)
       (emit-modrm-byte-for-arithmetic-reg-imm opcode-base arg1)
       (string-to-32-bit-little-endian (name arg2))))
    ((and
       (is-register-indirect arg1)
       (not (needs-sib arg1))
       (parse-integer (name arg2) :junk-allowed t))
     (append
       (list #x81)
       (emit-modrm-byte-for-arithmetic-rm-imm opcode-base #x00 arg1)
       (string-to-32-bit-little-endian (name arg2))))
    ((and
       (is-register-indirect arg1)
       (not (needs-sib arg1))
       (equalp (name arg2) "dword"))
     (append
       (list #x81)
       (emit-modrm-byte-for-arithmetic-rm-imm opcode-base #x00 arg1)
       (string-to-32-bit-little-endian (name arg3))))
    (t nil)))

(defun arithmetic-reg-rm-x64 (opcode-base arg1 arg2 &rest args)
  (cond
    ((and
       (is-reg arg1)
       (not (needs-rex arg1))
       (eql (reg-size arg1) 8)
       (is-reg arg2)
       (not (needs-rex arg2))
       (eql (reg-size arg2) 8))
     ;; Example op-codes for add (method is the same for other instructions too):
     ;; 0x02 can also be used, requires reverse order in ModRM.
     ;; 0x00: add r/m8, r8
     ;; 0x02: add r8, r/m8
     (cons (+ opcode-base 2) (emit-modrm-byte-for-reg-reg arg2 arg1)))
    ((and
       (is-reg arg1)
       (not (needs-rex arg1))
       (eql (reg-size arg1) 16)
       (is-reg arg2)
       (not (needs-rex arg2))
       (eql (reg-size arg2) 16))
     ;; Example op-codes for add (method is the same for other instructions too):
     ;; 0x03 can also be used, requires reverse order in ModRM.
     ;; 0x01: add r/m16, r16
     ;; 0x03: add r16, r/m16
     (append (list #x66 (+ opcode-base 3)) (emit-modrm-byte-for-reg-reg arg2 arg1)))
    ((and
       (is-reg arg1)
       (not (needs-rex arg1))
       (eql (reg-size arg1) 32)
       (is-reg arg2)
       (not (needs-rex arg2))
       (eql (reg-size arg2) 32))
     ;; Example op-codes for add (method is the same for other instructions too):
     ;; 0x03 can also be used, requires reverse order in ModRM.
     ;; 0x01: add r/m32, r32
     ;; 0x03: add r32, r/m32
     (cons (+ opcode-base 3) (emit-modrm-byte-for-reg-reg arg2 arg1)))
    ((and
       (is-reg arg1)
       (not (needs-rex arg1))
       (eql (reg-size arg1) 8)
       (is-register-indirect arg2)
       (not (needs-sib arg2)))
     (cons (+ opcode-base 2) (emit-modrm-byte-for-indirect-without-SIB arg2 arg1)))
    ((and
       (is-reg arg1)
       (not (needs-rex arg1))
       (eql (reg-size arg1) 16)
       (is-register-indirect arg2)
       (not (needs-sib arg2)))
     (append (list #x66 (+ opcode-base 3)) (emit-modrm-byte-for-indirect-without-SIB arg2 arg1)))
    ((and
       (is-reg arg1)
       (not (needs-rex arg1))
       (eql (reg-size arg1) 32)
       (is-register-indirect arg2)
       (not (needs-sib arg2)))
     (cons (+ opcode-base 3) (emit-modrm-byte-for-indirect-without-SIB arg2 arg1)))
    (t nil)))

(defun arithmetic-rm-reg-x64 (opcode-base arg1 arg2)
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
       ;; Example op-codes for add (method is the same for other instructions too):
       ;; 0x02 can also be used, requires reverse order in ModRM.
       ;; 0x00: add r/m8, r8
       ;; 0x02: add r8, r/m8
       (cons opcode-base (emit-modrm-byte-for-reg-reg arg1 arg2)))
      ((and (equal arg1-reg-type "old-16-bit-reg")
            (equal arg2-reg-type "old-16-bit-reg"))
       ;; Example op-codes for add (method is the same for other instructions too):
       ;; 0x03 can also be used, requires reverse order in ModRM.
       ;; 0x01: add r/m16, r16
       ;; 0x03: add r16, r/m16
       (append (list #x66 (1+ opcode-base)) (emit-modrm-byte-for-reg-reg arg1 arg2)))
      ((and (equal arg1-reg-type "old-32-bit-reg")
            (equal arg2-reg-type "old-32-bit-reg"))
       ;; Example op-codes for add (method is the same for other instructions too):
       ;; 0x03 can also be used, requires reverse order in ModRM.
       ;; 0x01: add r/m32, r32
       ;; 0x03: add r32, r/m32
       (cons (1+ opcode-base) (emit-modrm-byte-for-reg-reg arg1 arg2)))
      ((and
         (equal arg1-reg-type "register-indirect-without-SIB")
         (or
           (equal arg2-reg-type "old-8-bit-low-reg")
           (equal arg2-reg-type "old-8-bit-high-reg")))
       (cons opcode-base (emit-modrm-byte-for-indirect-without-SIB arg1 arg2)))
      ((and
         (equal arg1-reg-type "register-indirect-without-SIB")
         (equal arg2-reg-type "old-16-bit-reg"))
       (append (list #x66 (1+ opcode-base)) (emit-modrm-byte-for-indirect-without-SIB arg1 arg2)))
      ((and
         (equal arg1-reg-type "register-indirect-without-SIB")
         (equal arg2-reg-type "old-32-bit-reg"))
       (cons (1+ opcode-base) (emit-modrm-byte-for-indirect-without-SIB arg1 arg2)))
      (t nil))))

(defun arithmetic-x64 (opcode-base arg1 arg2 &optional arg3)
  ;; Current default usage:
  ;; Use `defun arithmetic-rm-reg-x64` always when you can.
  ;; Use `defun arithmetic-al-imm8-x86` always when you can.
  ;; Use `defun arithmetic-ax-imm16-x64` always when you can.
  ;; Use `defun arithmetic-eax-imm32-x64` always when you can.
  ;; Use `defun arithmetic-rax-imm32-x64` always when you can.
  ;; Use `defun arithmetic-reg-rm-x64` only if source is a register indirect without SIB.
  (cond
    ((and
       (equal (gethash (gethash arg1 *reg-type-hash-table-x64*) *is-register-hash-table-x64*) "is-register")
       (equal (gethash (gethash arg2 *reg-type-hash-table-x64*) *is-register-hash-table-x64*) "is-register"))
     ;; this could be also (arithmetic-reg-rm-x64 opcode-base arg1 arg2 arg3) .
     (arithmetic-rm-reg-x64 opcode-base arg1 arg2))
    ((and
       (equal (gethash (gethash arg1 *reg-type-hash-table-x64*) *is-register-hash-table-x64*) "is-register")
       (equal (gethash arg2 *reg-type-hash-table-x64*) "register-indirect-without-SIB"))
     ;; no alternatives available here, except using SIB.
     (arithmetic-reg-rm-x64 opcode-base arg1 arg2))
    ((and
       (equal (gethash arg1 *reg-type-hash-table-x64*) "register-indirect-without-SIB")
       (equal (gethash (gethash arg2 *reg-type-hash-table-x64*) *is-register-hash-table-x64*) "is-register"))
     ;; no alternatives available here, except using SIB.
     (arithmetic-rm-reg-x64 opcode-base arg1 arg2))
    ((and
       (equalp arg1 "al")
       (equalp arg2 "byte"))
     ;; could use also `arithmetic-rm8-imm8-x64` instead of AL-specific encoding.
     (arithmetic-al-imm8-x86 opcode-base arg3))
    ((and
       (equalp arg1 "al")
       (parse-integer arg2 :junk-allowed t))
     ;; could use also `arithmetic-rm8-imm8-x64` instead of AL-specific encoding.
     (arithmetic-al-imm8-x86 opcode-base arg2))
    ((and
       (equalp arg1 "ax")
       (equalp arg2 "word"))
     ;; could use also `arithmetic-rm16-imm16-x64` instead of AX-specific encoding.
     (arithmetic-ax-imm16-x64 opcode-base arg3))
    ((and
       (equalp arg1 "ax")
       (parse-integer arg2 :junk-allowed t))
     ;; could use also `arithmetic-rm16-imm16-x64` instead of AX-specific encoding.
     (arithmetic-ax-imm16-x64 opcode-base arg2))
    ((and
       (equalp arg1 "eax")
       (equalp arg2 "dword"))
     ;; could use also `arithmetic-rm32-imm32-x64` instead of EAX-specific encoding.
     (arithmetic-eax-imm32-x64 opcode-base arg3))
    ((and
       (equalp arg1 "eax")
       (parse-integer arg2 :junk-allowed t))
     ;; could use also `arithmetic-rm32-imm32-x64` instead of EAX-specific encoding.
     (arithmetic-eax-imm32-x64 opcode-base arg2))
    ((and
       (equalp arg1 "rax")
       (equalp arg2 "dword"))
     ;; could use also ...
     (arithmetic-rax-imm32-x64 opcode-base arg3))
    ((and
       (equalp arg1 "rax")
       (parse-integer arg2 :junk-allowed t))
     ;; could use also ...
     (arithmetic-rax-imm32-x64 opcode-base arg2))
    ((and
       (or
         (equal (gethash arg1 *reg-type-hash-table-x64*) "old-8-bit-low-reg")
         (equal (gethash arg1 *reg-type-hash-table-x64*) "old-8-bit-high-reg"))
       (parse-integer arg2 :junk-allowed t))
     ;; no alternatives available here.
     (arithmetic-rm8-imm8-x64 opcode-base arg1 arg2))
    ((and
       (equal (gethash arg1 *reg-type-hash-table-x64*) "old-16-bit-reg")
       (parse-integer arg2 :junk-allowed t))
     ;; no alternatives available here.
     (arithmetic-rm16-imm16-x64 opcode-base arg1 arg2))
    ((and
       (equal (gethash arg1 *reg-type-hash-table-x64*) "old-32-bit-reg")
       (parse-integer arg2 :junk-allowed t))
     ;; no alternatives available here.
     (arithmetic-rm32-imm32-x64 opcode-base arg1 arg2))
    ((and
       (or
         (equal (gethash arg1 *reg-type-hash-table-x64*) "register-indirect-without-SIB")
         (equal (gethash arg1 *reg-type-hash-table-x64*) "old-8-bit-low-reg")
         (equal (gethash arg1 *reg-type-hash-table-x64*) "old-8-bit-high-reg"))
       (equal arg2 "byte"))
     ;; no alternatives available here.
     (arithmetic-rm8-imm8-x64 opcode-base arg1 arg3))
    ((and
       (or
         (equal (gethash arg1 *reg-type-hash-table-x64*) "register-indirect-without-SIB")
         (equal (gethash arg1 *reg-type-hash-table-x64*) "old-16-bit-reg"))
       (equal arg2 "word"))
     ;; no alternatives available here.
     (arithmetic-rm16-imm16-x64 opcode-base arg1 arg3))
    ((and
       (or
         (equal (gethash arg1 *reg-type-hash-table-x64*) "register-indirect-without-SIB")
         (equal (gethash arg1 *reg-type-hash-table-x64*) "old-32-bit-reg"))
       (equal arg2 "dword"))
     ;; no alternatives available here.
     (arithmetic-rm32-imm32-x64 opcode-base arg1 arg3))
    (t nil)))
