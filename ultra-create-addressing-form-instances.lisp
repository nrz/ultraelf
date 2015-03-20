;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

;; create register and register-indirect addressing form instances.
;; note: indirect addressing form that use `[` & `]` need backslash in SBCL REPL,
;; eg: ULTRAELF> \[rax\]
(loop for register-list in *create-addressing-form-instances-list*
      do (loop for register-index below (length (second register-list))
               do (setf (symbol-value (intern (string-upcase (nth register-index (second register-list)))))
                        (make-instance (third register-list)
                                       :r/m (+ register-index (first register-list))
                                       :name (string-upcase (nth register-index (second register-list)))))))

;; create SIB addressing instances.
;; ultraELF uses slightly extended Intel syntax (from here on ultraELF syntax).
;; below examples are using x64 (x86-64) encoding, but the same principles apply to all relevant architectures.
;;
;; design goals of ultraELF syntax:
;;  1. surjective function from assembly code to binary code (that is, every possible binary encoding has a distinct assembly syntax).
;;  2. injective non-surjective function from binary code to assemble code (that is, every possible binary encoding has a distinct assembly syntax).
;;  3. possibility to leave out all operand size where it is normally required, allowing several different encodings (eg. `mov [rsi],3`).
;;  4. possibility to define more than 1 operand size, allowing several different encodings (eg. `mov [rsi], byte/word 3`).
;;  5. possibility to hardcode displacements as constant values (useful for: 1. overlapping instructions, 2. code/data mixes).
;;  6. possibility to split code into code blocks that can be reordered, the blocks usually ending with `ret` or `jmp?` (jump if the next block is not next).
;;  7. possibility to use Common Lisp as macro language (every `(` in code opens a Common Lisp form that is `read` and `eval`ed).
;;  8. possibility to define instruction-level constraints (for example: `(contains (list 0bff 0b00))` ), permitting the use of ultraELF regular expressions.
;;  9. possibility to define embedded instructions recursively (overlapping instructions given as assembly code).
;; 10. possibility to define code block-level constraints.
;; 11. possibility to define program-level constraints.
;; 12. maintain compatibility with NASM to the extent possible: ultraELF should assemble all valid NASM-syntax assembly code identical to NASM.
;;
;; keywords specific to ultraELF:
;; `pc`     program counter specific addressing (such as RIP-relative addressing) must be used.
;; `sib`    SIB must be used in encoding.
;; `nosib`  SIB must not be used in encoding.
;; `disp8`  displacement size must be 8 bits.
;; `disp32` displacement size must be 32 bits.
;; `scale`  scale: 1, 2, 4 or 8
;; `ss`     scale bits: 0b00 (scale=1), 0b01 (scale=2), 0b10 (scale=4), 0b11 (scale=8).
;; `rex`    REX must be used in encoding.
;; `norex`  REX must not be used in encoding.
;;
;; ultraELF syntax                produces                  Intel *1    Intel *2            NASM syntax         my own explanation
;; `inc dword [rax]`              FF 00                     FF /0       [rax]               `inc dword [rax]`   reg. indir., no disp.
;; `inc dword rex [rax]`          40 FF 00                  FF /0       n/a *3              n/a                 reg. indir., no disp., REX.
;; `inc dword [rax+0]`            FF 40 00                  FF /0       n/a *4              n/a                 reg. indir., any disp. size (8 bits), disp. 0.
;; `inc dword [rax-0]`            FF 40 00                  FF /0       n/a *4              n/a                 reg. indir., any disp. size (8 bits), disp. 0.
;; `inc dword rex [rax+0]`        40 FF 40 00               FF /0       n/a *4              n/a                 reg. indir., any disp. size (8 bits), disp. 0, REX.
;; `inc dword [rax+disp8 0]`      FF 40 00                  FF /0       [rax]+disp8         n/a                 reg. indir., disp. size 8 bits,  disp. 0.
;; `inc dword [rax-disp8 0]`      FF 40 00                  FF /0       [rax]+disp8         n/a                 reg. indir., disp. size 8 bits,  disp. 0.
;; `inc dword [rax+disp32 0]`     FF 80 00 00 00 00         FF /0       [rax]+disp32        n/a                 reg. indir., disp. size 32 bits, disp. 0.
;; `inc dword [rax-disp32 0]`     FF 80 00 00 00 00         FF /0       [rax]+disp32        n/a                 reg. indir., disp. size 32 bits, disp. 0.
;; `inc dword rex [rax+disp32 0]` 40 FF 80 00 00 00 00      FF /0       [rax]+disp32        n/a                 reg. indir., disp. size 32 bits, disp. 0, REX.
;; `inc dword [sib 0]`            FF 04 25 00 00 00 00      FF /0       none, none, SS=01   inc dword [0x0]     SIB direct, any disp. size (32 bits), disp. 0.
;; `inc dword rex [sib 0]`        40 FF 04 25 00 00 00 00   FF /0       none, none, SS=01   n/a                 SIB direct, any disp. size (32 bits), disp. 0, REX.
;; `inc dword [++0]`              FF 04 25 00 00 00 00      FF /0       none, none, SS=01   inc dword [0x0]     SIB direct, any disp. size (32 bits), disp. 0.
;; `inc dword [+-0]`              FF 04 25 00 00 00 00      FF /0       none, none, SS=01   inc dword [0x0]     SIB direct, any disp. size (32 bits), disp. 0.
;; `inc dword [sib disp32 0]`     FF 04 25 00 00 00 00      FF /0       none, none, SS=01   inc dword [0x0]     SIB direct, displacement size 32 bits),disp. 0.
;; `inc dword [++disp32 0]`       FF 04 25 00 00 00 00      FF /0       none, none, SS=01   inc dword [0x0]     SIB direct, displacement size 32 bits),disp. 0.
;; `inc dword [sib disp8 0]`      invalid                   n/a         n/a                 n/a                 invalid (there is no SIB direct with disp. size 8 bits.
;; `inc dword [++disp8 0]`        invalid                   n/a         n/a                 n/a                 invalid (there is no SIB direct with disp. size 8 bits.
;; `inc dword [++disp32 0]`       SIB register indirect, displacement size 32 bits, displacement 0
;; `inc dword [sib rax]`          SIB register indirect, rax as base
;; `inc dword [sib rax+]`         SIB register indirect, rax as base
;; `inc dword [sib rax++]`        SIB register indirect, rax as base, any displacement size, displacement 0
;; `inc dword [sib +rax]`         SIB register indirect, rax as index
;; `inc dword [sib +rax+]`        SIB register indirect, rax as index
;; `inc dword [rax+]`             SIB register indirect, rax as base
;; `inc dword [rax++0]`           SIB register indirect, rax as base, any displacement size, displacement 0
;; `inc dword [rax++disp8 0]`     SIB register indirect, rax as base, displacement size 8 bits, displacement 0
;; `inc dword [rax++disp32 0]`    SIB register indirect, rax as base, displacement size 32 bits, displacement 0
;; `inc dword [sib +rax]`         SIB register indirect, rax as index
;; `inc dword [+rax+]`            SIB register indirect, rax as index
;; `inc dword [+rax+0]`           SIB register indirect, rax as index, any displacement size, displacement 0
;; `inc dword [+rax+disp8 0]`     SIB register indirect, rax as index, displacement size 8 bits, displacement 0
;; `inc dword [+rax+disp32 0]`    SIB register indirect, rax as index, displacement size 32 bits, displacement 0
;; `inc dword [rax+disp]`         base, no SIB, uses disp8/disp32
;; `inc dword [rax+disp 0]`       base, no SIB, uses disp8/disp32
;; `inc dword [rax+disp8]`        base, no SIB, uses disp8
;; `inc dword [rax+disp32]`       base, no SIB, uses disp8
;; `inc dword [rax+]`             base, uses SIB
;; `inc dword [0]`                basic direct, displacement size 32 bits, displacement 0
;; `inc dword [+0]`               basic direct, displacement size 32 bits, displacement 0
;; `inc dword [-0]`               basic direct, displacement size 32 bits, displacement 0
;;
