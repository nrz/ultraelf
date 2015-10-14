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
;; design goals of ultraELF syntax (in order of descending importance):
;;  1. surjective function from assembly code to binary code (that is, every possible binary encoding has a distinct assembly syntax).
;;  2. injective non-surjective function from binary code to assemble code (that is, every possible binary encoding has a distinct assembly syntax).
;;  3. possibility to hardcode displacements as constant values (useful for: 1. overlapping instructions, 2. code/data mixes).
;;  4. possibility to split code into code blocks that can be reordered, the blocks usually ending with `ret` or `jmp?` (jump if the next block is not next).
;;  5. possibility to use Common Lisp as macro language (every `(` in code opens a Common Lisp form that is `read` and `eval`ed).
;;  6. possibility to define instruction-level constraints (for example: `(contains (list 0bff 0b00))` ), permitting the use of ultraELF regular expressions.
;;  7. possibility to define embedded instructions recursively (overlapping instructions given as assembly code).
;;  8. possibility to define code block-level constraints.
;;  9. possibility to define program-level constraints.
;; 10. maintain compatibility with NASM to the extent possible: ultraELF should assemble all NASM-syntax assembly code identical to NASM.
;;
;; keywords specific to ultraELF:
;; `pc`     program counter specific addressing (such as RIP-relative addressing) must be used.
;; `sib`    SIB must be used in encoding.
;; `nosib`  SIB must not be used in encoding.
;; `null`   empty base or index in SIB (if `null` is used as index, it can be scaled).
;; `disp`   there must be a displacement.
;; `disp8`  there must be a displacement encoded in 8 bits.
;; `disp32` there must be a displacement encoded in  32 bits.
;; `nodisp` there must not be a displacement.
;; `scale`  scale: 1, 2, 4 or 8
;; `ss`     scale bits: 0b00 (scale=1), 0b01 (scale=2), 0b10 (scale=4), 0b11 (scale=8).
;; `rex`    REX must be used in encoding.
;; `norex`  REX must not be used in encoding.
;;
;; In native mode (default), ultraELF disassembler should always produce 'full' information disassembly
;; (with `rex`/`norex`, `sib`/`nosib`, `null`, `disp8`/`disp32` as needed).
;; In NDISASM mode, ultraELF disassembler should produce disassembly identical to the one produced by NDISASM.
;;
;; ultraELF syntax                produces                 OK? Intel*1 Intel*2            NASM syntax        my own explanation
;; `inc dword [rax]`              FF 00                    [ ] FF /0   [rax]              `inc dword [rax]`  reg. indir., no disp.
;; `inc dword rex [rax]`          40 FF 00                 [ ] FF /0   n/a *3             n/a                reg. indir., no disp., REX.
;; `inc dword [rax+0]`            FF 40 00                 [ ] FF /0   n/a *4             n/a                reg. indir., any disp. size (8 bits), disp. 0.
;; `inc dword [rax-0]`            FF 40 00                 [ ] FF /0   n/a *4             n/a                reg. indir., any disp. size (8 bits), disp. 0.
;; `inc dword [rax+disp]`         FF 40 00                 [ ] FF /0   n/a *4             n/a                reg. indir., any disp. size (8 bits), disp. 0.
;; `inc dword [rax-disp]`         FF 40 00                 [ ] FF /0   n/a *4             n/a                reg. indir., any disp. size (8 bits), disp. 0.
;; `inc dword [rax+disp 0]`       FF 40 00                 [ ] FF /0   n/a *4             n/a                reg. indir., any disp. size (8 bits), disp. 0.
;; `inc dword rex [rax+0]`        40 FF 40 00              [ ] FF /0   n/a *4             n/a                reg. indir., any disp. size (8 bits), disp. 0, REX.
;; `inc dword [rax+disp8]`        FF 40 00                 [ ] FF /0   [rax]+disp8        n/a                reg. indir., disp. size 8 bits,  disp. 0.
;; `inc dword [rax+disp8 0]`      FF 40 00                 [ ] FF /0   [rax]+disp8        n/a                reg. indir., disp. size 8 bits,  disp. 0.
;; `inc dword [rax-disp8 0]`      FF 40 00                 [ ] FF /0   [rax]+disp8        n/a                reg. indir., disp. size 8 bits,  disp. 0.
;; `inc dword [rax+disp32 0]`     FF 80 00 00 00 00        [ ] FF /0   [rax]+disp32       n/a                reg. indir., disp. size 32 bits, disp. 0.
;; `inc dword [rax-disp32 0]`     FF 80 00 00 00 00        [ ] FF /0   [rax]+disp32       n/a                reg. indir., disp. size 32 bits, disp. 0.
;; `inc dword rex [rax+disp32 0]` 40 FF 80 00 00 00 00     [ ] FF /0   [rax]+disp32       n/a                reg. indir., disp. size 32 bits, disp. 0, REX.
;; `inc dword [sib 0]`            FF 04 25 00 00 00 00     [ ] FF /0   none, none, SS=01  inc dword [0x0]    SIB direct, any disp. size (32 bits), disp. 0.
;; `inc dword [null+null+0]`      FF 04 25 00 00 00 00     [ ] FF /0   none, none, SS=01  inc dword [0x0]    SIB direct, any disp. size (32 bits), disp. 0.
;; `inc dword rex [sib 0]`        40 FF 04 25 00 00 00 00  [ ] FF /0   none, none, SS=01  n/a                SIB direct, any disp. size (32 bits), disp. 0, REX.
;; `inc dword [++0]`              FF 04 25 00 00 00 00     [ ] FF /0   none, none, SS=01  inc dword [0x0]    SIB direct, any disp. size (32 bits), disp. 0.
;; `inc dword [+-0]`              FF 04 25 00 00 00 00     [ ] FF /0   none, none, SS=01  inc dword [0x0]    SIB direct, any disp. size (32 bits), disp. 0.
;; `inc dword [sib disp32 0]`     FF 04 25 00 00 00 00     [ ] FF /0   none, none, SS=01  inc dword [0x0]    SIB direct, displacement size 32 bits), disp. 0.
;; `inc dword [++disp32 0]`       FF 04 25 00 00 00 00     [ ] FF /0   none, none, SS=01  inc dword [0x0]    SIB direct, displacement size 32 bits), disp. 0.
;; `inc dword [sib disp8 0]`      invalid                  [ ] n/a     n/a                n/a                invalid (there is no SIB direct with disp. size 8 bits.
;; `inc dword [++disp8 0]`        invalid                  [ ] n/a     n/a                n/a                invalid (there is no SIB direct with disp. size 8 bits.
;; `inc dword [rax+]`             FF 04 20                 [ ] FF /0   n/a                n/a                SIB register indirect, rax as base.
;;                                                                                                           Note: `+` without displacement refers to a register,
;;                                                                                                           in this case to an index register, as the order is
;;                                                                                                           base+index+displacement. `+` without displacement
;;                                                                                                           implies the use of SIB.
;; `inc dword [sib rax]`          FF 04 20                 [ ] FF /0   n/a                n/a                SIB register indirect, rax as base.
;; `inc dword [sib rax+]`         FF 04 20                 [ ] FF /0   n/a                n/a                SIB register indirect, rax as base.
;; `inc dword [sib rax++]`        FF 44 20 00              [ ] FF /0   [rax]+disp8        n/a                SIB reg. indir., rax as base, any disp. size, disp. 0.
;; `inc dword [rax++0]`           FF 44 20 00              [ ] FF /0   [rax]+disp8        n/a                SIB reg. indir., rax as base, any disp. size, disp. 0.
;; `inc dword [sib disp8 rax++]`  FF 44 20 00              [ ] FF /0   [rax]+disp8        n/a                SIB reg. indir., rax as base, disp. size 8, disp. 0.
;; `inc dword [sib disp32 rax++]` FF 84 20 00 00 00 00     [ ] FF /0   [rax]+disp32       n/a                SIB reg. indir., rax as base, disp. size 32, disp. 0.
;; `inc dword [+rax]`             FF 04 05 00 00 00 00     [ ] FF /0   [rax]+disp32       n/a                SIB reg. indir., rax as index, any disp. size, disp. 0.
;; `inc dword [null+rax]`         FF 04 05 00 00 00 00     [ ] FF /0   [rax]+disp32       n/a                SIB reg. indir., rax as index, any disp. size, disp. 0.
;; `inc dword [sib +rax]`         FF 04 05 00 00 00 00     [ ] FF /0   [rax]+disp32       n/a                SIB reg. indir., rax as index, any disp. size, disp. 0.
;; `inc dword [+rax+]`            FF 04 05 00 00 00 00     [ ] FF /0   [rax]+disp32       n/a                SIB reg. indir., rax as index, any disp. size, disp. 0.
;; `inc dword [+rax+0]`           FF 04 05 00 00 00 00     [ ] FF /0   [rax]+disp32       n/a                SIB reg. indir., rax as index, any disp. size, disp. 0.
;; `inc dword [+rax-0]`           FF 04 05 00 00 00 00     [ ] FF /0   [rax]+disp32       n/a                SIB reg. indir., rax as index, any disp. size, disp. 0.
;; `inc dword [sib +rax+]`        FF 04 05 00 00 00 00     [ ] FF /0   [rax]+disp32       n/a                SIB reg. indir., rax as index, any disp. size, disp. 0.
;; `inc dword [rax++disp8 0]`     FF 44 20 00              [ ] FF /0   [rax]+disp8        n/a                SIB reg. indir., rax as base, disp. size 8 bits, disp. 0.
;; `inc dword [rax++disp32 0]`    FF 84 20 00 00 00 00     [ ] FF /0   [rax]+disp32       n/a                SIB reg. indir., rax as base, disp. size 32 bits, disp. 0.
;; `inc dword [+rax+disp8 0]`     invalid                  [ ] n/a     n/a                n/a                invalid (there is no SIB reg. indirect, rax as index, disp. size 8 bits, displacement 0).
;; `inc dword [+rax+disp32 0]`    FF 04 05 00 00 00 00     [ ] FF /0   [rax]+disp32       n/a                SIB reg. indir., rax as index, any disp. size, disp. 0.
;; `inc dword [0]`                FF 05 00 00 00 00        [ ] FF /0   n/a                n/a                x32: basic direct, disp. size 32, disp. 0.
;; `inc dword [+0]`               FF 05 00 00 00 00        [ ] FF /0   n/a                n/a                x32: basic direct, disp. size 32, disp. 0.
;; `inc dword [-0]`               FF 05 00 00 00 00        [ ] FF /0   n/a                n/a                x32: basic direct, disp. size 32, disp. 0.
;; `inc dword [rip]`              FF 05 00 00 00 00        [ ] FF /0   n/a                n/a                x64: RIP-relative, disp. size 32, disp. 0.
;; `inc dword [pc]`               FF 05 00 00 00 00        [ ] FF /0   n/a                n/a                x64: RIP-relative, disp. size 32, disp. 0.
;; `inc dword [rip+0]`            FF 05 00 00 00 00        [ ] FF /0   n/a                n/a                x64: RIP-relative, disp. size 32, disp. 0.
;; `inc dword [rip-0]`            FF 05 00 00 00 00        [ ] FF /0   n/a                n/a                x64: RIP-relative, disp. size 32, disp. 0.
;; `inc dword [pc+0]``            FF 05 00 00 00 00        [ ] FF /0   n/a                n/a                x64: RIP-relative, disp. size 32, disp. 0.
;; `inc dword [pc-0]`             FF 05 00 00 00 00        [ ] FF /0   n/a                n/a                x64: RIP-relative, disp. size 32, disp. 0.
