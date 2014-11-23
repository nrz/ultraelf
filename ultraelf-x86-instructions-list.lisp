;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(eval-when (:compile-toplevel :load-toplevel :execute)
(setf DB-ignore (make-instance 'x86-asm-instruction
:name "DB"
:operands "ignore"
:code-string "ignore"
:arch-flags (list "ignore")))

(setf DW-ignore (make-instance 'x86-asm-instruction
:name "DW"
:operands "ignore"
:code-string "ignore"
:arch-flags (list "ignore")))

(setf DD-ignore (make-instance 'x86-asm-instruction
:name "DD"
:operands "ignore"
:code-string "ignore"
:arch-flags (list "ignore")))

(setf DQ-ignore (make-instance 'x86-asm-instruction
:name "DQ"
:operands "ignore"
:code-string "ignore"
:arch-flags (list "ignore")))

(setf DT-ignore (make-instance 'x86-asm-instruction
:name "DT"
:operands "ignore"
:code-string "ignore"
:arch-flags (list "ignore")))

(setf DO-ignore (make-instance 'x86-asm-instruction
:name "DO"
:operands "ignore"
:code-string "ignore"
:arch-flags (list "ignore")))

(setf DY-ignore (make-instance 'x86-asm-instruction
:name "DY"
:operands "ignore"
:code-string "ignore"
:arch-flags (list "ignore")))

(setf DZ-ignore (make-instance 'x86-asm-instruction
:name "DZ"
:operands "ignore"
:code-string "ignore"
:arch-flags (list "ignore")))

(setf RESB-imm (make-instance 'x86-asm-instruction
:name "RESB"
:operands "imm"
:code-string "[ resb]"
:arch-flags (list "8086")))

(setf RESW-ignore (make-instance 'x86-asm-instruction
:name "RESW"
:operands "ignore"
:code-string "ignore"
:arch-flags (list "ignore")))

(setf RESD-ignore (make-instance 'x86-asm-instruction
:name "RESD"
:operands "ignore"
:code-string "ignore"
:arch-flags (list "ignore")))

(setf RESQ-ignore (make-instance 'x86-asm-instruction
:name "RESQ"
:operands "ignore"
:code-string "ignore"
:arch-flags (list "ignore")))

(setf REST-ignore (make-instance 'x86-asm-instruction
:name "REST"
:operands "ignore"
:code-string "ignore"
:arch-flags (list "ignore")))

(setf RESO-ignore (make-instance 'x86-asm-instruction
:name "RESO"
:operands "ignore"
:code-string "ignore"
:arch-flags (list "ignore")))

(setf RESY-ignore (make-instance 'x86-asm-instruction
:name "RESY"
:operands "ignore"
:code-string "ignore"
:arch-flags (list "ignore")))

(setf RESZ-ignore (make-instance 'x86-asm-instruction
:name "RESZ"
:operands "ignore"
:code-string "ignore"
:arch-flags (list "ignore")))

(setf ADC-mem.reg8 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "mem,reg8"
:code-string "[mr: hle 10 /r]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf ADC-reg8.reg8 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "reg8,reg8"
:code-string "[mr: 10 /r]"
:arch-flags (list "8086")))

(setf ADC-mem.reg16 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "mem,reg16"
:code-string "[mr: hle o16 11 /r]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf ADC-reg16.reg16 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "reg16,reg16"
:code-string "[mr: o16 11 /r]"
:arch-flags (list "8086")))

(setf ADC-mem.reg32 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "mem,reg32"
:code-string "[mr: hle o32 11 /r]"
:arch-flags (list "386" "SM" "LOCK")))

(setf ADC-reg32.reg32 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "reg32,reg32"
:code-string "[mr: o32 11 /r]"
:arch-flags (list "386")))

(setf ADC-mem.reg64 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "mem,reg64"
:code-string "[mr: hle o64 11 /r]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf ADC-reg64.reg64 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "reg64,reg64"
:code-string "[mr: o64 11 /r]"
:arch-flags (list "X64")))

(setf ADC-reg8.mem (make-instance 'x86-asm-instruction
:name "ADC"
:operands "reg8,mem"
:code-string "[rm: 12 /r]"
:arch-flags (list "8086" "SM")))

(setf ADC-reg8.reg8 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "reg8,reg8"
:code-string "[rm: 12 /r]"
:arch-flags (list "8086")))

(setf ADC-reg16.mem (make-instance 'x86-asm-instruction
:name "ADC"
:operands "reg16,mem"
:code-string "[rm: o16 13 /r]"
:arch-flags (list "8086" "SM")))

(setf ADC-reg16.reg16 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "reg16,reg16"
:code-string "[rm: o16 13 /r]"
:arch-flags (list "8086")))

(setf ADC-reg32.mem (make-instance 'x86-asm-instruction
:name "ADC"
:operands "reg32,mem"
:code-string "[rm: o32 13 /r]"
:arch-flags (list "386" "SM")))

(setf ADC-reg32.reg32 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "reg32,reg32"
:code-string "[rm: o32 13 /r]"
:arch-flags (list "386")))

(setf ADC-reg64.mem (make-instance 'x86-asm-instruction
:name "ADC"
:operands "reg64,mem"
:code-string "[rm: o64 13 /r]"
:arch-flags (list "X64" "SM")))

(setf ADC-reg64.reg64 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "reg64,reg64"
:code-string "[rm: o64 13 /r]"
:arch-flags (list "X64")))

(setf ADC-rm16.imm8 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "rm16,imm8"
:code-string "[mi: hle o16 83 /2 ib,s]"
:arch-flags (list "8086" "LOCK")))

(setf ADC-rm32.imm8 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "rm32,imm8"
:code-string "[mi: hle o32 83 /2 ib,s]"
:arch-flags (list "386" "LOCK")))

(setf ADC-rm64.imm8 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "rm64,imm8"
:code-string "[mi: hle o64 83 /2 ib,s]"
:arch-flags (list "X64" "LOCK")))

(setf ADC-reg_al.imm (make-instance 'x86-asm-instruction
:name "ADC"
:operands "reg_al,imm"
:code-string "[-i: 14 ib]"
:arch-flags (list "8086" "SM")))

(setf ADC-reg_ax.sbyteword (make-instance 'x86-asm-instruction
:name "ADC"
:operands "reg_ax,sbyteword"
:code-string "[mi: o16 83 /2 ib,s]"
:arch-flags (list "8086" "SM" "ND")))

(setf ADC-reg_ax.imm (make-instance 'x86-asm-instruction
:name "ADC"
:operands "reg_ax,imm"
:code-string "[-i: o16 15 iw]"
:arch-flags (list "8086" "SM")))

(setf ADC-reg_eax.sbytedword (make-instance 'x86-asm-instruction
:name "ADC"
:operands "reg_eax,sbytedword"
:code-string "[mi: o32 83 /2 ib,s]"
:arch-flags (list "386" "SM" "ND")))

(setf ADC-reg_eax.imm (make-instance 'x86-asm-instruction
:name "ADC"
:operands "reg_eax,imm"
:code-string "[-i: o32 15 id]"
:arch-flags (list "386" "SM")))

(setf ADC-reg_rax.sbytedword (make-instance 'x86-asm-instruction
:name "ADC"
:operands "reg_rax,sbytedword"
:code-string "[mi: o64 83 /2 ib,s]"
:arch-flags (list "X64" "SM" "ND")))

(setf ADC-reg_rax.imm (make-instance 'x86-asm-instruction
:name "ADC"
:operands "reg_rax,imm"
:code-string "[-i: o64 15 id,s]"
:arch-flags (list "X64" "SM")))

(setf ADC-rm8.imm (make-instance 'x86-asm-instruction
:name "ADC"
:operands "rm8,imm"
:code-string "[mi: hle 80 /2 ib]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf ADC-rm16.sbyteword (make-instance 'x86-asm-instruction
:name "ADC"
:operands "rm16,sbyteword"
:code-string "[mi: hle o16 83 /2 ib,s]"
:arch-flags (list "8086" "SM" "LOCK" "ND")))

(setf ADC-rm16.imm (make-instance 'x86-asm-instruction
:name "ADC"
:operands "rm16,imm"
:code-string "[mi: hle o16 81 /2 iw]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf ADC-rm32.sbytedword (make-instance 'x86-asm-instruction
:name "ADC"
:operands "rm32,sbytedword"
:code-string "[mi: hle o32 83 /2 ib,s]"
:arch-flags (list "386" "SM" "LOCK" "ND")))

(setf ADC-rm32.imm (make-instance 'x86-asm-instruction
:name "ADC"
:operands "rm32,imm"
:code-string "[mi: hle o32 81 /2 id]"
:arch-flags (list "386" "SM" "LOCK")))

(setf ADC-rm64.sbytedword (make-instance 'x86-asm-instruction
:name "ADC"
:operands "rm64,sbytedword"
:code-string "[mi: hle o64 83 /2 ib,s]"
:arch-flags (list "X64" "SM" "LOCK" "ND")))

(setf ADC-rm64.imm (make-instance 'x86-asm-instruction
:name "ADC"
:operands "rm64,imm"
:code-string "[mi: hle o64 81 /2 id,s]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf ADC-mem.imm8 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "mem,imm8"
:code-string "[mi: hle 80 /2 ib]"
:arch-flags (list "8086" "SM" "LOCK" "ND")))

(setf ADC-mem.sbyteword16 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "mem,sbyteword16"
:code-string "[mi: hle o16 83 /2 ib,s]"
:arch-flags (list "8086" "SM" "LOCK" "ND")))

(setf ADC-mem.imm16 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "mem,imm16"
:code-string "[mi: hle o16 81 /2 iw]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf ADC-mem.sbytedword32 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "mem,sbytedword32"
:code-string "[mi: hle o32 83 /2 ib,s]"
:arch-flags (list "386" "SM" "LOCK" "ND")))

(setf ADC-mem.imm32 (make-instance 'x86-asm-instruction
:name "ADC"
:operands "mem,imm32"
:code-string "[mi: hle o32 81 /2 id]"
:arch-flags (list "386" "SM" "LOCK")))

(setf ADD-mem.reg8 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "mem,reg8"
:code-string "[mr: hle 00 /r]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf ADD-reg8.reg8 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "reg8,reg8"
:code-string "[mr: 00 /r]"
:arch-flags (list "8086")))

(setf ADD-mem.reg16 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "mem,reg16"
:code-string "[mr: hle o16 01 /r]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf ADD-reg16.reg16 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "reg16,reg16"
:code-string "[mr: o16 01 /r]"
:arch-flags (list "8086")))

(setf ADD-mem.reg32 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "mem,reg32"
:code-string "[mr: hle o32 01 /r]"
:arch-flags (list "386" "SM" "LOCK")))

(setf ADD-reg32.reg32 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "reg32,reg32"
:code-string "[mr: o32 01 /r]"
:arch-flags (list "386")))

(setf ADD-mem.reg64 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "mem,reg64"
:code-string "[mr: hle o64 01 /r]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf ADD-reg64.reg64 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "reg64,reg64"
:code-string "[mr: o64 01 /r]"
:arch-flags (list "X64")))

(setf ADD-reg8.mem (make-instance 'x86-asm-instruction
:name "ADD"
:operands "reg8,mem"
:code-string "[rm: 02 /r]"
:arch-flags (list "8086" "SM")))

(setf ADD-reg8.reg8 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "reg8,reg8"
:code-string "[rm: 02 /r]"
:arch-flags (list "8086")))

(setf ADD-reg16.mem (make-instance 'x86-asm-instruction
:name "ADD"
:operands "reg16,mem"
:code-string "[rm: o16 03 /r]"
:arch-flags (list "8086" "SM")))

(setf ADD-reg16.reg16 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "reg16,reg16"
:code-string "[rm: o16 03 /r]"
:arch-flags (list "8086")))

(setf ADD-reg32.mem (make-instance 'x86-asm-instruction
:name "ADD"
:operands "reg32,mem"
:code-string "[rm: o32 03 /r]"
:arch-flags (list "386" "SM")))

(setf ADD-reg32.reg32 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "reg32,reg32"
:code-string "[rm: o32 03 /r]"
:arch-flags (list "386")))

(setf ADD-reg64.mem (make-instance 'x86-asm-instruction
:name "ADD"
:operands "reg64,mem"
:code-string "[rm: o64 03 /r]"
:arch-flags (list "X64" "SM")))

(setf ADD-reg64.reg64 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "reg64,reg64"
:code-string "[rm: o64 03 /r]"
:arch-flags (list "X64")))

(setf ADD-rm16.imm8 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "rm16,imm8"
:code-string "[mi: hle o16 83 /0 ib,s]"
:arch-flags (list "8086" "LOCK")))

(setf ADD-rm32.imm8 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "rm32,imm8"
:code-string "[mi: hle o32 83 /0 ib,s]"
:arch-flags (list "386" "LOCK")))

(setf ADD-rm64.imm8 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "rm64,imm8"
:code-string "[mi: hle o64 83 /0 ib,s]"
:arch-flags (list "X64" "LOCK")))

(setf ADD-reg_al.imm (make-instance 'x86-asm-instruction
:name "ADD"
:operands "reg_al,imm"
:code-string "[-i: 04 ib]"
:arch-flags (list "8086" "SM")))

(setf ADD-reg_ax.sbyteword (make-instance 'x86-asm-instruction
:name "ADD"
:operands "reg_ax,sbyteword"
:code-string "[mi: o16 83 /0 ib,s]"
:arch-flags (list "8086" "SM" "ND")))

(setf ADD-reg_ax.imm (make-instance 'x86-asm-instruction
:name "ADD"
:operands "reg_ax,imm"
:code-string "[-i: o16 05 iw]"
:arch-flags (list "8086" "SM")))

(setf ADD-reg_eax.sbytedword (make-instance 'x86-asm-instruction
:name "ADD"
:operands "reg_eax,sbytedword"
:code-string "[mi: o32 83 /0 ib,s]"
:arch-flags (list "386" "SM" "ND")))

(setf ADD-reg_eax.imm (make-instance 'x86-asm-instruction
:name "ADD"
:operands "reg_eax,imm"
:code-string "[-i: o32 05 id]"
:arch-flags (list "386" "SM")))

(setf ADD-reg_rax.sbytedword (make-instance 'x86-asm-instruction
:name "ADD"
:operands "reg_rax,sbytedword"
:code-string "[mi: o64 83 /0 ib,s]"
:arch-flags (list "X64" "SM" "ND")))

(setf ADD-reg_rax.imm (make-instance 'x86-asm-instruction
:name "ADD"
:operands "reg_rax,imm"
:code-string "[-i: o64 05 id,s]"
:arch-flags (list "X64" "SM")))

(setf ADD-rm8.imm (make-instance 'x86-asm-instruction
:name "ADD"
:operands "rm8,imm"
:code-string "[mi: hle 80 /0 ib]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf ADD-rm16.sbyteword (make-instance 'x86-asm-instruction
:name "ADD"
:operands "rm16,sbyteword"
:code-string "[mi: hle o16 83 /0 ib,s]"
:arch-flags (list "8086" "SM" "LOCK" "ND")))

(setf ADD-rm16.imm (make-instance 'x86-asm-instruction
:name "ADD"
:operands "rm16,imm"
:code-string "[mi: hle o16 81 /0 iw]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf ADD-rm32.sbytedword (make-instance 'x86-asm-instruction
:name "ADD"
:operands "rm32,sbytedword"
:code-string "[mi: hle o32 83 /0 ib,s]"
:arch-flags (list "386" "SM" "LOCK" "ND")))

(setf ADD-rm32.imm (make-instance 'x86-asm-instruction
:name "ADD"
:operands "rm32,imm"
:code-string "[mi: hle o32 81 /0 id]"
:arch-flags (list "386" "SM" "LOCK")))

(setf ADD-rm64.sbytedword (make-instance 'x86-asm-instruction
:name "ADD"
:operands "rm64,sbytedword"
:code-string "[mi: hle o64 83 /0 ib,s]"
:arch-flags (list "X64" "SM" "LOCK" "ND")))

(setf ADD-rm64.imm (make-instance 'x86-asm-instruction
:name "ADD"
:operands "rm64,imm"
:code-string "[mi: hle o64 81 /0 id,s]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf ADD-mem.imm8 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "mem,imm8"
:code-string "[mi: hle 80 /0 ib]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf ADD-mem.sbyteword16 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "mem,sbyteword16"
:code-string "[mi: hle o16 83 /0 ib,s]"
:arch-flags (list "8086" "SM" "LOCK" "ND")))

(setf ADD-mem.imm16 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "mem,imm16"
:code-string "[mi: hle o16 81 /0 iw]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf ADD-mem.sbytedword32 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "mem,sbytedword32"
:code-string "[mi: hle o32 83 /0 ib,s]"
:arch-flags (list "386" "SM" "LOCK" "ND")))

(setf ADD-mem.imm32 (make-instance 'x86-asm-instruction
:name "ADD"
:operands "mem,imm32"
:code-string "[mi: hle o32 81 /0 id]"
:arch-flags (list "386" "SM" "LOCK")))

(setf AND-mem.reg8 (make-instance 'x86-asm-instruction
:name "AND"
:operands "mem,reg8"
:code-string "[mr: hle 20 /r]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf AND-reg8.reg8 (make-instance 'x86-asm-instruction
:name "AND"
:operands "reg8,reg8"
:code-string "[mr: 20 /r]"
:arch-flags (list "8086")))

(setf AND-mem.reg16 (make-instance 'x86-asm-instruction
:name "AND"
:operands "mem,reg16"
:code-string "[mr: hle o16 21 /r]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf AND-reg16.reg16 (make-instance 'x86-asm-instruction
:name "AND"
:operands "reg16,reg16"
:code-string "[mr: o16 21 /r]"
:arch-flags (list "8086")))

(setf AND-mem.reg32 (make-instance 'x86-asm-instruction
:name "AND"
:operands "mem,reg32"
:code-string "[mr: hle o32 21 /r]"
:arch-flags (list "386" "SM" "LOCK")))

(setf AND-reg32.reg32 (make-instance 'x86-asm-instruction
:name "AND"
:operands "reg32,reg32"
:code-string "[mr: o32 21 /r]"
:arch-flags (list "386")))

(setf AND-mem.reg64 (make-instance 'x86-asm-instruction
:name "AND"
:operands "mem,reg64"
:code-string "[mr: hle o64 21 /r]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf AND-reg64.reg64 (make-instance 'x86-asm-instruction
:name "AND"
:operands "reg64,reg64"
:code-string "[mr: o64 21 /r]"
:arch-flags (list "X64")))

(setf AND-reg8.mem (make-instance 'x86-asm-instruction
:name "AND"
:operands "reg8,mem"
:code-string "[rm: 22 /r]"
:arch-flags (list "8086" "SM")))

(setf AND-reg8.reg8 (make-instance 'x86-asm-instruction
:name "AND"
:operands "reg8,reg8"
:code-string "[rm: 22 /r]"
:arch-flags (list "8086")))

(setf AND-reg16.mem (make-instance 'x86-asm-instruction
:name "AND"
:operands "reg16,mem"
:code-string "[rm: o16 23 /r]"
:arch-flags (list "8086" "SM")))

(setf AND-reg16.reg16 (make-instance 'x86-asm-instruction
:name "AND"
:operands "reg16,reg16"
:code-string "[rm: o16 23 /r]"
:arch-flags (list "8086")))

(setf AND-reg32.mem (make-instance 'x86-asm-instruction
:name "AND"
:operands "reg32,mem"
:code-string "[rm: o32 23 /r]"
:arch-flags (list "386" "SM")))

(setf AND-reg32.reg32 (make-instance 'x86-asm-instruction
:name "AND"
:operands "reg32,reg32"
:code-string "[rm: o32 23 /r]"
:arch-flags (list "386")))

(setf AND-reg64.mem (make-instance 'x86-asm-instruction
:name "AND"
:operands "reg64,mem"
:code-string "[rm: o64 23 /r]"
:arch-flags (list "X64" "SM")))

(setf AND-reg64.reg64 (make-instance 'x86-asm-instruction
:name "AND"
:operands "reg64,reg64"
:code-string "[rm: o64 23 /r]"
:arch-flags (list "X64")))

(setf AND-rm16.imm8 (make-instance 'x86-asm-instruction
:name "AND"
:operands "rm16,imm8"
:code-string "[mi: hle o16 83 /4 ib,s]"
:arch-flags (list "8086" "LOCK")))

(setf AND-rm32.imm8 (make-instance 'x86-asm-instruction
:name "AND"
:operands "rm32,imm8"
:code-string "[mi: hle o32 83 /4 ib,s]"
:arch-flags (list "386" "LOCK")))

(setf AND-rm64.imm8 (make-instance 'x86-asm-instruction
:name "AND"
:operands "rm64,imm8"
:code-string "[mi: hle o64 83 /4 ib,s]"
:arch-flags (list "X64" "LOCK")))

(setf AND-reg_al.imm (make-instance 'x86-asm-instruction
:name "AND"
:operands "reg_al,imm"
:code-string "[-i: 24 ib]"
:arch-flags (list "8086" "SM")))

(setf AND-reg_ax.sbyteword (make-instance 'x86-asm-instruction
:name "AND"
:operands "reg_ax,sbyteword"
:code-string "[mi: o16 83 /4 ib,s]"
:arch-flags (list "8086" "SM" "ND")))

(setf AND-reg_ax.imm (make-instance 'x86-asm-instruction
:name "AND"
:operands "reg_ax,imm"
:code-string "[-i: o16 25 iw]"
:arch-flags (list "8086" "SM")))

(setf AND-reg_eax.sbytedword (make-instance 'x86-asm-instruction
:name "AND"
:operands "reg_eax,sbytedword"
:code-string "[mi: o32 83 /4 ib,s]"
:arch-flags (list "386" "SM" "ND")))

(setf AND-reg_eax.imm (make-instance 'x86-asm-instruction
:name "AND"
:operands "reg_eax,imm"
:code-string "[-i: o32 25 id]"
:arch-flags (list "386" "SM")))

(setf AND-reg_rax.sbytedword (make-instance 'x86-asm-instruction
:name "AND"
:operands "reg_rax,sbytedword"
:code-string "[mi: o64 83 /4 ib,s]"
:arch-flags (list "X64" "SM" "ND")))

(setf AND-reg_rax.imm (make-instance 'x86-asm-instruction
:name "AND"
:operands "reg_rax,imm"
:code-string "[-i: o64 25 id,s]"
:arch-flags (list "X64" "SM")))

(setf AND-rm8.imm (make-instance 'x86-asm-instruction
:name "AND"
:operands "rm8,imm"
:code-string "[mi: hle 80 /4 ib]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf AND-rm16.sbyteword (make-instance 'x86-asm-instruction
:name "AND"
:operands "rm16,sbyteword"
:code-string "[mi: hle o16 83 /4 ib,s]"
:arch-flags (list "8086" "SM" "LOCK" "ND")))

(setf AND-rm16.imm (make-instance 'x86-asm-instruction
:name "AND"
:operands "rm16,imm"
:code-string "[mi: hle o16 81 /4 iw]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf AND-rm32.sbytedword (make-instance 'x86-asm-instruction
:name "AND"
:operands "rm32,sbytedword"
:code-string "[mi: hle o32 83 /4 ib,s]"
:arch-flags (list "386" "SM" "LOCK" "ND")))

(setf AND-rm32.imm (make-instance 'x86-asm-instruction
:name "AND"
:operands "rm32,imm"
:code-string "[mi: hle o32 81 /4 id]"
:arch-flags (list "386" "SM" "LOCK")))

(setf AND-rm64.sbytedword (make-instance 'x86-asm-instruction
:name "AND"
:operands "rm64,sbytedword"
:code-string "[mi: hle o64 83 /4 ib,s]"
:arch-flags (list "X64" "SM" "LOCK" "ND")))

(setf AND-rm64.imm (make-instance 'x86-asm-instruction
:name "AND"
:operands "rm64,imm"
:code-string "[mi: hle o64 81 /4 id,s]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf AND-mem.imm8 (make-instance 'x86-asm-instruction
:name "AND"
:operands "mem,imm8"
:code-string "[mi: hle 80 /4 ib]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf AND-mem.sbyteword16 (make-instance 'x86-asm-instruction
:name "AND"
:operands "mem,sbyteword16"
:code-string "[mi: hle o16 83 /4 ib,s]"
:arch-flags (list "8086" "SM" "LOCK" "ND")))

(setf AND-mem.imm16 (make-instance 'x86-asm-instruction
:name "AND"
:operands "mem,imm16"
:code-string "[mi: hle o16 81 /4 iw]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf AND-mem.sbytedword32 (make-instance 'x86-asm-instruction
:name "AND"
:operands "mem,sbytedword32"
:code-string "[mi: hle o32 83 /4 ib,s]"
:arch-flags (list "386" "SM" "LOCK" "ND")))

(setf AND-mem.imm32 (make-instance 'x86-asm-instruction
:name "AND"
:operands "mem,imm32"
:code-string "[mi: hle o32 81 /4 id]"
:arch-flags (list "386" "SM" "LOCK")))

(setf BB0_RESET-void (make-instance 'x86-asm-instruction
:name "BB0_RESET"
:operands "void"
:code-string "[ 0f 3a]"
:arch-flags (list "PENT" "CYRIX" "ND")))

(setf BB1_RESET-void (make-instance 'x86-asm-instruction
:name "BB1_RESET"
:operands "void"
:code-string "[ 0f 3b]"
:arch-flags (list "PENT" "CYRIX" "ND")))

(setf BSF-reg16.mem (make-instance 'x86-asm-instruction
:name "BSF"
:operands "reg16,mem"
:code-string "[rm: o16 nof3 0f bc /r]"
:arch-flags (list "386" "SM")))

(setf BSF-reg16.reg16 (make-instance 'x86-asm-instruction
:name "BSF"
:operands "reg16,reg16"
:code-string "[rm: o16 nof3 0f bc /r]"
:arch-flags (list "386")))

(setf BSF-reg32.mem (make-instance 'x86-asm-instruction
:name "BSF"
:operands "reg32,mem"
:code-string "[rm: o32 nof3 0f bc /r]"
:arch-flags (list "386" "SM")))

(setf BSF-reg32.reg32 (make-instance 'x86-asm-instruction
:name "BSF"
:operands "reg32,reg32"
:code-string "[rm: o32 nof3 0f bc /r]"
:arch-flags (list "386")))

(setf BSF-reg64.mem (make-instance 'x86-asm-instruction
:name "BSF"
:operands "reg64,mem"
:code-string "[rm: o64 nof3 0f bc /r]"
:arch-flags (list "X64" "SM")))

(setf BSF-reg64.reg64 (make-instance 'x86-asm-instruction
:name "BSF"
:operands "reg64,reg64"
:code-string "[rm: o64 nof3 0f bc /r]"
:arch-flags (list "X64")))

(setf BSR-reg16.mem (make-instance 'x86-asm-instruction
:name "BSR"
:operands "reg16,mem"
:code-string "[rm: o16 nof3 0f bd /r]"
:arch-flags (list "386" "SM")))

(setf BSR-reg16.reg16 (make-instance 'x86-asm-instruction
:name "BSR"
:operands "reg16,reg16"
:code-string "[rm: o16 nof3 0f bd /r]"
:arch-flags (list "386")))

(setf BSR-reg32.mem (make-instance 'x86-asm-instruction
:name "BSR"
:operands "reg32,mem"
:code-string "[rm: o32 nof3 0f bd /r]"
:arch-flags (list "386" "SM")))

(setf BSR-reg32.reg32 (make-instance 'x86-asm-instruction
:name "BSR"
:operands "reg32,reg32"
:code-string "[rm: o32 nof3 0f bd /r]"
:arch-flags (list "386")))

(setf BSR-reg64.mem (make-instance 'x86-asm-instruction
:name "BSR"
:operands "reg64,mem"
:code-string "[rm: o64 nof3 0f bd /r]"
:arch-flags (list "X64" "SM")))

(setf BSR-reg64.reg64 (make-instance 'x86-asm-instruction
:name "BSR"
:operands "reg64,reg64"
:code-string "[rm: o64 nof3 0f bd /r]"
:arch-flags (list "X64")))

(setf BSWAP-reg32 (make-instance 'x86-asm-instruction
:name "BSWAP"
:operands "reg32"
:code-string "[r: o32 0f c8+r]"
:arch-flags (list "486")))

(setf BSWAP-reg64 (make-instance 'x86-asm-instruction
:name "BSWAP"
:operands "reg64"
:code-string "[r: o64 0f c8+r]"
:arch-flags (list "X64")))

(setf BT-mem.reg16 (make-instance 'x86-asm-instruction
:name "BT"
:operands "mem,reg16"
:code-string "[mr: o16 0f a3 /r]"
:arch-flags (list "386" "SM")))

(setf BT-reg16.reg16 (make-instance 'x86-asm-instruction
:name "BT"
:operands "reg16,reg16"
:code-string "[mr: o16 0f a3 /r]"
:arch-flags (list "386")))

(setf BT-mem.reg32 (make-instance 'x86-asm-instruction
:name "BT"
:operands "mem,reg32"
:code-string "[mr: o32 0f a3 /r]"
:arch-flags (list "386" "SM")))

(setf BT-reg32.reg32 (make-instance 'x86-asm-instruction
:name "BT"
:operands "reg32,reg32"
:code-string "[mr: o32 0f a3 /r]"
:arch-flags (list "386")))

(setf BT-mem.reg64 (make-instance 'x86-asm-instruction
:name "BT"
:operands "mem,reg64"
:code-string "[mr: o64 0f a3 /r]"
:arch-flags (list "X64" "SM")))

(setf BT-reg64.reg64 (make-instance 'x86-asm-instruction
:name "BT"
:operands "reg64,reg64"
:code-string "[mr: o64 0f a3 /r]"
:arch-flags (list "X64")))

(setf BT-rm16.imm (make-instance 'x86-asm-instruction
:name "BT"
:operands "rm16,imm"
:code-string "[mi: o16 0f ba /4 ib,u]"
:arch-flags (list "386" "SB")))

(setf BT-rm32.imm (make-instance 'x86-asm-instruction
:name "BT"
:operands "rm32,imm"
:code-string "[mi: o32 0f ba /4 ib,u]"
:arch-flags (list "386" "SB")))

(setf BT-rm64.imm (make-instance 'x86-asm-instruction
:name "BT"
:operands "rm64,imm"
:code-string "[mi: o64 0f ba /4 ib,u]"
:arch-flags (list "X64" "SB")))

(setf BTC-mem.reg16 (make-instance 'x86-asm-instruction
:name "BTC"
:operands "mem,reg16"
:code-string "[mr: hle o16 0f bb /r]"
:arch-flags (list "386" "SM" "LOCK")))

(setf BTC-reg16.reg16 (make-instance 'x86-asm-instruction
:name "BTC"
:operands "reg16,reg16"
:code-string "[mr: o16 0f bb /r]"
:arch-flags (list "386")))

(setf BTC-mem.reg32 (make-instance 'x86-asm-instruction
:name "BTC"
:operands "mem,reg32"
:code-string "[mr: hle o32 0f bb /r]"
:arch-flags (list "386" "SM" "LOCK")))

(setf BTC-reg32.reg32 (make-instance 'x86-asm-instruction
:name "BTC"
:operands "reg32,reg32"
:code-string "[mr: o32 0f bb /r]"
:arch-flags (list "386")))

(setf BTC-mem.reg64 (make-instance 'x86-asm-instruction
:name "BTC"
:operands "mem,reg64"
:code-string "[mr: hle o64 0f bb /r]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf BTC-reg64.reg64 (make-instance 'x86-asm-instruction
:name "BTC"
:operands "reg64,reg64"
:code-string "[mr: o64 0f bb /r]"
:arch-flags (list "X64")))

(setf BTC-rm16.imm (make-instance 'x86-asm-instruction
:name "BTC"
:operands "rm16,imm"
:code-string "[mi: hle o16 0f ba /7 ib,u]"
:arch-flags (list "386" "SB" "LOCK")))

(setf BTC-rm32.imm (make-instance 'x86-asm-instruction
:name "BTC"
:operands "rm32,imm"
:code-string "[mi: hle o32 0f ba /7 ib,u]"
:arch-flags (list "386" "SB" "LOCK")))

(setf BTC-rm64.imm (make-instance 'x86-asm-instruction
:name "BTC"
:operands "rm64,imm"
:code-string "[mi: hle o64 0f ba /7 ib,u]"
:arch-flags (list "X64" "SB" "LOCK")))

(setf BTR-mem.reg16 (make-instance 'x86-asm-instruction
:name "BTR"
:operands "mem,reg16"
:code-string "[mr: hle o16 0f b3 /r]"
:arch-flags (list "386" "SM" "LOCK")))

(setf BTR-reg16.reg16 (make-instance 'x86-asm-instruction
:name "BTR"
:operands "reg16,reg16"
:code-string "[mr: o16 0f b3 /r]"
:arch-flags (list "386")))

(setf BTR-mem.reg32 (make-instance 'x86-asm-instruction
:name "BTR"
:operands "mem,reg32"
:code-string "[mr: hle o32 0f b3 /r]"
:arch-flags (list "386" "SM" "LOCK")))

(setf BTR-reg32.reg32 (make-instance 'x86-asm-instruction
:name "BTR"
:operands "reg32,reg32"
:code-string "[mr: o32 0f b3 /r]"
:arch-flags (list "386")))

(setf BTR-mem.reg64 (make-instance 'x86-asm-instruction
:name "BTR"
:operands "mem,reg64"
:code-string "[mr: hle o64 0f b3 /r]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf BTR-reg64.reg64 (make-instance 'x86-asm-instruction
:name "BTR"
:operands "reg64,reg64"
:code-string "[mr: o64 0f b3 /r]"
:arch-flags (list "X64")))

(setf BTR-rm16.imm (make-instance 'x86-asm-instruction
:name "BTR"
:operands "rm16,imm"
:code-string "[mi: hle o16 0f ba /6 ib,u]"
:arch-flags (list "386" "SB" "LOCK")))

(setf BTR-rm32.imm (make-instance 'x86-asm-instruction
:name "BTR"
:operands "rm32,imm"
:code-string "[mi: hle o32 0f ba /6 ib,u]"
:arch-flags (list "386" "SB" "LOCK")))

(setf BTR-rm64.imm (make-instance 'x86-asm-instruction
:name "BTR"
:operands "rm64,imm"
:code-string "[mi: hle o64 0f ba /6 ib,u]"
:arch-flags (list "X64" "SB" "LOCK")))

(setf BTS-mem.reg16 (make-instance 'x86-asm-instruction
:name "BTS"
:operands "mem,reg16"
:code-string "[mr: hle o16 0f ab /r]"
:arch-flags (list "386" "SM" "LOCK")))

(setf BTS-reg16.reg16 (make-instance 'x86-asm-instruction
:name "BTS"
:operands "reg16,reg16"
:code-string "[mr: o16 0f ab /r]"
:arch-flags (list "386")))

(setf BTS-mem.reg32 (make-instance 'x86-asm-instruction
:name "BTS"
:operands "mem,reg32"
:code-string "[mr: hle o32 0f ab /r]"
:arch-flags (list "386" "SM" "LOCK")))

(setf BTS-reg32.reg32 (make-instance 'x86-asm-instruction
:name "BTS"
:operands "reg32,reg32"
:code-string "[mr: o32 0f ab /r]"
:arch-flags (list "386")))

(setf BTS-mem.reg64 (make-instance 'x86-asm-instruction
:name "BTS"
:operands "mem,reg64"
:code-string "[mr: hle o64 0f ab /r]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf BTS-reg64.reg64 (make-instance 'x86-asm-instruction
:name "BTS"
:operands "reg64,reg64"
:code-string "[mr: o64 0f ab /r]"
:arch-flags (list "X64")))

(setf BTS-rm16.imm (make-instance 'x86-asm-instruction
:name "BTS"
:operands "rm16,imm"
:code-string "[mi: hle o16 0f ba /5 ib,u]"
:arch-flags (list "386" "SB" "LOCK")))

(setf BTS-rm32.imm (make-instance 'x86-asm-instruction
:name "BTS"
:operands "rm32,imm"
:code-string "[mi: hle o32 0f ba /5 ib,u]"
:arch-flags (list "386" "SB" "LOCK")))

(setf BTS-rm64.imm (make-instance 'x86-asm-instruction
:name "BTS"
:operands "rm64,imm"
:code-string "[mi: hle o64 0f ba /5 ib,u]"
:arch-flags (list "X64" "SB" "LOCK")))

(setf CALL-imm (make-instance 'x86-asm-instruction
:name "CALL"
:operands "imm"
:code-string "[i: odf e8 rel]"
:arch-flags (list "8086" "BND")))

(setf CALL-imm-near (make-instance 'x86-asm-instruction
:name "CALL"
:operands "imm|near"
:code-string "[i: odf e8 rel]"
:arch-flags (list "8086" "ND" "BND")))

(setf CALL-imm64 (make-instance 'x86-asm-instruction
:name "CALL"
:operands "imm64"
:code-string "[i: o64nw e8 rel]"
:arch-flags (list "X64" "BND")))

(setf CALL-imm64-near (make-instance 'x86-asm-instruction
:name "CALL"
:operands "imm64|near"
:code-string "[i: o64nw e8 rel]"
:arch-flags (list "X64" "ND" "BND")))

(setf CALL-mem-far (make-instance 'x86-asm-instruction
:name "CALL"
:operands "mem|far"
:code-string "[m: o64 ff /3]"
:arch-flags (list "X64")))

(setf CALL-mem16-far (make-instance 'x86-asm-instruction
:name "CALL"
:operands "mem16|far"
:code-string "[m: o16 ff /3]"
:arch-flags (list "8086")))

(setf CALL-mem32-far (make-instance 'x86-asm-instruction
:name "CALL"
:operands "mem32|far"
:code-string "[m: o32 ff /3]"
:arch-flags (list "386")))

(setf CALL-mem64-far (make-instance 'x86-asm-instruction
:name "CALL"
:operands "mem64|far"
:code-string "[m: o64 ff /3]"
:arch-flags (list "X64")))

(setf CALL-mem-near (make-instance 'x86-asm-instruction
:name "CALL"
:operands "mem|near"
:code-string "[m: odf ff /2]"
:arch-flags (list "8086" "ND" "BND")))

(setf CALL-rm64-near (make-instance 'x86-asm-instruction
:name "CALL"
:operands "rm64|near"
:code-string "[m: o64nw ff /2]"
:arch-flags (list "X64" "ND" "BND")))

(setf CALL-mem (make-instance 'x86-asm-instruction
:name "CALL"
:operands "mem"
:code-string "[m: odf ff /2]"
:arch-flags (list "8086" "BND")))

(setf CALL-rm64 (make-instance 'x86-asm-instruction
:name "CALL"
:operands "rm64"
:code-string "[m: o64nw ff /2]"
:arch-flags (list "X64" "BND")))

(setf CBW-void (make-instance 'x86-asm-instruction
:name "CBW"
:operands "void"
:code-string "[ o16 98]"
:arch-flags (list "8086")))

(setf CDQ-void (make-instance 'x86-asm-instruction
:name "CDQ"
:operands "void"
:code-string "[ o32 99]"
:arch-flags (list "386")))

(setf CDQE-void (make-instance 'x86-asm-instruction
:name "CDQE"
:operands "void"
:code-string "[ o64 98]"
:arch-flags (list "X64")))

(setf CLC-void (make-instance 'x86-asm-instruction
:name "CLC"
:operands "void"
:code-string "[ f8]"
:arch-flags (list "8086")))

(setf CLD-void (make-instance 'x86-asm-instruction
:name "CLD"
:operands "void"
:code-string "[ fc]"
:arch-flags (list "8086")))

(setf CLI-void (make-instance 'x86-asm-instruction
:name "CLI"
:operands "void"
:code-string "[ fa]"
:arch-flags (list "8086")))

(setf CLTS-void (make-instance 'x86-asm-instruction
:name "CLTS"
:operands "void"
:code-string "[ 0f 06]"
:arch-flags (list "286" "PRIV")))

(setf CMC-void (make-instance 'x86-asm-instruction
:name "CMC"
:operands "void"
:code-string "[ f5]"
:arch-flags (list "8086")))

(setf CMP-mem.reg8 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "mem,reg8"
:code-string "[mr: 38 /r]"
:arch-flags (list "8086" "SM")))

(setf CMP-reg8.reg8 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "reg8,reg8"
:code-string "[mr: 38 /r]"
:arch-flags (list "8086")))

(setf CMP-mem.reg16 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "mem,reg16"
:code-string "[mr: o16 39 /r]"
:arch-flags (list "8086" "SM")))

(setf CMP-reg16.reg16 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "reg16,reg16"
:code-string "[mr: o16 39 /r]"
:arch-flags (list "8086")))

(setf CMP-mem.reg32 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "mem,reg32"
:code-string "[mr: o32 39 /r]"
:arch-flags (list "386" "SM")))

(setf CMP-reg32.reg32 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "reg32,reg32"
:code-string "[mr: o32 39 /r]"
:arch-flags (list "386")))

(setf CMP-mem.reg64 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "mem,reg64"
:code-string "[mr: o64 39 /r]"
:arch-flags (list "X64" "SM")))

(setf CMP-reg64.reg64 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "reg64,reg64"
:code-string "[mr: o64 39 /r]"
:arch-flags (list "X64")))

(setf CMP-reg8.mem (make-instance 'x86-asm-instruction
:name "CMP"
:operands "reg8,mem"
:code-string "[rm: 3a /r]"
:arch-flags (list "8086" "SM")))

(setf CMP-reg8.reg8 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "reg8,reg8"
:code-string "[rm: 3a /r]"
:arch-flags (list "8086")))

(setf CMP-reg16.mem (make-instance 'x86-asm-instruction
:name "CMP"
:operands "reg16,mem"
:code-string "[rm: o16 3b /r]"
:arch-flags (list "8086" "SM")))

(setf CMP-reg16.reg16 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "reg16,reg16"
:code-string "[rm: o16 3b /r]"
:arch-flags (list "8086")))

(setf CMP-reg32.mem (make-instance 'x86-asm-instruction
:name "CMP"
:operands "reg32,mem"
:code-string "[rm: o32 3b /r]"
:arch-flags (list "386" "SM")))

(setf CMP-reg32.reg32 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "reg32,reg32"
:code-string "[rm: o32 3b /r]"
:arch-flags (list "386")))

(setf CMP-reg64.mem (make-instance 'x86-asm-instruction
:name "CMP"
:operands "reg64,mem"
:code-string "[rm: o64 3b /r]"
:arch-flags (list "X64" "SM")))

(setf CMP-reg64.reg64 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "reg64,reg64"
:code-string "[rm: o64 3b /r]"
:arch-flags (list "X64")))

(setf CMP-rm16.imm8 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "rm16,imm8"
:code-string "[mi: o16 83 /7 ib,s]"
:arch-flags (list "8086")))

(setf CMP-rm32.imm8 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "rm32,imm8"
:code-string "[mi: o32 83 /7 ib,s]"
:arch-flags (list "386")))

(setf CMP-rm64.imm8 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "rm64,imm8"
:code-string "[mi: o64 83 /7 ib,s]"
:arch-flags (list "X64")))

(setf CMP-reg_al.imm (make-instance 'x86-asm-instruction
:name "CMP"
:operands "reg_al,imm"
:code-string "[-i: 3c ib]"
:arch-flags (list "8086" "SM")))

(setf CMP-reg_ax.sbyteword (make-instance 'x86-asm-instruction
:name "CMP"
:operands "reg_ax,sbyteword"
:code-string "[mi: o16 83 /7 ib,s]"
:arch-flags (list "8086" "SM" "ND")))

(setf CMP-reg_ax.imm (make-instance 'x86-asm-instruction
:name "CMP"
:operands "reg_ax,imm"
:code-string "[-i: o16 3d iw]"
:arch-flags (list "8086" "SM")))

(setf CMP-reg_eax.sbytedword (make-instance 'x86-asm-instruction
:name "CMP"
:operands "reg_eax,sbytedword"
:code-string "[mi: o32 83 /7 ib,s]"
:arch-flags (list "386" "SM" "ND")))

(setf CMP-reg_eax.imm (make-instance 'x86-asm-instruction
:name "CMP"
:operands "reg_eax,imm"
:code-string "[-i: o32 3d id]"
:arch-flags (list "386" "SM")))

(setf CMP-reg_rax.sbytedword (make-instance 'x86-asm-instruction
:name "CMP"
:operands "reg_rax,sbytedword"
:code-string "[mi: o64 83 /7 ib,s]"
:arch-flags (list "X64" "SM" "ND")))

(setf CMP-reg_rax.imm (make-instance 'x86-asm-instruction
:name "CMP"
:operands "reg_rax,imm"
:code-string "[-i: o64 3d id,s]"
:arch-flags (list "X64" "SM")))

(setf CMP-rm8.imm (make-instance 'x86-asm-instruction
:name "CMP"
:operands "rm8,imm"
:code-string "[mi: 80 /7 ib]"
:arch-flags (list "8086" "SM")))

(setf CMP-rm16.sbyteword (make-instance 'x86-asm-instruction
:name "CMP"
:operands "rm16,sbyteword"
:code-string "[mi: o16 83 /7 ib,s]"
:arch-flags (list "8086" "SM" "ND")))

(setf CMP-rm16.imm (make-instance 'x86-asm-instruction
:name "CMP"
:operands "rm16,imm"
:code-string "[mi: o16 81 /7 iw]"
:arch-flags (list "8086" "SM")))

(setf CMP-rm32.sbytedword (make-instance 'x86-asm-instruction
:name "CMP"
:operands "rm32,sbytedword"
:code-string "[mi: o32 83 /7 ib,s]"
:arch-flags (list "386" "SM" "ND")))

(setf CMP-rm32.imm (make-instance 'x86-asm-instruction
:name "CMP"
:operands "rm32,imm"
:code-string "[mi: o32 81 /7 id]"
:arch-flags (list "386" "SM")))

(setf CMP-rm64.sbytedword (make-instance 'x86-asm-instruction
:name "CMP"
:operands "rm64,sbytedword"
:code-string "[mi: o64 83 /7 ib,s]"
:arch-flags (list "X64" "SM" "ND")))

(setf CMP-rm64.imm (make-instance 'x86-asm-instruction
:name "CMP"
:operands "rm64,imm"
:code-string "[mi: o64 81 /7 id,s]"
:arch-flags (list "X64" "SM")))

(setf CMP-mem.imm8 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "mem,imm8"
:code-string "[mi: 80 /7 ib]"
:arch-flags (list "8086" "SM")))

(setf CMP-mem.sbyteword16 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "mem,sbyteword16"
:code-string "[mi: o16 83 /7 ib,s]"
:arch-flags (list "8086" "SM" "ND")))

(setf CMP-mem.imm16 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "mem,imm16"
:code-string "[mi: o16 81 /7 iw]"
:arch-flags (list "8086" "SM")))

(setf CMP-mem.sbytedword32 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "mem,sbytedword32"
:code-string "[mi: o32 83 /7 ib,s]"
:arch-flags (list "386" "SM" "ND")))

(setf CMP-mem.imm32 (make-instance 'x86-asm-instruction
:name "CMP"
:operands "mem,imm32"
:code-string "[mi: o32 81 /7 id]"
:arch-flags (list "386" "SM")))

(setf CMPSB-void (make-instance 'x86-asm-instruction
:name "CMPSB"
:operands "void"
:code-string "[ repe a6]"
:arch-flags (list "8086")))

(setf CMPSD-void (make-instance 'x86-asm-instruction
:name "CMPSD"
:operands "void"
:code-string "[ repe o32 a7]"
:arch-flags (list "386")))

(setf CMPSQ-void (make-instance 'x86-asm-instruction
:name "CMPSQ"
:operands "void"
:code-string "[ repe o64 a7]"
:arch-flags (list "X64")))

(setf CMPSW-void (make-instance 'x86-asm-instruction
:name "CMPSW"
:operands "void"
:code-string "[ repe o16 a7]"
:arch-flags (list "8086")))

(setf CMPXCHG-mem.reg8 (make-instance 'x86-asm-instruction
:name "CMPXCHG"
:operands "mem,reg8"
:code-string "[mr: hle 0f b0 /r]"
:arch-flags (list "PENT" "SM" "LOCK")))

(setf CMPXCHG-reg8.reg8 (make-instance 'x86-asm-instruction
:name "CMPXCHG"
:operands "reg8,reg8"
:code-string "[mr: 0f b0 /r]"
:arch-flags (list "PENT")))

(setf CMPXCHG-mem.reg16 (make-instance 'x86-asm-instruction
:name "CMPXCHG"
:operands "mem,reg16"
:code-string "[mr: hle o16 0f b1 /r]"
:arch-flags (list "PENT" "SM" "LOCK")))

(setf CMPXCHG-reg16.reg16 (make-instance 'x86-asm-instruction
:name "CMPXCHG"
:operands "reg16,reg16"
:code-string "[mr: o16 0f b1 /r]"
:arch-flags (list "PENT")))

(setf CMPXCHG-mem.reg32 (make-instance 'x86-asm-instruction
:name "CMPXCHG"
:operands "mem,reg32"
:code-string "[mr: hle o32 0f b1 /r]"
:arch-flags (list "PENT" "SM" "LOCK")))

(setf CMPXCHG-reg32.reg32 (make-instance 'x86-asm-instruction
:name "CMPXCHG"
:operands "reg32,reg32"
:code-string "[mr: o32 0f b1 /r]"
:arch-flags (list "PENT")))

(setf CMPXCHG-mem.reg64 (make-instance 'x86-asm-instruction
:name "CMPXCHG"
:operands "mem,reg64"
:code-string "[mr: hle o64 0f b1 /r]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf CMPXCHG-reg64.reg64 (make-instance 'x86-asm-instruction
:name "CMPXCHG"
:operands "reg64,reg64"
:code-string "[mr: o64 0f b1 /r]"
:arch-flags (list "X64")))

(setf CMPXCHG486-mem.reg8 (make-instance 'x86-asm-instruction
:name "CMPXCHG486"
:operands "mem,reg8"
:code-string "[mr: 0f a6 /r]"
:arch-flags (list "486" "SM" "UNDOC" "ND" "LOCK")))

(setf CMPXCHG486-reg8.reg8 (make-instance 'x86-asm-instruction
:name "CMPXCHG486"
:operands "reg8,reg8"
:code-string "[mr: 0f a6 /r]"
:arch-flags (list "486" "UNDOC" "ND")))

(setf CMPXCHG486-mem.reg16 (make-instance 'x86-asm-instruction
:name "CMPXCHG486"
:operands "mem,reg16"
:code-string "[mr: o16 0f a7 /r]"
:arch-flags (list "486" "SM" "UNDOC" "ND" "LOCK")))

(setf CMPXCHG486-reg16.reg16 (make-instance 'x86-asm-instruction
:name "CMPXCHG486"
:operands "reg16,reg16"
:code-string "[mr: o16 0f a7 /r]"
:arch-flags (list "486" "UNDOC" "ND")))

(setf CMPXCHG486-mem.reg32 (make-instance 'x86-asm-instruction
:name "CMPXCHG486"
:operands "mem,reg32"
:code-string "[mr: o32 0f a7 /r]"
:arch-flags (list "486" "SM" "UNDOC" "ND" "LOCK")))

(setf CMPXCHG486-reg32.reg32 (make-instance 'x86-asm-instruction
:name "CMPXCHG486"
:operands "reg32,reg32"
:code-string "[mr: o32 0f a7 /r]"
:arch-flags (list "486" "UNDOC" "ND")))

(setf CMPXCHG8B-mem (make-instance 'x86-asm-instruction
:name "CMPXCHG8B"
:operands "mem"
:code-string "[m: hle norexw 0f c7 /1]"
:arch-flags (list "PENT" "LOCK")))

(setf CMPXCHG16B-mem (make-instance 'x86-asm-instruction
:name "CMPXCHG16B"
:operands "mem"
:code-string "[m: o64 0f c7 /1]"
:arch-flags (list "X64" "LOCK")))

(setf CPUID-void (make-instance 'x86-asm-instruction
:name "CPUID"
:operands "void"
:code-string "[ 0f a2]"
:arch-flags (list "PENT")))

(setf CPU_READ-void (make-instance 'x86-asm-instruction
:name "CPU_READ"
:operands "void"
:code-string "[ 0f 3d]"
:arch-flags (list "PENT" "CYRIX")))

(setf CPU_WRITE-void (make-instance 'x86-asm-instruction
:name "CPU_WRITE"
:operands "void"
:code-string "[ 0f 3c]"
:arch-flags (list "PENT" "CYRIX")))

(setf CQO-void (make-instance 'x86-asm-instruction
:name "CQO"
:operands "void"
:code-string "[ o64 99]"
:arch-flags (list "X64")))

(setf CWD-void (make-instance 'x86-asm-instruction
:name "CWD"
:operands "void"
:code-string "[ o16 99]"
:arch-flags (list "8086")))

(setf CWDE-void (make-instance 'x86-asm-instruction
:name "CWDE"
:operands "void"
:code-string "[ o32 98]"
:arch-flags (list "386")))

(setf DEC-rm8 (make-instance 'x86-asm-instruction
:name "DEC"
:operands "rm8"
:code-string "[m: hle fe /1]"
:arch-flags (list "8086" "LOCK")))

(setf DEC-rm16 (make-instance 'x86-asm-instruction
:name "DEC"
:operands "rm16"
:code-string "[m: hle o16 ff /1]"
:arch-flags (list "8086" "LOCK")))

(setf DEC-rm32 (make-instance 'x86-asm-instruction
:name "DEC"
:operands "rm32"
:code-string "[m: hle o32 ff /1]"
:arch-flags (list "386" "LOCK")))

(setf DEC-rm64 (make-instance 'x86-asm-instruction
:name "DEC"
:operands "rm64"
:code-string "[m: hle o64 ff /1]"
:arch-flags (list "X64" "LOCK")))

(setf DIV-rm8 (make-instance 'x86-asm-instruction
:name "DIV"
:operands "rm8"
:code-string "[m: f6 /6]"
:arch-flags (list "8086")))

(setf DIV-rm16 (make-instance 'x86-asm-instruction
:name "DIV"
:operands "rm16"
:code-string "[m: o16 f7 /6]"
:arch-flags (list "8086")))

(setf DIV-rm32 (make-instance 'x86-asm-instruction
:name "DIV"
:operands "rm32"
:code-string "[m: o32 f7 /6]"
:arch-flags (list "386")))

(setf DIV-rm64 (make-instance 'x86-asm-instruction
:name "DIV"
:operands "rm64"
:code-string "[m: o64 f7 /6]"
:arch-flags (list "X64")))

(setf DMINT-void (make-instance 'x86-asm-instruction
:name "DMINT"
:operands "void"
:code-string "[ 0f 39]"
:arch-flags (list "P6" "CYRIX")))

(setf EMMS-void (make-instance 'x86-asm-instruction
:name "EMMS"
:operands "void"
:code-string "[ 0f 77]"
:arch-flags (list "PENT" "MMX")))

(setf ENTER-imm.imm (make-instance 'x86-asm-instruction
:name "ENTER"
:operands "imm,imm"
:code-string "[ij: c8 iw ib,u]"
:arch-flags (list "186")))

(setf EQU-imm (make-instance 'x86-asm-instruction
:name "EQU"
:operands "imm"
:code-string "ignore"
:arch-flags (list "8086")))

(setf EQU-imm.imm (make-instance 'x86-asm-instruction
:name "EQU"
:operands "imm:imm"
:code-string "ignore"
:arch-flags (list "8086")))

(setf F2XM1-void (make-instance 'x86-asm-instruction
:name "F2XM1"
:operands "void"
:code-string "[ d9 f0]"
:arch-flags (list "8086" "FPU")))

(setf FABS-void (make-instance 'x86-asm-instruction
:name "FABS"
:operands "void"
:code-string "[ d9 e1]"
:arch-flags (list "8086" "FPU")))

(setf FADD-mem32 (make-instance 'x86-asm-instruction
:name "FADD"
:operands "mem32"
:code-string "[m: d8 /0]"
:arch-flags (list "8086" "FPU")))

(setf FADD-mem64 (make-instance 'x86-asm-instruction
:name "FADD"
:operands "mem64"
:code-string "[m: dc /0]"
:arch-flags (list "8086" "FPU")))

(setf FADD-fpureg-to (make-instance 'x86-asm-instruction
:name "FADD"
:operands "fpureg|to"
:code-string "[r: dc c0+r]"
:arch-flags (list "8086" "FPU")))

(setf FADD-fpureg (make-instance 'x86-asm-instruction
:name "FADD"
:operands "fpureg"
:code-string "[r: d8 c0+r]"
:arch-flags (list "8086" "FPU")))

(setf FADD-fpureg.fpu0 (make-instance 'x86-asm-instruction
:name "FADD"
:operands "fpureg,fpu0"
:code-string "[r-: dc c0+r]"
:arch-flags (list "8086" "FPU")))

(setf FADD-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FADD"
:operands "fpu0,fpureg"
:code-string "[-r: d8 c0+r]"
:arch-flags (list "8086" "FPU")))

(setf FADD-void (make-instance 'x86-asm-instruction
:name "FADD"
:operands "void"
:code-string "[ de c1]"
:arch-flags (list "8086" "FPU" "ND")))

(setf FADDP-fpureg (make-instance 'x86-asm-instruction
:name "FADDP"
:operands "fpureg"
:code-string "[r: de c0+r]"
:arch-flags (list "8086" "FPU")))

(setf FADDP-fpureg.fpu0 (make-instance 'x86-asm-instruction
:name "FADDP"
:operands "fpureg,fpu0"
:code-string "[r-: de c0+r]"
:arch-flags (list "8086" "FPU")))

(setf FADDP-void (make-instance 'x86-asm-instruction
:name "FADDP"
:operands "void"
:code-string "[ de c1]"
:arch-flags (list "8086" "FPU" "ND")))

(setf FBLD-mem80 (make-instance 'x86-asm-instruction
:name "FBLD"
:operands "mem80"
:code-string "[m: df /4]"
:arch-flags (list "8086" "FPU")))

(setf FBLD-mem (make-instance 'x86-asm-instruction
:name "FBLD"
:operands "mem"
:code-string "[m: df /4]"
:arch-flags (list "8086" "FPU")))

(setf FBSTP-mem80 (make-instance 'x86-asm-instruction
:name "FBSTP"
:operands "mem80"
:code-string "[m: df /6]"
:arch-flags (list "8086" "FPU")))

(setf FBSTP-mem (make-instance 'x86-asm-instruction
:name "FBSTP"
:operands "mem"
:code-string "[m: df /6]"
:arch-flags (list "8086" "FPU")))

(setf FCHS-void (make-instance 'x86-asm-instruction
:name "FCHS"
:operands "void"
:code-string "[ d9 e0]"
:arch-flags (list "8086" "FPU")))

(setf FCLEX-void (make-instance 'x86-asm-instruction
:name "FCLEX"
:operands "void"
:code-string "[ wait db e2]"
:arch-flags (list "8086" "FPU")))

(setf FCMOVB-fpureg (make-instance 'x86-asm-instruction
:name "FCMOVB"
:operands "fpureg"
:code-string "[r: da c0+r]"
:arch-flags (list "P6" "FPU")))

(setf FCMOVB-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FCMOVB"
:operands "fpu0,fpureg"
:code-string "[-r: da c0+r]"
:arch-flags (list "P6" "FPU")))

(setf FCMOVB-void (make-instance 'x86-asm-instruction
:name "FCMOVB"
:operands "void"
:code-string "[ da c1]"
:arch-flags (list "P6" "FPU" "ND")))

(setf FCMOVBE-fpureg (make-instance 'x86-asm-instruction
:name "FCMOVBE"
:operands "fpureg"
:code-string "[r: da d0+r]"
:arch-flags (list "P6" "FPU")))

(setf FCMOVBE-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FCMOVBE"
:operands "fpu0,fpureg"
:code-string "[-r: da d0+r]"
:arch-flags (list "P6" "FPU")))

(setf FCMOVBE-void (make-instance 'x86-asm-instruction
:name "FCMOVBE"
:operands "void"
:code-string "[ da d1]"
:arch-flags (list "P6" "FPU" "ND")))

(setf FCMOVE-fpureg (make-instance 'x86-asm-instruction
:name "FCMOVE"
:operands "fpureg"
:code-string "[r: da c8+r]"
:arch-flags (list "P6" "FPU")))

(setf FCMOVE-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FCMOVE"
:operands "fpu0,fpureg"
:code-string "[-r: da c8+r]"
:arch-flags (list "P6" "FPU")))

(setf FCMOVE-void (make-instance 'x86-asm-instruction
:name "FCMOVE"
:operands "void"
:code-string "[ da c9]"
:arch-flags (list "P6" "FPU" "ND")))

(setf FCMOVNB-fpureg (make-instance 'x86-asm-instruction
:name "FCMOVNB"
:operands "fpureg"
:code-string "[r: db c0+r]"
:arch-flags (list "P6" "FPU")))

(setf FCMOVNB-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FCMOVNB"
:operands "fpu0,fpureg"
:code-string "[-r: db c0+r]"
:arch-flags (list "P6" "FPU")))

(setf FCMOVNB-void (make-instance 'x86-asm-instruction
:name "FCMOVNB"
:operands "void"
:code-string "[ db c1]"
:arch-flags (list "P6" "FPU" "ND")))

(setf FCMOVNBE-fpureg (make-instance 'x86-asm-instruction
:name "FCMOVNBE"
:operands "fpureg"
:code-string "[r: db d0+r]"
:arch-flags (list "P6" "FPU")))

(setf FCMOVNBE-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FCMOVNBE"
:operands "fpu0,fpureg"
:code-string "[-r: db d0+r]"
:arch-flags (list "P6" "FPU")))

(setf FCMOVNBE-void (make-instance 'x86-asm-instruction
:name "FCMOVNBE"
:operands "void"
:code-string "[ db d1]"
:arch-flags (list "P6" "FPU" "ND")))

(setf FCMOVNE-fpureg (make-instance 'x86-asm-instruction
:name "FCMOVNE"
:operands "fpureg"
:code-string "[r: db c8+r]"
:arch-flags (list "P6" "FPU")))

(setf FCMOVNE-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FCMOVNE"
:operands "fpu0,fpureg"
:code-string "[-r: db c8+r]"
:arch-flags (list "P6" "FPU")))

(setf FCMOVNE-void (make-instance 'x86-asm-instruction
:name "FCMOVNE"
:operands "void"
:code-string "[ db c9]"
:arch-flags (list "P6" "FPU" "ND")))

(setf FCMOVNU-fpureg (make-instance 'x86-asm-instruction
:name "FCMOVNU"
:operands "fpureg"
:code-string "[r: db d8+r]"
:arch-flags (list "P6" "FPU")))

(setf FCMOVNU-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FCMOVNU"
:operands "fpu0,fpureg"
:code-string "[-r: db d8+r]"
:arch-flags (list "P6" "FPU")))

(setf FCMOVNU-void (make-instance 'x86-asm-instruction
:name "FCMOVNU"
:operands "void"
:code-string "[ db d9]"
:arch-flags (list "P6" "FPU" "ND")))

(setf FCMOVU-fpureg (make-instance 'x86-asm-instruction
:name "FCMOVU"
:operands "fpureg"
:code-string "[r: da d8+r]"
:arch-flags (list "P6" "FPU")))

(setf FCMOVU-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FCMOVU"
:operands "fpu0,fpureg"
:code-string "[-r: da d8+r]"
:arch-flags (list "P6" "FPU")))

(setf FCMOVU-void (make-instance 'x86-asm-instruction
:name "FCMOVU"
:operands "void"
:code-string "[ da d9]"
:arch-flags (list "P6" "FPU" "ND")))

(setf FCOM-mem32 (make-instance 'x86-asm-instruction
:name "FCOM"
:operands "mem32"
:code-string "[m: d8 /2]"
:arch-flags (list "8086" "FPU")))

(setf FCOM-mem64 (make-instance 'x86-asm-instruction
:name "FCOM"
:operands "mem64"
:code-string "[m: dc /2]"
:arch-flags (list "8086" "FPU")))

(setf FCOM-fpureg (make-instance 'x86-asm-instruction
:name "FCOM"
:operands "fpureg"
:code-string "[r: d8 d0+r]"
:arch-flags (list "8086" "FPU")))

(setf FCOM-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FCOM"
:operands "fpu0,fpureg"
:code-string "[-r: d8 d0+r]"
:arch-flags (list "8086" "FPU")))

(setf FCOM-void (make-instance 'x86-asm-instruction
:name "FCOM"
:operands "void"
:code-string "[ d8 d1]"
:arch-flags (list "8086" "FPU" "ND")))

(setf FCOMI-fpureg (make-instance 'x86-asm-instruction
:name "FCOMI"
:operands "fpureg"
:code-string "[r: db f0+r]"
:arch-flags (list "P6" "FPU")))

(setf FCOMI-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FCOMI"
:operands "fpu0,fpureg"
:code-string "[-r: db f0+r]"
:arch-flags (list "P6" "FPU")))

(setf FCOMI-void (make-instance 'x86-asm-instruction
:name "FCOMI"
:operands "void"
:code-string "[ db f1]"
:arch-flags (list "P6" "FPU" "ND")))

(setf FCOMIP-fpureg (make-instance 'x86-asm-instruction
:name "FCOMIP"
:operands "fpureg"
:code-string "[r: df f0+r]"
:arch-flags (list "P6" "FPU")))

(setf FCOMIP-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FCOMIP"
:operands "fpu0,fpureg"
:code-string "[-r: df f0+r]"
:arch-flags (list "P6" "FPU")))

(setf FCOMIP-void (make-instance 'x86-asm-instruction
:name "FCOMIP"
:operands "void"
:code-string "[ df f1]"
:arch-flags (list "P6" "FPU" "ND")))

(setf FCOMP-mem32 (make-instance 'x86-asm-instruction
:name "FCOMP"
:operands "mem32"
:code-string "[m: d8 /3]"
:arch-flags (list "8086" "FPU")))

(setf FCOMP-mem64 (make-instance 'x86-asm-instruction
:name "FCOMP"
:operands "mem64"
:code-string "[m: dc /3]"
:arch-flags (list "8086" "FPU")))

(setf FCOMP-fpureg (make-instance 'x86-asm-instruction
:name "FCOMP"
:operands "fpureg"
:code-string "[r: d8 d8+r]"
:arch-flags (list "8086" "FPU")))

(setf FCOMP-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FCOMP"
:operands "fpu0,fpureg"
:code-string "[-r: d8 d8+r]"
:arch-flags (list "8086" "FPU")))

(setf FCOMP-void (make-instance 'x86-asm-instruction
:name "FCOMP"
:operands "void"
:code-string "[ d8 d9]"
:arch-flags (list "8086" "FPU" "ND")))

(setf FCOMPP-void (make-instance 'x86-asm-instruction
:name "FCOMPP"
:operands "void"
:code-string "[ de d9]"
:arch-flags (list "8086" "FPU")))

(setf FCOS-void (make-instance 'x86-asm-instruction
:name "FCOS"
:operands "void"
:code-string "[ d9 ff]"
:arch-flags (list "386" "FPU")))

(setf FDECSTP-void (make-instance 'x86-asm-instruction
:name "FDECSTP"
:operands "void"
:code-string "[ d9 f6]"
:arch-flags (list "8086" "FPU")))

(setf FDISI-void (make-instance 'x86-asm-instruction
:name "FDISI"
:operands "void"
:code-string "[ wait db e1]"
:arch-flags (list "8086" "FPU")))

(setf FDIV-mem32 (make-instance 'x86-asm-instruction
:name "FDIV"
:operands "mem32"
:code-string "[m: d8 /6]"
:arch-flags (list "8086" "FPU")))

(setf FDIV-mem64 (make-instance 'x86-asm-instruction
:name "FDIV"
:operands "mem64"
:code-string "[m: dc /6]"
:arch-flags (list "8086" "FPU")))

(setf FDIV-fpureg-to (make-instance 'x86-asm-instruction
:name "FDIV"
:operands "fpureg|to"
:code-string "[r: dc f8+r]"
:arch-flags (list "8086" "FPU")))

(setf FDIV-fpureg (make-instance 'x86-asm-instruction
:name "FDIV"
:operands "fpureg"
:code-string "[r: d8 f0+r]"
:arch-flags (list "8086" "FPU")))

(setf FDIV-fpureg.fpu0 (make-instance 'x86-asm-instruction
:name "FDIV"
:operands "fpureg,fpu0"
:code-string "[r-: dc f8+r]"
:arch-flags (list "8086" "FPU")))

(setf FDIV-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FDIV"
:operands "fpu0,fpureg"
:code-string "[-r: d8 f0+r]"
:arch-flags (list "8086" "FPU")))

(setf FDIV-void (make-instance 'x86-asm-instruction
:name "FDIV"
:operands "void"
:code-string "[ de f9]"
:arch-flags (list "8086" "FPU" "ND")))

(setf FDIVP-fpureg (make-instance 'x86-asm-instruction
:name "FDIVP"
:operands "fpureg"
:code-string "[r: de f8+r]"
:arch-flags (list "8086" "FPU")))

(setf FDIVP-fpureg.fpu0 (make-instance 'x86-asm-instruction
:name "FDIVP"
:operands "fpureg,fpu0"
:code-string "[r-: de f8+r]"
:arch-flags (list "8086" "FPU")))

(setf FDIVP-void (make-instance 'x86-asm-instruction
:name "FDIVP"
:operands "void"
:code-string "[ de f9]"
:arch-flags (list "8086" "FPU" "ND")))

(setf FDIVR-mem32 (make-instance 'x86-asm-instruction
:name "FDIVR"
:operands "mem32"
:code-string "[m: d8 /7]"
:arch-flags (list "8086" "FPU")))

(setf FDIVR-mem64 (make-instance 'x86-asm-instruction
:name "FDIVR"
:operands "mem64"
:code-string "[m: dc /7]"
:arch-flags (list "8086" "FPU")))

(setf FDIVR-fpureg-to (make-instance 'x86-asm-instruction
:name "FDIVR"
:operands "fpureg|to"
:code-string "[r: dc f0+r]"
:arch-flags (list "8086" "FPU")))

(setf FDIVR-fpureg.fpu0 (make-instance 'x86-asm-instruction
:name "FDIVR"
:operands "fpureg,fpu0"
:code-string "[r-: dc f0+r]"
:arch-flags (list "8086" "FPU")))

(setf FDIVR-fpureg (make-instance 'x86-asm-instruction
:name "FDIVR"
:operands "fpureg"
:code-string "[r: d8 f8+r]"
:arch-flags (list "8086" "FPU")))

(setf FDIVR-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FDIVR"
:operands "fpu0,fpureg"
:code-string "[-r: d8 f8+r]"
:arch-flags (list "8086" "FPU")))

(setf FDIVR-void (make-instance 'x86-asm-instruction
:name "FDIVR"
:operands "void"
:code-string "[ de f1]"
:arch-flags (list "8086" "FPU" "ND")))

(setf FDIVRP-fpureg (make-instance 'x86-asm-instruction
:name "FDIVRP"
:operands "fpureg"
:code-string "[r: de f0+r]"
:arch-flags (list "8086" "FPU")))

(setf FDIVRP-fpureg.fpu0 (make-instance 'x86-asm-instruction
:name "FDIVRP"
:operands "fpureg,fpu0"
:code-string "[r-: de f0+r]"
:arch-flags (list "8086" "FPU")))

(setf FDIVRP-void (make-instance 'x86-asm-instruction
:name "FDIVRP"
:operands "void"
:code-string "[ de f1]"
:arch-flags (list "8086" "FPU" "ND")))

(setf FEMMS-void (make-instance 'x86-asm-instruction
:name "FEMMS"
:operands "void"
:code-string "[ 0f 0e]"
:arch-flags (list "PENT" "3DNOW")))

(setf FENI-void (make-instance 'x86-asm-instruction
:name "FENI"
:operands "void"
:code-string "[ wait db e0]"
:arch-flags (list "8086" "FPU")))

(setf FFREE-fpureg (make-instance 'x86-asm-instruction
:name "FFREE"
:operands "fpureg"
:code-string "[r: dd c0+r]"
:arch-flags (list "8086" "FPU")))

(setf FFREE-void (make-instance 'x86-asm-instruction
:name "FFREE"
:operands "void"
:code-string "[ dd c1]"
:arch-flags (list "8086" "FPU")))

(setf FFREEP-fpureg (make-instance 'x86-asm-instruction
:name "FFREEP"
:operands "fpureg"
:code-string "[r: df c0+r]"
:arch-flags (list "286" "FPU" "UNDOC")))

(setf FFREEP-void (make-instance 'x86-asm-instruction
:name "FFREEP"
:operands "void"
:code-string "[ df c1]"
:arch-flags (list "286" "FPU" "UNDOC")))

(setf FIADD-mem32 (make-instance 'x86-asm-instruction
:name "FIADD"
:operands "mem32"
:code-string "[m: da /0]"
:arch-flags (list "8086" "FPU")))

(setf FIADD-mem16 (make-instance 'x86-asm-instruction
:name "FIADD"
:operands "mem16"
:code-string "[m: de /0]"
:arch-flags (list "8086" "FPU")))

(setf FICOM-mem32 (make-instance 'x86-asm-instruction
:name "FICOM"
:operands "mem32"
:code-string "[m: da /2]"
:arch-flags (list "8086" "FPU")))

(setf FICOM-mem16 (make-instance 'x86-asm-instruction
:name "FICOM"
:operands "mem16"
:code-string "[m: de /2]"
:arch-flags (list "8086" "FPU")))

(setf FICOMP-mem32 (make-instance 'x86-asm-instruction
:name "FICOMP"
:operands "mem32"
:code-string "[m: da /3]"
:arch-flags (list "8086" "FPU")))

(setf FICOMP-mem16 (make-instance 'x86-asm-instruction
:name "FICOMP"
:operands "mem16"
:code-string "[m: de /3]"
:arch-flags (list "8086" "FPU")))

(setf FIDIV-mem32 (make-instance 'x86-asm-instruction
:name "FIDIV"
:operands "mem32"
:code-string "[m: da /6]"
:arch-flags (list "8086" "FPU")))

(setf FIDIV-mem16 (make-instance 'x86-asm-instruction
:name "FIDIV"
:operands "mem16"
:code-string "[m: de /6]"
:arch-flags (list "8086" "FPU")))

(setf FIDIVR-mem32 (make-instance 'x86-asm-instruction
:name "FIDIVR"
:operands "mem32"
:code-string "[m: da /7]"
:arch-flags (list "8086" "FPU")))

(setf FIDIVR-mem16 (make-instance 'x86-asm-instruction
:name "FIDIVR"
:operands "mem16"
:code-string "[m: de /7]"
:arch-flags (list "8086" "FPU")))

(setf FILD-mem32 (make-instance 'x86-asm-instruction
:name "FILD"
:operands "mem32"
:code-string "[m: db /0]"
:arch-flags (list "8086" "FPU")))

(setf FILD-mem16 (make-instance 'x86-asm-instruction
:name "FILD"
:operands "mem16"
:code-string "[m: df /0]"
:arch-flags (list "8086" "FPU")))

(setf FILD-mem64 (make-instance 'x86-asm-instruction
:name "FILD"
:operands "mem64"
:code-string "[m: df /5]"
:arch-flags (list "8086" "FPU")))

(setf FIMUL-mem32 (make-instance 'x86-asm-instruction
:name "FIMUL"
:operands "mem32"
:code-string "[m: da /1]"
:arch-flags (list "8086" "FPU")))

(setf FIMUL-mem16 (make-instance 'x86-asm-instruction
:name "FIMUL"
:operands "mem16"
:code-string "[m: de /1]"
:arch-flags (list "8086" "FPU")))

(setf FINCSTP-void (make-instance 'x86-asm-instruction
:name "FINCSTP"
:operands "void"
:code-string "[ d9 f7]"
:arch-flags (list "8086" "FPU")))

(setf FINIT-void (make-instance 'x86-asm-instruction
:name "FINIT"
:operands "void"
:code-string "[ wait db e3]"
:arch-flags (list "8086" "FPU")))

(setf FIST-mem32 (make-instance 'x86-asm-instruction
:name "FIST"
:operands "mem32"
:code-string "[m: db /2]"
:arch-flags (list "8086" "FPU")))

(setf FIST-mem16 (make-instance 'x86-asm-instruction
:name "FIST"
:operands "mem16"
:code-string "[m: df /2]"
:arch-flags (list "8086" "FPU")))

(setf FISTP-mem32 (make-instance 'x86-asm-instruction
:name "FISTP"
:operands "mem32"
:code-string "[m: db /3]"
:arch-flags (list "8086" "FPU")))

(setf FISTP-mem16 (make-instance 'x86-asm-instruction
:name "FISTP"
:operands "mem16"
:code-string "[m: df /3]"
:arch-flags (list "8086" "FPU")))

(setf FISTP-mem64 (make-instance 'x86-asm-instruction
:name "FISTP"
:operands "mem64"
:code-string "[m: df /7]"
:arch-flags (list "8086" "FPU")))

(setf FISTTP-mem16 (make-instance 'x86-asm-instruction
:name "FISTTP"
:operands "mem16"
:code-string "[m: df /1]"
:arch-flags (list "PRESCOTT" "FPU")))

(setf FISTTP-mem32 (make-instance 'x86-asm-instruction
:name "FISTTP"
:operands "mem32"
:code-string "[m: db /1]"
:arch-flags (list "PRESCOTT" "FPU")))

(setf FISTTP-mem64 (make-instance 'x86-asm-instruction
:name "FISTTP"
:operands "mem64"
:code-string "[m: dd /1]"
:arch-flags (list "PRESCOTT" "FPU")))

(setf FISUB-mem32 (make-instance 'x86-asm-instruction
:name "FISUB"
:operands "mem32"
:code-string "[m: da /4]"
:arch-flags (list "8086" "FPU")))

(setf FISUB-mem16 (make-instance 'x86-asm-instruction
:name "FISUB"
:operands "mem16"
:code-string "[m: de /4]"
:arch-flags (list "8086" "FPU")))

(setf FISUBR-mem32 (make-instance 'x86-asm-instruction
:name "FISUBR"
:operands "mem32"
:code-string "[m: da /5]"
:arch-flags (list "8086" "FPU")))

(setf FISUBR-mem16 (make-instance 'x86-asm-instruction
:name "FISUBR"
:operands "mem16"
:code-string "[m: de /5]"
:arch-flags (list "8086" "FPU")))

(setf FLD-mem32 (make-instance 'x86-asm-instruction
:name "FLD"
:operands "mem32"
:code-string "[m: d9 /0]"
:arch-flags (list "8086" "FPU")))

(setf FLD-mem64 (make-instance 'x86-asm-instruction
:name "FLD"
:operands "mem64"
:code-string "[m: dd /0]"
:arch-flags (list "8086" "FPU")))

(setf FLD-mem80 (make-instance 'x86-asm-instruction
:name "FLD"
:operands "mem80"
:code-string "[m: db /5]"
:arch-flags (list "8086" "FPU")))

(setf FLD-fpureg (make-instance 'x86-asm-instruction
:name "FLD"
:operands "fpureg"
:code-string "[r: d9 c0+r]"
:arch-flags (list "8086" "FPU")))

(setf FLD-void (make-instance 'x86-asm-instruction
:name "FLD"
:operands "void"
:code-string "[ d9 c1]"
:arch-flags (list "8086" "FPU" "ND")))

(setf FLD1-void (make-instance 'x86-asm-instruction
:name "FLD1"
:operands "void"
:code-string "[ d9 e8]"
:arch-flags (list "8086" "FPU")))

(setf FLDCW-mem (make-instance 'x86-asm-instruction
:name "FLDCW"
:operands "mem"
:code-string "[m: d9 /5]"
:arch-flags (list "8086" "FPU" "SW")))

(setf FLDENV-mem (make-instance 'x86-asm-instruction
:name "FLDENV"
:operands "mem"
:code-string "[m: d9 /4]"
:arch-flags (list "8086" "FPU")))

(setf FLDL2E-void (make-instance 'x86-asm-instruction
:name "FLDL2E"
:operands "void"
:code-string "[ d9 ea]"
:arch-flags (list "8086" "FPU")))

(setf FLDL2T-void (make-instance 'x86-asm-instruction
:name "FLDL2T"
:operands "void"
:code-string "[ d9 e9]"
:arch-flags (list "8086" "FPU")))

(setf FLDLG2-void (make-instance 'x86-asm-instruction
:name "FLDLG2"
:operands "void"
:code-string "[ d9 ec]"
:arch-flags (list "8086" "FPU")))

(setf FLDLN2-void (make-instance 'x86-asm-instruction
:name "FLDLN2"
:operands "void"
:code-string "[ d9 ed]"
:arch-flags (list "8086" "FPU")))

(setf FLDPI-void (make-instance 'x86-asm-instruction
:name "FLDPI"
:operands "void"
:code-string "[ d9 eb]"
:arch-flags (list "8086" "FPU")))

(setf FLDZ-void (make-instance 'x86-asm-instruction
:name "FLDZ"
:operands "void"
:code-string "[ d9 ee]"
:arch-flags (list "8086" "FPU")))

(setf FMUL-mem32 (make-instance 'x86-asm-instruction
:name "FMUL"
:operands "mem32"
:code-string "[m: d8 /1]"
:arch-flags (list "8086" "FPU")))

(setf FMUL-mem64 (make-instance 'x86-asm-instruction
:name "FMUL"
:operands "mem64"
:code-string "[m: dc /1]"
:arch-flags (list "8086" "FPU")))

(setf FMUL-fpureg-to (make-instance 'x86-asm-instruction
:name "FMUL"
:operands "fpureg|to"
:code-string "[r: dc c8+r]"
:arch-flags (list "8086" "FPU")))

(setf FMUL-fpureg.fpu0 (make-instance 'x86-asm-instruction
:name "FMUL"
:operands "fpureg,fpu0"
:code-string "[r-: dc c8+r]"
:arch-flags (list "8086" "FPU")))

(setf FMUL-fpureg (make-instance 'x86-asm-instruction
:name "FMUL"
:operands "fpureg"
:code-string "[r: d8 c8+r]"
:arch-flags (list "8086" "FPU")))

(setf FMUL-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FMUL"
:operands "fpu0,fpureg"
:code-string "[-r: d8 c8+r]"
:arch-flags (list "8086" "FPU")))

(setf FMUL-void (make-instance 'x86-asm-instruction
:name "FMUL"
:operands "void"
:code-string "[ de c9]"
:arch-flags (list "8086" "FPU" "ND")))

(setf FMULP-fpureg (make-instance 'x86-asm-instruction
:name "FMULP"
:operands "fpureg"
:code-string "[r: de c8+r]"
:arch-flags (list "8086" "FPU")))

(setf FMULP-fpureg.fpu0 (make-instance 'x86-asm-instruction
:name "FMULP"
:operands "fpureg,fpu0"
:code-string "[r-: de c8+r]"
:arch-flags (list "8086" "FPU")))

(setf FMULP-void (make-instance 'x86-asm-instruction
:name "FMULP"
:operands "void"
:code-string "[ de c9]"
:arch-flags (list "8086" "FPU" "ND")))

(setf FNCLEX-void (make-instance 'x86-asm-instruction
:name "FNCLEX"
:operands "void"
:code-string "[ db e2]"
:arch-flags (list "8086" "FPU")))

(setf FNDISI-void (make-instance 'x86-asm-instruction
:name "FNDISI"
:operands "void"
:code-string "[ db e1]"
:arch-flags (list "8086" "FPU")))

(setf FNENI-void (make-instance 'x86-asm-instruction
:name "FNENI"
:operands "void"
:code-string "[ db e0]"
:arch-flags (list "8086" "FPU")))

(setf FNINIT-void (make-instance 'x86-asm-instruction
:name "FNINIT"
:operands "void"
:code-string "[ db e3]"
:arch-flags (list "8086" "FPU")))

(setf FNOP-void (make-instance 'x86-asm-instruction
:name "FNOP"
:operands "void"
:code-string "[ d9 d0]"
:arch-flags (list "8086" "FPU")))

(setf FNSAVE-mem (make-instance 'x86-asm-instruction
:name "FNSAVE"
:operands "mem"
:code-string "[m: dd /6]"
:arch-flags (list "8086" "FPU")))

(setf FNSTCW-mem (make-instance 'x86-asm-instruction
:name "FNSTCW"
:operands "mem"
:code-string "[m: d9 /7]"
:arch-flags (list "8086" "FPU" "SW")))

(setf FNSTENV-mem (make-instance 'x86-asm-instruction
:name "FNSTENV"
:operands "mem"
:code-string "[m: d9 /6]"
:arch-flags (list "8086" "FPU")))

(setf FNSTSW-mem (make-instance 'x86-asm-instruction
:name "FNSTSW"
:operands "mem"
:code-string "[m: dd /7]"
:arch-flags (list "8086" "FPU" "SW")))

(setf FNSTSW-reg_ax (make-instance 'x86-asm-instruction
:name "FNSTSW"
:operands "reg_ax"
:code-string "[-: df e0]"
:arch-flags (list "286" "FPU")))

(setf FPATAN-void (make-instance 'x86-asm-instruction
:name "FPATAN"
:operands "void"
:code-string "[ d9 f3]"
:arch-flags (list "8086" "FPU")))

(setf FPREM-void (make-instance 'x86-asm-instruction
:name "FPREM"
:operands "void"
:code-string "[ d9 f8]"
:arch-flags (list "8086" "FPU")))

(setf FPREM1-void (make-instance 'x86-asm-instruction
:name "FPREM1"
:operands "void"
:code-string "[ d9 f5]"
:arch-flags (list "386" "FPU")))

(setf FPTAN-void (make-instance 'x86-asm-instruction
:name "FPTAN"
:operands "void"
:code-string "[ d9 f2]"
:arch-flags (list "8086" "FPU")))

(setf FRNDINT-void (make-instance 'x86-asm-instruction
:name "FRNDINT"
:operands "void"
:code-string "[ d9 fc]"
:arch-flags (list "8086" "FPU")))

(setf FRSTOR-mem (make-instance 'x86-asm-instruction
:name "FRSTOR"
:operands "mem"
:code-string "[m: dd /4]"
:arch-flags (list "8086" "FPU")))

(setf FSAVE-mem (make-instance 'x86-asm-instruction
:name "FSAVE"
:operands "mem"
:code-string "[m: wait dd /6]"
:arch-flags (list "8086" "FPU")))

(setf FSCALE-void (make-instance 'x86-asm-instruction
:name "FSCALE"
:operands "void"
:code-string "[ d9 fd]"
:arch-flags (list "8086" "FPU")))

(setf FSETPM-void (make-instance 'x86-asm-instruction
:name "FSETPM"
:operands "void"
:code-string "[ db e4]"
:arch-flags (list "286" "FPU")))

(setf FSIN-void (make-instance 'x86-asm-instruction
:name "FSIN"
:operands "void"
:code-string "[ d9 fe]"
:arch-flags (list "386" "FPU")))

(setf FSINCOS-void (make-instance 'x86-asm-instruction
:name "FSINCOS"
:operands "void"
:code-string "[ d9 fb]"
:arch-flags (list "386" "FPU")))

(setf FSQRT-void (make-instance 'x86-asm-instruction
:name "FSQRT"
:operands "void"
:code-string "[ d9 fa]"
:arch-flags (list "8086" "FPU")))

(setf FST-mem32 (make-instance 'x86-asm-instruction
:name "FST"
:operands "mem32"
:code-string "[m: d9 /2]"
:arch-flags (list "8086" "FPU")))

(setf FST-mem64 (make-instance 'x86-asm-instruction
:name "FST"
:operands "mem64"
:code-string "[m: dd /2]"
:arch-flags (list "8086" "FPU")))

(setf FST-fpureg (make-instance 'x86-asm-instruction
:name "FST"
:operands "fpureg"
:code-string "[r: dd d0+r]"
:arch-flags (list "8086" "FPU")))

(setf FST-void (make-instance 'x86-asm-instruction
:name "FST"
:operands "void"
:code-string "[ dd d1]"
:arch-flags (list "8086" "FPU" "ND")))

(setf FSTCW-mem (make-instance 'x86-asm-instruction
:name "FSTCW"
:operands "mem"
:code-string "[m: wait d9 /7]"
:arch-flags (list "8086" "FPU" "SW")))

(setf FSTENV-mem (make-instance 'x86-asm-instruction
:name "FSTENV"
:operands "mem"
:code-string "[m: wait d9 /6]"
:arch-flags (list "8086" "FPU")))

(setf FSTP-mem32 (make-instance 'x86-asm-instruction
:name "FSTP"
:operands "mem32"
:code-string "[m: d9 /3]"
:arch-flags (list "8086" "FPU")))

(setf FSTP-mem64 (make-instance 'x86-asm-instruction
:name "FSTP"
:operands "mem64"
:code-string "[m: dd /3]"
:arch-flags (list "8086" "FPU")))

(setf FSTP-mem80 (make-instance 'x86-asm-instruction
:name "FSTP"
:operands "mem80"
:code-string "[m: db /7]"
:arch-flags (list "8086" "FPU")))

(setf FSTP-fpureg (make-instance 'x86-asm-instruction
:name "FSTP"
:operands "fpureg"
:code-string "[r: dd d8+r]"
:arch-flags (list "8086" "FPU")))

(setf FSTP-void (make-instance 'x86-asm-instruction
:name "FSTP"
:operands "void"
:code-string "[ dd d9]"
:arch-flags (list "8086" "FPU" "ND")))

(setf FSTSW-mem (make-instance 'x86-asm-instruction
:name "FSTSW"
:operands "mem"
:code-string "[m: wait dd /7]"
:arch-flags (list "8086" "FPU" "SW")))

(setf FSTSW-reg_ax (make-instance 'x86-asm-instruction
:name "FSTSW"
:operands "reg_ax"
:code-string "[-: wait df e0]"
:arch-flags (list "286" "FPU")))

(setf FSUB-mem32 (make-instance 'x86-asm-instruction
:name "FSUB"
:operands "mem32"
:code-string "[m: d8 /4]"
:arch-flags (list "8086" "FPU")))

(setf FSUB-mem64 (make-instance 'x86-asm-instruction
:name "FSUB"
:operands "mem64"
:code-string "[m: dc /4]"
:arch-flags (list "8086" "FPU")))

(setf FSUB-fpureg-to (make-instance 'x86-asm-instruction
:name "FSUB"
:operands "fpureg|to"
:code-string "[r: dc e8+r]"
:arch-flags (list "8086" "FPU")))

(setf FSUB-fpureg.fpu0 (make-instance 'x86-asm-instruction
:name "FSUB"
:operands "fpureg,fpu0"
:code-string "[r-: dc e8+r]"
:arch-flags (list "8086" "FPU")))

(setf FSUB-fpureg (make-instance 'x86-asm-instruction
:name "FSUB"
:operands "fpureg"
:code-string "[r: d8 e0+r]"
:arch-flags (list "8086" "FPU")))

(setf FSUB-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FSUB"
:operands "fpu0,fpureg"
:code-string "[-r: d8 e0+r]"
:arch-flags (list "8086" "FPU")))

(setf FSUB-void (make-instance 'x86-asm-instruction
:name "FSUB"
:operands "void"
:code-string "[ de e9]"
:arch-flags (list "8086" "FPU" "ND")))

(setf FSUBP-fpureg (make-instance 'x86-asm-instruction
:name "FSUBP"
:operands "fpureg"
:code-string "[r: de e8+r]"
:arch-flags (list "8086" "FPU")))

(setf FSUBP-fpureg.fpu0 (make-instance 'x86-asm-instruction
:name "FSUBP"
:operands "fpureg,fpu0"
:code-string "[r-: de e8+r]"
:arch-flags (list "8086" "FPU")))

(setf FSUBP-void (make-instance 'x86-asm-instruction
:name "FSUBP"
:operands "void"
:code-string "[ de e9]"
:arch-flags (list "8086" "FPU" "ND")))

(setf FSUBR-mem32 (make-instance 'x86-asm-instruction
:name "FSUBR"
:operands "mem32"
:code-string "[m: d8 /5]"
:arch-flags (list "8086" "FPU")))

(setf FSUBR-mem64 (make-instance 'x86-asm-instruction
:name "FSUBR"
:operands "mem64"
:code-string "[m: dc /5]"
:arch-flags (list "8086" "FPU")))

(setf FSUBR-fpureg-to (make-instance 'x86-asm-instruction
:name "FSUBR"
:operands "fpureg|to"
:code-string "[r: dc e0+r]"
:arch-flags (list "8086" "FPU")))

(setf FSUBR-fpureg.fpu0 (make-instance 'x86-asm-instruction
:name "FSUBR"
:operands "fpureg,fpu0"
:code-string "[r-: dc e0+r]"
:arch-flags (list "8086" "FPU")))

(setf FSUBR-fpureg (make-instance 'x86-asm-instruction
:name "FSUBR"
:operands "fpureg"
:code-string "[r: d8 e8+r]"
:arch-flags (list "8086" "FPU")))

(setf FSUBR-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FSUBR"
:operands "fpu0,fpureg"
:code-string "[-r: d8 e8+r]"
:arch-flags (list "8086" "FPU")))

(setf FSUBR-void (make-instance 'x86-asm-instruction
:name "FSUBR"
:operands "void"
:code-string "[ de e1]"
:arch-flags (list "8086" "FPU" "ND")))

(setf FSUBRP-fpureg (make-instance 'x86-asm-instruction
:name "FSUBRP"
:operands "fpureg"
:code-string "[r: de e0+r]"
:arch-flags (list "8086" "FPU")))

(setf FSUBRP-fpureg.fpu0 (make-instance 'x86-asm-instruction
:name "FSUBRP"
:operands "fpureg,fpu0"
:code-string "[r-: de e0+r]"
:arch-flags (list "8086" "FPU")))

(setf FSUBRP-void (make-instance 'x86-asm-instruction
:name "FSUBRP"
:operands "void"
:code-string "[ de e1]"
:arch-flags (list "8086" "FPU" "ND")))

(setf FTST-void (make-instance 'x86-asm-instruction
:name "FTST"
:operands "void"
:code-string "[ d9 e4]"
:arch-flags (list "8086" "FPU")))

(setf FUCOM-fpureg (make-instance 'x86-asm-instruction
:name "FUCOM"
:operands "fpureg"
:code-string "[r: dd e0+r]"
:arch-flags (list "386" "FPU")))

(setf FUCOM-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FUCOM"
:operands "fpu0,fpureg"
:code-string "[-r: dd e0+r]"
:arch-flags (list "386" "FPU")))

(setf FUCOM-void (make-instance 'x86-asm-instruction
:name "FUCOM"
:operands "void"
:code-string "[ dd e1]"
:arch-flags (list "386" "FPU" "ND")))

(setf FUCOMI-fpureg (make-instance 'x86-asm-instruction
:name "FUCOMI"
:operands "fpureg"
:code-string "[r: db e8+r]"
:arch-flags (list "P6" "FPU")))

(setf FUCOMI-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FUCOMI"
:operands "fpu0,fpureg"
:code-string "[-r: db e8+r]"
:arch-flags (list "P6" "FPU")))

(setf FUCOMI-void (make-instance 'x86-asm-instruction
:name "FUCOMI"
:operands "void"
:code-string "[ db e9]"
:arch-flags (list "P6" "FPU" "ND")))

(setf FUCOMIP-fpureg (make-instance 'x86-asm-instruction
:name "FUCOMIP"
:operands "fpureg"
:code-string "[r: df e8+r]"
:arch-flags (list "P6" "FPU")))

(setf FUCOMIP-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FUCOMIP"
:operands "fpu0,fpureg"
:code-string "[-r: df e8+r]"
:arch-flags (list "P6" "FPU")))

(setf FUCOMIP-void (make-instance 'x86-asm-instruction
:name "FUCOMIP"
:operands "void"
:code-string "[ df e9]"
:arch-flags (list "P6" "FPU" "ND")))

(setf FUCOMP-fpureg (make-instance 'x86-asm-instruction
:name "FUCOMP"
:operands "fpureg"
:code-string "[r: dd e8+r]"
:arch-flags (list "386" "FPU")))

(setf FUCOMP-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FUCOMP"
:operands "fpu0,fpureg"
:code-string "[-r: dd e8+r]"
:arch-flags (list "386" "FPU")))

(setf FUCOMP-void (make-instance 'x86-asm-instruction
:name "FUCOMP"
:operands "void"
:code-string "[ dd e9]"
:arch-flags (list "386" "FPU" "ND")))

(setf FUCOMPP-void (make-instance 'x86-asm-instruction
:name "FUCOMPP"
:operands "void"
:code-string "[ da e9]"
:arch-flags (list "386" "FPU")))

(setf FXAM-void (make-instance 'x86-asm-instruction
:name "FXAM"
:operands "void"
:code-string "[ d9 e5]"
:arch-flags (list "8086" "FPU")))

(setf FXCH-fpureg (make-instance 'x86-asm-instruction
:name "FXCH"
:operands "fpureg"
:code-string "[r: d9 c8+r]"
:arch-flags (list "8086" "FPU")))

(setf FXCH-fpureg.fpu0 (make-instance 'x86-asm-instruction
:name "FXCH"
:operands "fpureg,fpu0"
:code-string "[r-: d9 c8+r]"
:arch-flags (list "8086" "FPU")))

(setf FXCH-fpu0.fpureg (make-instance 'x86-asm-instruction
:name "FXCH"
:operands "fpu0,fpureg"
:code-string "[-r: d9 c8+r]"
:arch-flags (list "8086" "FPU")))

(setf FXCH-void (make-instance 'x86-asm-instruction
:name "FXCH"
:operands "void"
:code-string "[ d9 c9]"
:arch-flags (list "8086" "FPU" "ND")))

(setf FXTRACT-void (make-instance 'x86-asm-instruction
:name "FXTRACT"
:operands "void"
:code-string "[ d9 f4]"
:arch-flags (list "8086" "FPU")))

(setf FYL2X-void (make-instance 'x86-asm-instruction
:name "FYL2X"
:operands "void"
:code-string "[ d9 f1]"
:arch-flags (list "8086" "FPU")))

(setf FYL2XP1-void (make-instance 'x86-asm-instruction
:name "FYL2XP1"
:operands "void"
:code-string "[ d9 f9]"
:arch-flags (list "8086" "FPU")))

(setf HLT-void (make-instance 'x86-asm-instruction
:name "HLT"
:operands "void"
:code-string "[ f4]"
:arch-flags (list "8086" "PRIV")))

(setf IBTS-mem.reg16 (make-instance 'x86-asm-instruction
:name "IBTS"
:operands "mem,reg16"
:code-string "[mr: o16 0f a7 /r]"
:arch-flags (list "386" "SW" "UNDOC" "ND")))

(setf IBTS-reg16.reg16 (make-instance 'x86-asm-instruction
:name "IBTS"
:operands "reg16,reg16"
:code-string "[mr: o16 0f a7 /r]"
:arch-flags (list "386" "UNDOC" "ND")))

(setf IBTS-mem.reg32 (make-instance 'x86-asm-instruction
:name "IBTS"
:operands "mem,reg32"
:code-string "[mr: o32 0f a7 /r]"
:arch-flags (list "386" "SD" "UNDOC" "ND")))

(setf IBTS-reg32.reg32 (make-instance 'x86-asm-instruction
:name "IBTS"
:operands "reg32,reg32"
:code-string "[mr: o32 0f a7 /r]"
:arch-flags (list "386" "UNDOC" "ND")))

(setf ICEBP-void (make-instance 'x86-asm-instruction
:name "ICEBP"
:operands "void"
:code-string "[ f1]"
:arch-flags (list "386" "ND")))

(setf IDIV-rm8 (make-instance 'x86-asm-instruction
:name "IDIV"
:operands "rm8"
:code-string "[m: f6 /7]"
:arch-flags (list "8086")))

(setf IDIV-rm16 (make-instance 'x86-asm-instruction
:name "IDIV"
:operands "rm16"
:code-string "[m: o16 f7 /7]"
:arch-flags (list "8086")))

(setf IDIV-rm32 (make-instance 'x86-asm-instruction
:name "IDIV"
:operands "rm32"
:code-string "[m: o32 f7 /7]"
:arch-flags (list "386")))

(setf IDIV-rm64 (make-instance 'x86-asm-instruction
:name "IDIV"
:operands "rm64"
:code-string "[m: o64 f7 /7]"
:arch-flags (list "X64")))

(setf IMUL-rm8 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "rm8"
:code-string "[m: f6 /5]"
:arch-flags (list "8086")))

(setf IMUL-rm16 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "rm16"
:code-string "[m: o16 f7 /5]"
:arch-flags (list "8086")))

(setf IMUL-rm32 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "rm32"
:code-string "[m: o32 f7 /5]"
:arch-flags (list "386")))

(setf IMUL-rm64 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "rm64"
:code-string "[m: o64 f7 /5]"
:arch-flags (list "X64")))

(setf IMUL-reg16.mem (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg16,mem"
:code-string "[rm: o16 0f af /r]"
:arch-flags (list "386" "SM")))

(setf IMUL-reg16.reg16 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg16,reg16"
:code-string "[rm: o16 0f af /r]"
:arch-flags (list "386")))

(setf IMUL-reg32.mem (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg32,mem"
:code-string "[rm: o32 0f af /r]"
:arch-flags (list "386" "SM")))

(setf IMUL-reg32.reg32 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg32,reg32"
:code-string "[rm: o32 0f af /r]"
:arch-flags (list "386")))

(setf IMUL-reg64.mem (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg64,mem"
:code-string "[rm: o64 0f af /r]"
:arch-flags (list "X64" "SM")))

(setf IMUL-reg64.reg64 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg64,reg64"
:code-string "[rm: o64 0f af /r]"
:arch-flags (list "X64")))

(setf IMUL-reg16.mem.imm8 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg16,mem,imm8"
:code-string "[rmi: o16 6b /r ib,s]"
:arch-flags (list "186" "SM")))

(setf IMUL-reg16.mem.sbyteword (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg16,mem,sbyteword"
:code-string "[rmi: o16 6b /r ib,s]"
:arch-flags (list "186" "SM" "ND")))

(setf IMUL-reg16.mem.imm16 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg16,mem,imm16"
:code-string "[rmi: o16 69 /r iw]"
:arch-flags (list "186" "SM")))

(setf IMUL-reg16.mem.imm (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg16,mem,imm"
:code-string "[rmi: o16 69 /r iw]"
:arch-flags (list "186" "SM" "ND")))

(setf IMUL-reg16.reg16.imm8 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg16,reg16,imm8"
:code-string "[rmi: o16 6b /r ib,s]"
:arch-flags (list "186")))

(setf IMUL-reg16.reg16.sbyteword (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg16,reg16,sbyteword"
:code-string "[rmi: o16 6b /r ib,s]"
:arch-flags (list "186" "SM" "ND")))

(setf IMUL-reg16.reg16.imm16 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg16,reg16,imm16"
:code-string "[rmi: o16 69 /r iw]"
:arch-flags (list "186")))

(setf IMUL-reg16.reg16.imm (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg16,reg16,imm"
:code-string "[rmi: o16 69 /r iw]"
:arch-flags (list "186" "SM" "ND")))

(setf IMUL-reg32.mem.imm8 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg32,mem,imm8"
:code-string "[rmi: o32 6b /r ib,s]"
:arch-flags (list "386" "SM")))

(setf IMUL-reg32.mem.sbytedword (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg32,mem,sbytedword"
:code-string "[rmi: o32 6b /r ib,s]"
:arch-flags (list "386" "SM" "ND")))

(setf IMUL-reg32.mem.imm32 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg32,mem,imm32"
:code-string "[rmi: o32 69 /r id]"
:arch-flags (list "386" "SM")))

(setf IMUL-reg32.mem.imm (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg32,mem,imm"
:code-string "[rmi: o32 69 /r id]"
:arch-flags (list "386" "SM" "ND")))

(setf IMUL-reg32.reg32.imm8 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg32,reg32,imm8"
:code-string "[rmi: o32 6b /r ib,s]"
:arch-flags (list "386")))

(setf IMUL-reg32.reg32.sbytedword (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg32,reg32,sbytedword"
:code-string "[rmi: o32 6b /r ib,s]"
:arch-flags (list "386" "SM" "ND")))

(setf IMUL-reg32.reg32.imm32 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg32,reg32,imm32"
:code-string "[rmi: o32 69 /r id]"
:arch-flags (list "386")))

(setf IMUL-reg32.reg32.imm (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg32,reg32,imm"
:code-string "[rmi: o32 69 /r id]"
:arch-flags (list "386" "SM" "ND")))

(setf IMUL-reg64.mem.imm8 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg64,mem,imm8"
:code-string "[rmi: o64 6b /r ib,s]"
:arch-flags (list "X64" "SM")))

(setf IMUL-reg64.mem.sbytedword (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg64,mem,sbytedword"
:code-string "[rmi: o64 6b /r ib,s]"
:arch-flags (list "X64" "SM" "ND")))

(setf IMUL-reg64.mem.imm32 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg64,mem,imm32"
:code-string "[rmi: o64 69 /r id]"
:arch-flags (list "X64" "SM")))

(setf IMUL-reg64.mem.imm (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg64,mem,imm"
:code-string "[rmi: o64 69 /r id,s]"
:arch-flags (list "X64" "SM" "ND")))

(setf IMUL-reg64.reg64.imm8 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg64,reg64,imm8"
:code-string "[rmi: o64 6b /r ib,s]"
:arch-flags (list "X64")))

(setf IMUL-reg64.reg64.sbytedword (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg64,reg64,sbytedword"
:code-string "[rmi: o64 6b /r ib,s]"
:arch-flags (list "X64" "SM" "ND")))

(setf IMUL-reg64.reg64.imm32 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg64,reg64,imm32"
:code-string "[rmi: o64 69 /r id]"
:arch-flags (list "X64")))

(setf IMUL-reg64.reg64.imm (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg64,reg64,imm"
:code-string "[rmi: o64 69 /r id,s]"
:arch-flags (list "X64" "SM" "ND")))

(setf IMUL-reg16.imm8 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg16,imm8"
:code-string "[r+mi: o16 6b /r ib,s]"
:arch-flags (list "186")))

(setf IMUL-reg16.sbyteword (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg16,sbyteword"
:code-string "[r+mi: o16 6b /r ib,s]"
:arch-flags (list "186" "SM" "ND")))

(setf IMUL-reg16.imm16 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg16,imm16"
:code-string "[r+mi: o16 69 /r iw]"
:arch-flags (list "186")))

(setf IMUL-reg16.imm (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg16,imm"
:code-string "[r+mi: o16 69 /r iw]"
:arch-flags (list "186" "SM" "ND")))

(setf IMUL-reg32.imm8 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg32,imm8"
:code-string "[r+mi: o32 6b /r ib,s]"
:arch-flags (list "386")))

(setf IMUL-reg32.sbytedword (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg32,sbytedword"
:code-string "[r+mi: o32 6b /r ib,s]"
:arch-flags (list "386" "SM" "ND")))

(setf IMUL-reg32.imm32 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg32,imm32"
:code-string "[r+mi: o32 69 /r id]"
:arch-flags (list "386")))

(setf IMUL-reg32.imm (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg32,imm"
:code-string "[r+mi: o32 69 /r id]"
:arch-flags (list "386" "SM" "ND")))

(setf IMUL-reg64.imm8 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg64,imm8"
:code-string "[r+mi: o64 6b /r ib,s]"
:arch-flags (list "X64")))

(setf IMUL-reg64.sbytedword (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg64,sbytedword"
:code-string "[r+mi: o64 6b /r ib,s]"
:arch-flags (list "X64" "SM" "ND")))

(setf IMUL-reg64.imm32 (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg64,imm32"
:code-string "[r+mi: o64 69 /r id,s]"
:arch-flags (list "X64")))

(setf IMUL-reg64.imm (make-instance 'x86-asm-instruction
:name "IMUL"
:operands "reg64,imm"
:code-string "[r+mi: o64 69 /r id,s]"
:arch-flags (list "X64" "SM" "ND")))

(setf IN-reg_al.imm (make-instance 'x86-asm-instruction
:name "IN"
:operands "reg_al,imm"
:code-string "[-i: e4 ib,u]"
:arch-flags (list "8086" "SB")))

(setf IN-reg_ax.imm (make-instance 'x86-asm-instruction
:name "IN"
:operands "reg_ax,imm"
:code-string "[-i: o16 e5 ib,u]"
:arch-flags (list "8086" "SB")))

(setf IN-reg_eax.imm (make-instance 'x86-asm-instruction
:name "IN"
:operands "reg_eax,imm"
:code-string "[-i: o32 e5 ib,u]"
:arch-flags (list "386" "SB")))

(setf IN-reg_al.reg_dx (make-instance 'x86-asm-instruction
:name "IN"
:operands "reg_al,reg_dx"
:code-string "[--: ec]"
:arch-flags (list "8086")))

(setf IN-reg_ax.reg_dx (make-instance 'x86-asm-instruction
:name "IN"
:operands "reg_ax,reg_dx"
:code-string "[--: o16 ed]"
:arch-flags (list "8086")))

(setf IN-reg_eax.reg_dx (make-instance 'x86-asm-instruction
:name "IN"
:operands "reg_eax,reg_dx"
:code-string "[--: o32 ed]"
:arch-flags (list "386")))

(setf INC-rm8 (make-instance 'x86-asm-instruction
:name "INC"
:operands "rm8"
:code-string "[m: hle fe /0]"
:arch-flags (list "8086" "LOCK")))

(setf INC-rm16 (make-instance 'x86-asm-instruction
:name "INC"
:operands "rm16"
:code-string "[m: hle o16 ff /0]"
:arch-flags (list "8086" "LOCK")))

(setf INC-rm32 (make-instance 'x86-asm-instruction
:name "INC"
:operands "rm32"
:code-string "[m: hle o32 ff /0]"
:arch-flags (list "386" "LOCK")))

(setf INC-rm64 (make-instance 'x86-asm-instruction
:name "INC"
:operands "rm64"
:code-string "[m: hle o64 ff /0]"
:arch-flags (list "X64" "LOCK")))

(setf INCBIN-ignore (make-instance 'x86-asm-instruction
:name "INCBIN"
:operands "ignore"
:code-string "ignore"
:arch-flags (list "ignore")))

(setf INSB-void (make-instance 'x86-asm-instruction
:name "INSB"
:operands "void"
:code-string "[ 6c]"
:arch-flags (list "186")))

(setf INSD-void (make-instance 'x86-asm-instruction
:name "INSD"
:operands "void"
:code-string "[ o32 6d]"
:arch-flags (list "386")))

(setf INSW-void (make-instance 'x86-asm-instruction
:name "INSW"
:operands "void"
:code-string "[ o16 6d]"
:arch-flags (list "186")))

(setf INT-imm (make-instance 'x86-asm-instruction
:name "INT"
:operands "imm"
:code-string "[i: cd ib,u]"
:arch-flags (list "8086" "SB")))

(setf INT01-void (make-instance 'x86-asm-instruction
:name "INT01"
:operands "void"
:code-string "[ f1]"
:arch-flags (list "386" "ND")))

(setf INT1-void (make-instance 'x86-asm-instruction
:name "INT1"
:operands "void"
:code-string "[ f1]"
:arch-flags (list "386")))

(setf INT03-void (make-instance 'x86-asm-instruction
:name "INT03"
:operands "void"
:code-string "[ cc]"
:arch-flags (list "8086" "ND")))

(setf INT3-void (make-instance 'x86-asm-instruction
:name "INT3"
:operands "void"
:code-string "[ cc]"
:arch-flags (list "8086")))

(setf INVD-void (make-instance 'x86-asm-instruction
:name "INVD"
:operands "void"
:code-string "[ 0f 08]"
:arch-flags (list "486" "PRIV")))

(setf INVPCID-reg64.mem128 (make-instance 'x86-asm-instruction
:name "INVPCID"
:operands "reg64,mem128"
:code-string "[rm: 66 0f 38 82 /r]"
:arch-flags (list "FUTURE" "INVPCID" "PRIV" "LONG")))

(setf INVLPG-mem (make-instance 'x86-asm-instruction
:name "INVLPG"
:operands "mem"
:code-string "[m: 0f 01 /7]"
:arch-flags (list "486" "PRIV")))

(setf INVLPGA-reg_eax.reg_ecx (make-instance 'x86-asm-instruction
:name "INVLPGA"
:operands "reg_eax,reg_ecx"
:code-string "[--: a32 0f 01 df]"
:arch-flags (list "X86_64" "AMD")))

(setf INVLPGA-reg_rax.reg_ecx (make-instance 'x86-asm-instruction
:name "INVLPGA"
:operands "reg_rax,reg_ecx"
:code-string "[--: o64nw a64 0f 01 df]"
:arch-flags (list "X64" "AMD")))

(setf INVLPGA-void (make-instance 'x86-asm-instruction
:name "INVLPGA"
:operands "void"
:code-string "[ 0f 01 df]"
:arch-flags (list "X86_64" "AMD")))

(setf IRET-void (make-instance 'x86-asm-instruction
:name "IRET"
:operands "void"
:code-string "[ odf cf]"
:arch-flags (list "8086")))

(setf IRETD-void (make-instance 'x86-asm-instruction
:name "IRETD"
:operands "void"
:code-string "[ o32 cf]"
:arch-flags (list "386")))

(setf IRETQ-void (make-instance 'x86-asm-instruction
:name "IRETQ"
:operands "void"
:code-string "[ o64 cf]"
:arch-flags (list "X64")))

(setf IRETW-void (make-instance 'x86-asm-instruction
:name "IRETW"
:operands "void"
:code-string "[ o16 cf]"
:arch-flags (list "8086")))

(setf JECXZ-imm (make-instance 'x86-asm-instruction
:name "JECXZ"
:operands "imm"
:code-string "[i: a32 e3 rel8]"
:arch-flags (list "386")))

(setf JRCXZ-imm (make-instance 'x86-asm-instruction
:name "JRCXZ"
:operands "imm"
:code-string "[i: a64 e3 rel8]"
:arch-flags (list "X64")))

(setf JMP-imm-short (make-instance 'x86-asm-instruction
:name "JMP"
:operands "imm|short"
:code-string "[i: eb rel8]"
:arch-flags (list "8086")))

(setf JMP-imm (make-instance 'x86-asm-instruction
:name "JMP"
:operands "imm"
:code-string "[i: jmp8 eb rel8]"
:arch-flags (list "8086" "ND")))

(setf JMP-imm (make-instance 'x86-asm-instruction
:name "JMP"
:operands "imm"
:code-string "[i: odf e9 rel]"
:arch-flags (list "8086" "BND")))

(setf JMP-imm-near (make-instance 'x86-asm-instruction
:name "JMP"
:operands "imm|near"
:code-string "[i: odf e9 rel]"
:arch-flags (list "8086" "ND" "BND")))

(setf JMP-imm64 (make-instance 'x86-asm-instruction
:name "JMP"
:operands "imm64"
:code-string "[i: o64nw e9 rel]"
:arch-flags (list "X64" "BND")))

(setf JMP-imm64-near (make-instance 'x86-asm-instruction
:name "JMP"
:operands "imm64|near"
:code-string "[i: o64nw e9 rel]"
:arch-flags (list "X64" "ND" "BND")))

(setf JMP-mem-far (make-instance 'x86-asm-instruction
:name "JMP"
:operands "mem|far"
:code-string "[m: o64 ff /5]"
:arch-flags (list "X64")))

(setf JMP-mem16-far (make-instance 'x86-asm-instruction
:name "JMP"
:operands "mem16|far"
:code-string "[m: o16 ff /5]"
:arch-flags (list "8086")))

(setf JMP-mem32-far (make-instance 'x86-asm-instruction
:name "JMP"
:operands "mem32|far"
:code-string "[m: o32 ff /5]"
:arch-flags (list "386")))

(setf JMP-mem64-far (make-instance 'x86-asm-instruction
:name "JMP"
:operands "mem64|far"
:code-string "[m: o64 ff /5]"
:arch-flags (list "X64")))

(setf JMP-mem-near (make-instance 'x86-asm-instruction
:name "JMP"
:operands "mem|near"
:code-string "[m: odf ff /4]"
:arch-flags (list "8086" "ND" "BND")))

(setf JMP-rm64-near (make-instance 'x86-asm-instruction
:name "JMP"
:operands "rm64|near"
:code-string "[m: o64nw ff /4]"
:arch-flags (list "X64" "ND" "BND")))

(setf JMP-mem (make-instance 'x86-asm-instruction
:name "JMP"
:operands "mem"
:code-string "[m: odf ff /4]"
:arch-flags (list "8086" "BND")))

(setf JMP-rm64 (make-instance 'x86-asm-instruction
:name "JMP"
:operands "rm64"
:code-string "[m: o64nw ff /4]"
:arch-flags (list "X64" "BND")))

(setf JMPE-imm (make-instance 'x86-asm-instruction
:name "JMPE"
:operands "imm"
:code-string "[i: odf 0f b8 rel]"
:arch-flags (list "IA64")))

(setf JMPE-imm16 (make-instance 'x86-asm-instruction
:name "JMPE"
:operands "imm16"
:code-string "[i: o16 0f b8 rel]"
:arch-flags (list "IA64")))

(setf JMPE-imm32 (make-instance 'x86-asm-instruction
:name "JMPE"
:operands "imm32"
:code-string "[i: o32 0f b8 rel]"
:arch-flags (list "IA64")))

(setf JMPE-rm16 (make-instance 'x86-asm-instruction
:name "JMPE"
:operands "rm16"
:code-string "[m: o16 0f 00 /6]"
:arch-flags (list "IA64")))

(setf JMPE-rm32 (make-instance 'x86-asm-instruction
:name "JMPE"
:operands "rm32"
:code-string "[m: o32 0f 00 /6]"
:arch-flags (list "IA64")))

(setf LAHF-void (make-instance 'x86-asm-instruction
:name "LAHF"
:operands "void"
:code-string "[ 9f]"
:arch-flags (list "8086")))

(setf LAR-reg16.mem (make-instance 'x86-asm-instruction
:name "LAR"
:operands "reg16,mem"
:code-string "[rm: o16 0f 02 /r]"
:arch-flags (list "286" "PROT" "SW")))

(setf LAR-reg16.reg16 (make-instance 'x86-asm-instruction
:name "LAR"
:operands "reg16,reg16"
:code-string "[rm: o16 0f 02 /r]"
:arch-flags (list "286" "PROT")))

(setf LAR-reg16.reg32 (make-instance 'x86-asm-instruction
:name "LAR"
:operands "reg16,reg32"
:code-string "[rm: o16 0f 02 /r]"
:arch-flags (list "386" "PROT")))

(setf LAR-reg16.reg64 (make-instance 'x86-asm-instruction
:name "LAR"
:operands "reg16,reg64"
:code-string "[rm: o16 o64nw 0f 02 /r]"
:arch-flags (list "X64" "PROT" "ND")))

(setf LAR-reg32.mem (make-instance 'x86-asm-instruction
:name "LAR"
:operands "reg32,mem"
:code-string "[rm: o32 0f 02 /r]"
:arch-flags (list "386" "PROT" "SW")))

(setf LAR-reg32.reg16 (make-instance 'x86-asm-instruction
:name "LAR"
:operands "reg32,reg16"
:code-string "[rm: o32 0f 02 /r]"
:arch-flags (list "386" "PROT")))

(setf LAR-reg32.reg32 (make-instance 'x86-asm-instruction
:name "LAR"
:operands "reg32,reg32"
:code-string "[rm: o32 0f 02 /r]"
:arch-flags (list "386" "PROT")))

(setf LAR-reg32.reg64 (make-instance 'x86-asm-instruction
:name "LAR"
:operands "reg32,reg64"
:code-string "[rm: o32 o64nw 0f 02 /r]"
:arch-flags (list "X64" "PROT" "ND")))

(setf LAR-reg64.mem (make-instance 'x86-asm-instruction
:name "LAR"
:operands "reg64,mem"
:code-string "[rm: o64 0f 02 /r]"
:arch-flags (list "X64" "PROT" "SW")))

(setf LAR-reg64.reg16 (make-instance 'x86-asm-instruction
:name "LAR"
:operands "reg64,reg16"
:code-string "[rm: o64 0f 02 /r]"
:arch-flags (list "X64" "PROT")))

(setf LAR-reg64.reg32 (make-instance 'x86-asm-instruction
:name "LAR"
:operands "reg64,reg32"
:code-string "[rm: o64 0f 02 /r]"
:arch-flags (list "X64" "PROT")))

(setf LAR-reg64.reg64 (make-instance 'x86-asm-instruction
:name "LAR"
:operands "reg64,reg64"
:code-string "[rm: o64 0f 02 /r]"
:arch-flags (list "X64" "PROT")))

(setf LEA-reg16.mem (make-instance 'x86-asm-instruction
:name "LEA"
:operands "reg16,mem"
:code-string "[rm: o16 8d /r]"
:arch-flags (list "8086")))

(setf LEA-reg32.mem (make-instance 'x86-asm-instruction
:name "LEA"
:operands "reg32,mem"
:code-string "[rm: o32 8d /r]"
:arch-flags (list "386")))

(setf LEA-reg64.mem (make-instance 'x86-asm-instruction
:name "LEA"
:operands "reg64,mem"
:code-string "[rm: o64 8d /r]"
:arch-flags (list "X64")))

(setf LEAVE-void (make-instance 'x86-asm-instruction
:name "LEAVE"
:operands "void"
:code-string "[ c9]"
:arch-flags (list "186")))

(setf LFENCE-void (make-instance 'x86-asm-instruction
:name "LFENCE"
:operands "void"
:code-string "[ np 0f ae e8]"
:arch-flags (list "X64" "AMD")))

(setf LFS-reg16.mem (make-instance 'x86-asm-instruction
:name "LFS"
:operands "reg16,mem"
:code-string "[rm: o16 0f b4 /r]"
:arch-flags (list "386")))

(setf LFS-reg32.mem (make-instance 'x86-asm-instruction
:name "LFS"
:operands "reg32,mem"
:code-string "[rm: o32 0f b4 /r]"
:arch-flags (list "386")))

(setf LFS-reg64.mem (make-instance 'x86-asm-instruction
:name "LFS"
:operands "reg64,mem"
:code-string "[rm: o64 0f b4 /r]"
:arch-flags (list "X64")))

(setf LGDT-mem (make-instance 'x86-asm-instruction
:name "LGDT"
:operands "mem"
:code-string "[m: 0f 01 /2]"
:arch-flags (list "286" "PRIV")))

(setf LGS-reg16.mem (make-instance 'x86-asm-instruction
:name "LGS"
:operands "reg16,mem"
:code-string "[rm: o16 0f b5 /r]"
:arch-flags (list "386")))

(setf LGS-reg32.mem (make-instance 'x86-asm-instruction
:name "LGS"
:operands "reg32,mem"
:code-string "[rm: o32 0f b5 /r]"
:arch-flags (list "386")))

(setf LGS-reg64.mem (make-instance 'x86-asm-instruction
:name "LGS"
:operands "reg64,mem"
:code-string "[rm: o64 0f b5 /r]"
:arch-flags (list "X64")))

(setf LIDT-mem (make-instance 'x86-asm-instruction
:name "LIDT"
:operands "mem"
:code-string "[m: 0f 01 /3]"
:arch-flags (list "286" "PRIV")))

(setf LLDT-mem (make-instance 'x86-asm-instruction
:name "LLDT"
:operands "mem"
:code-string "[m: 0f 00 /2]"
:arch-flags (list "286" "PROT" "PRIV")))

(setf LLDT-mem16 (make-instance 'x86-asm-instruction
:name "LLDT"
:operands "mem16"
:code-string "[m: 0f 00 /2]"
:arch-flags (list "286" "PROT" "PRIV")))

(setf LLDT-reg16 (make-instance 'x86-asm-instruction
:name "LLDT"
:operands "reg16"
:code-string "[m: 0f 00 /2]"
:arch-flags (list "286" "PROT" "PRIV")))

(setf LMSW-mem (make-instance 'x86-asm-instruction
:name "LMSW"
:operands "mem"
:code-string "[m: 0f 01 /6]"
:arch-flags (list "286" "PRIV")))

(setf LMSW-mem16 (make-instance 'x86-asm-instruction
:name "LMSW"
:operands "mem16"
:code-string "[m: 0f 01 /6]"
:arch-flags (list "286" "PRIV")))

(setf LMSW-reg16 (make-instance 'x86-asm-instruction
:name "LMSW"
:operands "reg16"
:code-string "[m: 0f 01 /6]"
:arch-flags (list "286" "PRIV")))

(setf LOADALL-void (make-instance 'x86-asm-instruction
:name "LOADALL"
:operands "void"
:code-string "[ 0f 07]"
:arch-flags (list "386" "UNDOC" "ND")))

(setf LOADALL286-void (make-instance 'x86-asm-instruction
:name "LOADALL286"
:operands "void"
:code-string "[ 0f 05]"
:arch-flags (list "286" "UNDOC" "ND")))

(setf LODSB-void (make-instance 'x86-asm-instruction
:name "LODSB"
:operands "void"
:code-string "[ ac]"
:arch-flags (list "8086")))

(setf LODSD-void (make-instance 'x86-asm-instruction
:name "LODSD"
:operands "void"
:code-string "[ o32 ad]"
:arch-flags (list "386")))

(setf LODSQ-void (make-instance 'x86-asm-instruction
:name "LODSQ"
:operands "void"
:code-string "[ o64 ad]"
:arch-flags (list "X64")))

(setf LODSW-void (make-instance 'x86-asm-instruction
:name "LODSW"
:operands "void"
:code-string "[ o16 ad]"
:arch-flags (list "8086")))

(setf LOOP-imm (make-instance 'x86-asm-instruction
:name "LOOP"
:operands "imm"
:code-string "[i: adf e2 rel8]"
:arch-flags (list "8086")))

(setf LOOP-imm.reg_ecx (make-instance 'x86-asm-instruction
:name "LOOP"
:operands "imm,reg_ecx"
:code-string "[i-: a32 e2 rel8]"
:arch-flags (list "386")))

(setf LOOP-imm.reg_rcx (make-instance 'x86-asm-instruction
:name "LOOP"
:operands "imm,reg_rcx"
:code-string "[i-: a64 e2 rel8]"
:arch-flags (list "X64")))

(setf LOOPE-imm (make-instance 'x86-asm-instruction
:name "LOOPE"
:operands "imm"
:code-string "[i: adf e1 rel8]"
:arch-flags (list "8086")))

(setf LOOPE-imm.reg_ecx (make-instance 'x86-asm-instruction
:name "LOOPE"
:operands "imm,reg_ecx"
:code-string "[i-: a32 e1 rel8]"
:arch-flags (list "386")))

(setf LOOPE-imm.reg_rcx (make-instance 'x86-asm-instruction
:name "LOOPE"
:operands "imm,reg_rcx"
:code-string "[i-: a64 e1 rel8]"
:arch-flags (list "X64")))

(setf LOOPNE-imm (make-instance 'x86-asm-instruction
:name "LOOPNE"
:operands "imm"
:code-string "[i: adf e0 rel8]"
:arch-flags (list "8086")))

(setf LOOPNE-imm.reg_ecx (make-instance 'x86-asm-instruction
:name "LOOPNE"
:operands "imm,reg_ecx"
:code-string "[i-: a32 e0 rel8]"
:arch-flags (list "386")))

(setf LOOPNE-imm.reg_rcx (make-instance 'x86-asm-instruction
:name "LOOPNE"
:operands "imm,reg_rcx"
:code-string "[i-: a64 e0 rel8]"
:arch-flags (list "X64")))

(setf LOOPNZ-imm (make-instance 'x86-asm-instruction
:name "LOOPNZ"
:operands "imm"
:code-string "[i: adf e0 rel8]"
:arch-flags (list "8086")))

(setf LOOPNZ-imm.reg_ecx (make-instance 'x86-asm-instruction
:name "LOOPNZ"
:operands "imm,reg_ecx"
:code-string "[i-: a32 e0 rel8]"
:arch-flags (list "386")))

(setf LOOPNZ-imm.reg_rcx (make-instance 'x86-asm-instruction
:name "LOOPNZ"
:operands "imm,reg_rcx"
:code-string "[i-: a64 e0 rel8]"
:arch-flags (list "X64")))

(setf LOOPZ-imm (make-instance 'x86-asm-instruction
:name "LOOPZ"
:operands "imm"
:code-string "[i: adf e1 rel8]"
:arch-flags (list "8086")))

(setf LOOPZ-imm.reg_ecx (make-instance 'x86-asm-instruction
:name "LOOPZ"
:operands "imm,reg_ecx"
:code-string "[i-: a32 e1 rel8]"
:arch-flags (list "386")))

(setf LOOPZ-imm.reg_rcx (make-instance 'x86-asm-instruction
:name "LOOPZ"
:operands "imm,reg_rcx"
:code-string "[i-: a64 e1 rel8]"
:arch-flags (list "X64")))

(setf LSL-reg16.mem (make-instance 'x86-asm-instruction
:name "LSL"
:operands "reg16,mem"
:code-string "[rm: o16 0f 03 /r]"
:arch-flags (list "286" "PROT" "SW")))

(setf LSL-reg16.reg16 (make-instance 'x86-asm-instruction
:name "LSL"
:operands "reg16,reg16"
:code-string "[rm: o16 0f 03 /r]"
:arch-flags (list "286" "PROT")))

(setf LSL-reg16.reg32 (make-instance 'x86-asm-instruction
:name "LSL"
:operands "reg16,reg32"
:code-string "[rm: o16 0f 03 /r]"
:arch-flags (list "386" "PROT")))

(setf LSL-reg16.reg64 (make-instance 'x86-asm-instruction
:name "LSL"
:operands "reg16,reg64"
:code-string "[rm: o16 o64nw 0f 03 /r]"
:arch-flags (list "X64" "PROT" "ND")))

(setf LSL-reg32.mem (make-instance 'x86-asm-instruction
:name "LSL"
:operands "reg32,mem"
:code-string "[rm: o32 0f 03 /r]"
:arch-flags (list "386" "PROT" "SW")))

(setf LSL-reg32.reg16 (make-instance 'x86-asm-instruction
:name "LSL"
:operands "reg32,reg16"
:code-string "[rm: o32 0f 03 /r]"
:arch-flags (list "386" "PROT")))

(setf LSL-reg32.reg32 (make-instance 'x86-asm-instruction
:name "LSL"
:operands "reg32,reg32"
:code-string "[rm: o32 0f 03 /r]"
:arch-flags (list "386" "PROT")))

(setf LSL-reg32.reg64 (make-instance 'x86-asm-instruction
:name "LSL"
:operands "reg32,reg64"
:code-string "[rm: o32 o64nw 0f 03 /r]"
:arch-flags (list "X64" "PROT" "ND")))

(setf LSL-reg64.mem (make-instance 'x86-asm-instruction
:name "LSL"
:operands "reg64,mem"
:code-string "[rm: o64 0f 03 /r]"
:arch-flags (list "X64" "PROT" "SW")))

(setf LSL-reg64.reg16 (make-instance 'x86-asm-instruction
:name "LSL"
:operands "reg64,reg16"
:code-string "[rm: o64 0f 03 /r]"
:arch-flags (list "X64" "PROT")))

(setf LSL-reg64.reg32 (make-instance 'x86-asm-instruction
:name "LSL"
:operands "reg64,reg32"
:code-string "[rm: o64 0f 03 /r]"
:arch-flags (list "X64" "PROT")))

(setf LSL-reg64.reg64 (make-instance 'x86-asm-instruction
:name "LSL"
:operands "reg64,reg64"
:code-string "[rm: o64 0f 03 /r]"
:arch-flags (list "X64" "PROT")))

(setf LSS-reg16.mem (make-instance 'x86-asm-instruction
:name "LSS"
:operands "reg16,mem"
:code-string "[rm: o16 0f b2 /r]"
:arch-flags (list "386")))

(setf LSS-reg32.mem (make-instance 'x86-asm-instruction
:name "LSS"
:operands "reg32,mem"
:code-string "[rm: o32 0f b2 /r]"
:arch-flags (list "386")))

(setf LSS-reg64.mem (make-instance 'x86-asm-instruction
:name "LSS"
:operands "reg64,mem"
:code-string "[rm: o64 0f b2 /r]"
:arch-flags (list "X64")))

(setf LTR-mem (make-instance 'x86-asm-instruction
:name "LTR"
:operands "mem"
:code-string "[m: 0f 00 /3]"
:arch-flags (list "286" "PROT" "PRIV")))

(setf LTR-mem16 (make-instance 'x86-asm-instruction
:name "LTR"
:operands "mem16"
:code-string "[m: 0f 00 /3]"
:arch-flags (list "286" "PROT" "PRIV")))

(setf LTR-reg16 (make-instance 'x86-asm-instruction
:name "LTR"
:operands "reg16"
:code-string "[m: 0f 00 /3]"
:arch-flags (list "286" "PROT" "PRIV")))

(setf MFENCE-void (make-instance 'x86-asm-instruction
:name "MFENCE"
:operands "void"
:code-string "[ np 0f ae f0]"
:arch-flags (list "X64" "AMD")))

(setf MONITOR-void (make-instance 'x86-asm-instruction
:name "MONITOR"
:operands "void"
:code-string "[ 0f 01 c8]"
:arch-flags (list "PRESCOTT")))

(setf MONITOR-reg_rax.reg_ecx.reg_edx (make-instance 'x86-asm-instruction
:name "MONITOR"
:operands "reg_rax,reg_ecx,reg_edx"
:code-string "[---: 0f 01 c8]"
:arch-flags (list "X64" "ND")))

(setf MOV-mem.reg_sreg (make-instance 'x86-asm-instruction
:name "MOV"
:operands "mem,reg_sreg"
:code-string "[mr: 8c /r]"
:arch-flags (list "8086" "SW")))

(setf MOV-reg16.reg_sreg (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg16,reg_sreg"
:code-string "[mr: o16 8c /r]"
:arch-flags (list "8086")))

(setf MOV-reg32.reg_sreg (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg32,reg_sreg"
:code-string "[mr: o32 8c /r]"
:arch-flags (list "386")))

(setf MOV-reg64.reg_sreg (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg64,reg_sreg"
:code-string "[mr: o64nw 8c /r]"
:arch-flags (list "X64" "OPT" "ND")))

(setf MOV-rm64.reg_sreg (make-instance 'x86-asm-instruction
:name "MOV"
:operands "rm64,reg_sreg"
:code-string "[mr: o64 8c /r]"
:arch-flags (list "X64")))

(setf MOV-reg_sreg.mem (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg_sreg,mem"
:code-string "[rm: 8e /r]"
:arch-flags (list "8086" "SW")))

(setf MOV-reg_sreg.reg16 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg_sreg,reg16"
:code-string "[rm: 8e /r]"
:arch-flags (list "8086" "OPT" "ND")))

(setf MOV-reg_sreg.reg32 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg_sreg,reg32"
:code-string "[rm: 8e /r]"
:arch-flags (list "386" "OPT" "ND")))

(setf MOV-reg_sreg.reg64 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg_sreg,reg64"
:code-string "[rm: o64nw 8e /r]"
:arch-flags (list "X64" "OPT" "ND")))

(setf MOV-reg_sreg.reg16 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg_sreg,reg16"
:code-string "[rm: o16 8e /r]"
:arch-flags (list "8086")))

(setf MOV-reg_sreg.reg32 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg_sreg,reg32"
:code-string "[rm: o32 8e /r]"
:arch-flags (list "386")))

(setf MOV-reg_sreg.rm64 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg_sreg,rm64"
:code-string "[rm: o64 8e /r]"
:arch-flags (list "X64")))

(setf MOV-reg_al.mem_offs (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg_al,mem_offs"
:code-string "[-i: a0 iwdq]"
:arch-flags (list "8086" "SM")))

(setf MOV-reg_ax.mem_offs (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg_ax,mem_offs"
:code-string "[-i: o16 a1 iwdq]"
:arch-flags (list "8086" "SM")))

(setf MOV-reg_eax.mem_offs (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg_eax,mem_offs"
:code-string "[-i: o32 a1 iwdq]"
:arch-flags (list "386" "SM")))

(setf MOV-reg_rax.mem_offs (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg_rax,mem_offs"
:code-string "[-i: o64 a1 iwdq]"
:arch-flags (list "X64" "SM")))

(setf MOV-mem_offs.reg_al (make-instance 'x86-asm-instruction
:name "MOV"
:operands "mem_offs,reg_al"
:code-string "[i-: a2 iwdq]"
:arch-flags (list "8086" "SM" "NOHLE")))

(setf MOV-mem_offs.reg_ax (make-instance 'x86-asm-instruction
:name "MOV"
:operands "mem_offs,reg_ax"
:code-string "[i-: o16 a3 iwdq]"
:arch-flags (list "8086" "SM" "NOHLE")))

(setf MOV-mem_offs.reg_eax (make-instance 'x86-asm-instruction
:name "MOV"
:operands "mem_offs,reg_eax"
:code-string "[i-: o32 a3 iwdq]"
:arch-flags (list "386" "SM" "NOHLE")))

(setf MOV-mem_offs.reg_rax (make-instance 'x86-asm-instruction
:name "MOV"
:operands "mem_offs,reg_rax"
:code-string "[i-: o64 a3 iwdq]"
:arch-flags (list "X64" "SM" "NOHLE")))

(setf MOV-reg64.reg_creg (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg64,reg_creg"
:code-string "[mr: o64nw 0f 20 /r]"
:arch-flags (list "X64" "PRIV")))

(setf MOV-reg_creg.reg64 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg_creg,reg64"
:code-string "[rm: o64nw 0f 22 /r]"
:arch-flags (list "X64" "PRIV")))

(setf MOV-reg64.reg_dreg (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg64,reg_dreg"
:code-string "[mr: o64nw 0f 21 /r]"
:arch-flags (list "X64" "PRIV")))

(setf MOV-reg_dreg.reg64 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg_dreg,reg64"
:code-string "[rm: o64nw 0f 23 /r]"
:arch-flags (list "X64" "PRIV")))

(setf MOV-mem.reg8 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "mem,reg8"
:code-string "[mr: hlexr 88 /r]"
:arch-flags (list "8086" "SM")))

(setf MOV-reg8.reg8 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg8,reg8"
:code-string "[mr: 88 /r]"
:arch-flags (list "8086")))

(setf MOV-mem.reg16 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "mem,reg16"
:code-string "[mr: hlexr o16 89 /r]"
:arch-flags (list "8086" "SM")))

(setf MOV-reg16.reg16 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg16,reg16"
:code-string "[mr: o16 89 /r]"
:arch-flags (list "8086")))

(setf MOV-mem.reg32 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "mem,reg32"
:code-string "[mr: hlexr o32 89 /r]"
:arch-flags (list "386" "SM")))

(setf MOV-reg32.reg32 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg32,reg32"
:code-string "[mr: o32 89 /r]"
:arch-flags (list "386")))

(setf MOV-mem.reg64 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "mem,reg64"
:code-string "[mr: hlexr o64 89 /r]"
:arch-flags (list "X64" "SM")))

(setf MOV-reg64.reg64 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg64,reg64"
:code-string "[mr: o64 89 /r]"
:arch-flags (list "X64")))

(setf MOV-reg8.mem (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg8,mem"
:code-string "[rm: 8a /r]"
:arch-flags (list "8086" "SM")))

(setf MOV-reg8.reg8 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg8,reg8"
:code-string "[rm: 8a /r]"
:arch-flags (list "8086")))

(setf MOV-reg16.mem (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg16,mem"
:code-string "[rm: o16 8b /r]"
:arch-flags (list "8086" "SM")))

(setf MOV-reg16.reg16 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg16,reg16"
:code-string "[rm: o16 8b /r]"
:arch-flags (list "8086")))

(setf MOV-reg32.mem (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg32,mem"
:code-string "[rm: o32 8b /r]"
:arch-flags (list "386" "SM")))

(setf MOV-reg32.reg32 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg32,reg32"
:code-string "[rm: o32 8b /r]"
:arch-flags (list "386")))

(setf MOV-reg64.mem (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg64,mem"
:code-string "[rm: o64 8b /r]"
:arch-flags (list "X64" "SM")))

(setf MOV-reg64.reg64 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg64,reg64"
:code-string "[rm: o64 8b /r]"
:arch-flags (list "X64")))

(setf MOV-reg8.imm (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg8,imm"
:code-string "[ri: b0+r ib]"
:arch-flags (list "8086" "SM")))

(setf MOV-reg16.imm (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg16,imm"
:code-string "[ri: o16 b8+r iw]"
:arch-flags (list "8086" "SM")))

(setf MOV-reg32.imm (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg32,imm"
:code-string "[ri: o32 b8+r id]"
:arch-flags (list "386" "SM")))

(setf MOV-reg64.udword (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg64,udword"
:code-string "[ri: o64nw b8+r id]"
:arch-flags (list "X64" "SM" "OPT" "ND")))

(setf MOV-reg64.sdword (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg64,sdword"
:code-string "[mi: o64 c7 /0 id,s]"
:arch-flags (list "X64" "SM" "OPT" "ND")))

(setf MOV-reg64.imm (make-instance 'x86-asm-instruction
:name "MOV"
:operands "reg64,imm"
:code-string "[ri: o64 b8+r iq]"
:arch-flags (list "X64" "SM")))

(setf MOV-rm8.imm (make-instance 'x86-asm-instruction
:name "MOV"
:operands "rm8,imm"
:code-string "[mi: hlexr c6 /0 ib]"
:arch-flags (list "8086" "SM")))

(setf MOV-rm16.imm (make-instance 'x86-asm-instruction
:name "MOV"
:operands "rm16,imm"
:code-string "[mi: hlexr o16 c7 /0 iw]"
:arch-flags (list "8086" "SM")))

(setf MOV-rm32.imm (make-instance 'x86-asm-instruction
:name "MOV"
:operands "rm32,imm"
:code-string "[mi: hlexr o32 c7 /0 id]"
:arch-flags (list "386" "SM")))

(setf MOV-rm64.imm (make-instance 'x86-asm-instruction
:name "MOV"
:operands "rm64,imm"
:code-string "[mi: hlexr o64 c7 /0 id,s]"
:arch-flags (list "X64" "SM")))

(setf MOV-rm64.imm32 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "rm64,imm32"
:code-string "[mi: hlexr o64 c7 /0 id,s]"
:arch-flags (list "X64")))

(setf MOV-mem.imm8 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "mem,imm8"
:code-string "[mi: hlexr c6 /0 ib]"
:arch-flags (list "8086" "SM")))

(setf MOV-mem.imm16 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "mem,imm16"
:code-string "[mi: hlexr o16 c7 /0 iw]"
:arch-flags (list "8086" "SM")))

(setf MOV-mem.imm32 (make-instance 'x86-asm-instruction
:name "MOV"
:operands "mem,imm32"
:code-string "[mi: hlexr o32 c7 /0 id]"
:arch-flags (list "386" "SM")))

(setf MOVD-mmxreg.rm32 (make-instance 'x86-asm-instruction
:name "MOVD"
:operands "mmxreg,rm32"
:code-string "[rm: np 0f 6e /r]"
:arch-flags (list "PENT" "MMX" "SD")))

(setf MOVD-rm32.mmxreg (make-instance 'x86-asm-instruction
:name "MOVD"
:operands "rm32,mmxreg"
:code-string "[mr: np 0f 7e /r]"
:arch-flags (list "PENT" "MMX" "SD")))

(setf MOVD-mmxreg.rm64 (make-instance 'x86-asm-instruction
:name "MOVD"
:operands "mmxreg,rm64"
:code-string "[rm: np o64 0f 6e /r]"
:arch-flags (list "X64" "MMX" "SX" "ND")))

(setf MOVD-rm64.mmxreg (make-instance 'x86-asm-instruction
:name "MOVD"
:operands "rm64,mmxreg"
:code-string "[mr: np o64 0f 7e /r]"
:arch-flags (list "X64" "MMX" "SX" "ND")))

(setf MOVQ-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "MOVQ"
:operands "mmxreg,mmxrm"
:code-string "[rm: np 0f 6f /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf MOVQ-mmxrm.mmxreg (make-instance 'x86-asm-instruction
:name "MOVQ"
:operands "mmxrm,mmxreg"
:code-string "[mr: np 0f 7f /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf MOVQ-mmxreg.rm64 (make-instance 'x86-asm-instruction
:name "MOVQ"
:operands "mmxreg,rm64"
:code-string "[rm: np o64 0f 6e /r]"
:arch-flags (list "X64" "MMX")))

(setf MOVQ-rm64.mmxreg (make-instance 'x86-asm-instruction
:name "MOVQ"
:operands "rm64,mmxreg"
:code-string "[mr: np o64 0f 7e /r]"
:arch-flags (list "X64" "MMX")))

(setf MOVSB-void (make-instance 'x86-asm-instruction
:name "MOVSB"
:operands "void"
:code-string "[ a4]"
:arch-flags (list "8086")))

(setf MOVSD-void (make-instance 'x86-asm-instruction
:name "MOVSD"
:operands "void"
:code-string "[ o32 a5]"
:arch-flags (list "386")))

(setf MOVSQ-void (make-instance 'x86-asm-instruction
:name "MOVSQ"
:operands "void"
:code-string "[ o64 a5]"
:arch-flags (list "X64")))

(setf MOVSW-void (make-instance 'x86-asm-instruction
:name "MOVSW"
:operands "void"
:code-string "[ o16 a5]"
:arch-flags (list "8086")))

(setf MOVSX-reg16.mem (make-instance 'x86-asm-instruction
:name "MOVSX"
:operands "reg16,mem"
:code-string "[rm: o16 0f be /r]"
:arch-flags (list "386" "SB")))

(setf MOVSX-reg16.reg8 (make-instance 'x86-asm-instruction
:name "MOVSX"
:operands "reg16,reg8"
:code-string "[rm: o16 0f be /r]"
:arch-flags (list "386")))

(setf MOVSX-reg32.rm8 (make-instance 'x86-asm-instruction
:name "MOVSX"
:operands "reg32,rm8"
:code-string "[rm: o32 0f be /r]"
:arch-flags (list "386")))

(setf MOVSX-reg32.rm16 (make-instance 'x86-asm-instruction
:name "MOVSX"
:operands "reg32,rm16"
:code-string "[rm: o32 0f bf /r]"
:arch-flags (list "386")))

(setf MOVSX-reg64.rm8 (make-instance 'x86-asm-instruction
:name "MOVSX"
:operands "reg64,rm8"
:code-string "[rm: o64 0f be /r]"
:arch-flags (list "X64")))

(setf MOVSX-reg64.rm16 (make-instance 'x86-asm-instruction
:name "MOVSX"
:operands "reg64,rm16"
:code-string "[rm: o64 0f bf /r]"
:arch-flags (list "X64")))

(setf MOVSXD-reg64.rm32 (make-instance 'x86-asm-instruction
:name "MOVSXD"
:operands "reg64,rm32"
:code-string "[rm: o64 63 /r]"
:arch-flags (list "X64")))

(setf MOVSX-reg64.rm32 (make-instance 'x86-asm-instruction
:name "MOVSX"
:operands "reg64,rm32"
:code-string "[rm: o64 63 /r]"
:arch-flags (list "X64" "ND")))

(setf MOVZX-reg16.mem (make-instance 'x86-asm-instruction
:name "MOVZX"
:operands "reg16,mem"
:code-string "[rm: o16 0f b6 /r]"
:arch-flags (list "386" "SB")))

(setf MOVZX-reg16.reg8 (make-instance 'x86-asm-instruction
:name "MOVZX"
:operands "reg16,reg8"
:code-string "[rm: o16 0f b6 /r]"
:arch-flags (list "386")))

(setf MOVZX-reg32.rm8 (make-instance 'x86-asm-instruction
:name "MOVZX"
:operands "reg32,rm8"
:code-string "[rm: o32 0f b6 /r]"
:arch-flags (list "386")))

(setf MOVZX-reg32.rm16 (make-instance 'x86-asm-instruction
:name "MOVZX"
:operands "reg32,rm16"
:code-string "[rm: o32 0f b7 /r]"
:arch-flags (list "386")))

(setf MOVZX-reg64.rm8 (make-instance 'x86-asm-instruction
:name "MOVZX"
:operands "reg64,rm8"
:code-string "[rm: o64 0f b6 /r]"
:arch-flags (list "X64")))

(setf MOVZX-reg64.rm16 (make-instance 'x86-asm-instruction
:name "MOVZX"
:operands "reg64,rm16"
:code-string "[rm: o64 0f b7 /r]"
:arch-flags (list "X64")))

(setf MUL-rm8 (make-instance 'x86-asm-instruction
:name "MUL"
:operands "rm8"
:code-string "[m: f6 /4]"
:arch-flags (list "8086")))

(setf MUL-rm16 (make-instance 'x86-asm-instruction
:name "MUL"
:operands "rm16"
:code-string "[m: o16 f7 /4]"
:arch-flags (list "8086")))

(setf MUL-rm32 (make-instance 'x86-asm-instruction
:name "MUL"
:operands "rm32"
:code-string "[m: o32 f7 /4]"
:arch-flags (list "386")))

(setf MUL-rm64 (make-instance 'x86-asm-instruction
:name "MUL"
:operands "rm64"
:code-string "[m: o64 f7 /4]"
:arch-flags (list "X64")))

(setf MWAIT-void (make-instance 'x86-asm-instruction
:name "MWAIT"
:operands "void"
:code-string "[ 0f 01 c9]"
:arch-flags (list "PRESCOTT")))

(setf MWAIT-reg_eax.reg_ecx (make-instance 'x86-asm-instruction
:name "MWAIT"
:operands "reg_eax,reg_ecx"
:code-string "[--: 0f 01 c9]"
:arch-flags (list "PRESCOTT" "ND")))

(setf NEG-rm8 (make-instance 'x86-asm-instruction
:name "NEG"
:operands "rm8"
:code-string "[m: hle f6 /3]"
:arch-flags (list "8086" "LOCK")))

(setf NEG-rm16 (make-instance 'x86-asm-instruction
:name "NEG"
:operands "rm16"
:code-string "[m: hle o16 f7 /3]"
:arch-flags (list "8086" "LOCK")))

(setf NEG-rm32 (make-instance 'x86-asm-instruction
:name "NEG"
:operands "rm32"
:code-string "[m: hle o32 f7 /3]"
:arch-flags (list "386" "LOCK")))

(setf NEG-rm64 (make-instance 'x86-asm-instruction
:name "NEG"
:operands "rm64"
:code-string "[m: hle o64 f7 /3]"
:arch-flags (list "X64" "LOCK")))

(setf NOP-void (make-instance 'x86-asm-instruction
:name "NOP"
:operands "void"
:code-string "[ norexb nof3 90]"
:arch-flags (list "8086")))

(setf NOP-rm16 (make-instance 'x86-asm-instruction
:name "NOP"
:operands "rm16"
:code-string "[m: o16 0f 1f /0]"
:arch-flags (list "P6")))

(setf NOP-rm32 (make-instance 'x86-asm-instruction
:name "NOP"
:operands "rm32"
:code-string "[m: o32 0f 1f /0]"
:arch-flags (list "P6")))

(setf NOP-rm64 (make-instance 'x86-asm-instruction
:name "NOP"
:operands "rm64"
:code-string "[m: o64 0f 1f /0]"
:arch-flags (list "X64")))

(setf NOT-rm8 (make-instance 'x86-asm-instruction
:name "NOT"
:operands "rm8"
:code-string "[m: hle f6 /2]"
:arch-flags (list "8086" "LOCK")))

(setf NOT-rm16 (make-instance 'x86-asm-instruction
:name "NOT"
:operands "rm16"
:code-string "[m: hle o16 f7 /2]"
:arch-flags (list "8086" "LOCK")))

(setf NOT-rm32 (make-instance 'x86-asm-instruction
:name "NOT"
:operands "rm32"
:code-string "[m: hle o32 f7 /2]"
:arch-flags (list "386" "LOCK")))

(setf NOT-rm64 (make-instance 'x86-asm-instruction
:name "NOT"
:operands "rm64"
:code-string "[m: hle o64 f7 /2]"
:arch-flags (list "X64" "LOCK")))

(setf OR-mem.reg8 (make-instance 'x86-asm-instruction
:name "OR"
:operands "mem,reg8"
:code-string "[mr: hle 08 /r]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf OR-reg8.reg8 (make-instance 'x86-asm-instruction
:name "OR"
:operands "reg8,reg8"
:code-string "[mr: 08 /r]"
:arch-flags (list "8086")))

(setf OR-mem.reg16 (make-instance 'x86-asm-instruction
:name "OR"
:operands "mem,reg16"
:code-string "[mr: hle o16 09 /r]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf OR-reg16.reg16 (make-instance 'x86-asm-instruction
:name "OR"
:operands "reg16,reg16"
:code-string "[mr: o16 09 /r]"
:arch-flags (list "8086")))

(setf OR-mem.reg32 (make-instance 'x86-asm-instruction
:name "OR"
:operands "mem,reg32"
:code-string "[mr: hle o32 09 /r]"
:arch-flags (list "386" "SM" "LOCK")))

(setf OR-reg32.reg32 (make-instance 'x86-asm-instruction
:name "OR"
:operands "reg32,reg32"
:code-string "[mr: o32 09 /r]"
:arch-flags (list "386")))

(setf OR-mem.reg64 (make-instance 'x86-asm-instruction
:name "OR"
:operands "mem,reg64"
:code-string "[mr: hle o64 09 /r]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf OR-reg64.reg64 (make-instance 'x86-asm-instruction
:name "OR"
:operands "reg64,reg64"
:code-string "[mr: o64 09 /r]"
:arch-flags (list "X64")))

(setf OR-reg8.mem (make-instance 'x86-asm-instruction
:name "OR"
:operands "reg8,mem"
:code-string "[rm: 0a /r]"
:arch-flags (list "8086" "SM")))

(setf OR-reg8.reg8 (make-instance 'x86-asm-instruction
:name "OR"
:operands "reg8,reg8"
:code-string "[rm: 0a /r]"
:arch-flags (list "8086")))

(setf OR-reg16.mem (make-instance 'x86-asm-instruction
:name "OR"
:operands "reg16,mem"
:code-string "[rm: o16 0b /r]"
:arch-flags (list "8086" "SM")))

(setf OR-reg16.reg16 (make-instance 'x86-asm-instruction
:name "OR"
:operands "reg16,reg16"
:code-string "[rm: o16 0b /r]"
:arch-flags (list "8086")))

(setf OR-reg32.mem (make-instance 'x86-asm-instruction
:name "OR"
:operands "reg32,mem"
:code-string "[rm: o32 0b /r]"
:arch-flags (list "386" "SM")))

(setf OR-reg32.reg32 (make-instance 'x86-asm-instruction
:name "OR"
:operands "reg32,reg32"
:code-string "[rm: o32 0b /r]"
:arch-flags (list "386")))

(setf OR-reg64.mem (make-instance 'x86-asm-instruction
:name "OR"
:operands "reg64,mem"
:code-string "[rm: o64 0b /r]"
:arch-flags (list "X64" "SM")))

(setf OR-reg64.reg64 (make-instance 'x86-asm-instruction
:name "OR"
:operands "reg64,reg64"
:code-string "[rm: o64 0b /r]"
:arch-flags (list "X64")))

(setf OR-rm16.imm8 (make-instance 'x86-asm-instruction
:name "OR"
:operands "rm16,imm8"
:code-string "[mi: hle o16 83 /1 ib,s]"
:arch-flags (list "8086" "LOCK")))

(setf OR-rm32.imm8 (make-instance 'x86-asm-instruction
:name "OR"
:operands "rm32,imm8"
:code-string "[mi: hle o32 83 /1 ib,s]"
:arch-flags (list "386" "LOCK")))

(setf OR-rm64.imm8 (make-instance 'x86-asm-instruction
:name "OR"
:operands "rm64,imm8"
:code-string "[mi: hle o64 83 /1 ib,s]"
:arch-flags (list "X64" "LOCK")))

(setf OR-reg_al.imm (make-instance 'x86-asm-instruction
:name "OR"
:operands "reg_al,imm"
:code-string "[-i: 0c ib]"
:arch-flags (list "8086" "SM")))

(setf OR-reg_ax.sbyteword (make-instance 'x86-asm-instruction
:name "OR"
:operands "reg_ax,sbyteword"
:code-string "[mi: o16 83 /1 ib,s]"
:arch-flags (list "8086" "SM" "ND")))

(setf OR-reg_ax.imm (make-instance 'x86-asm-instruction
:name "OR"
:operands "reg_ax,imm"
:code-string "[-i: o16 0d iw]"
:arch-flags (list "8086" "SM")))

(setf OR-reg_eax.sbytedword (make-instance 'x86-asm-instruction
:name "OR"
:operands "reg_eax,sbytedword"
:code-string "[mi: o32 83 /1 ib,s]"
:arch-flags (list "386" "SM" "ND")))

(setf OR-reg_eax.imm (make-instance 'x86-asm-instruction
:name "OR"
:operands "reg_eax,imm"
:code-string "[-i: o32 0d id]"
:arch-flags (list "386" "SM")))

(setf OR-reg_rax.sbytedword (make-instance 'x86-asm-instruction
:name "OR"
:operands "reg_rax,sbytedword"
:code-string "[mi: o64 83 /1 ib,s]"
:arch-flags (list "X64" "SM" "ND")))

(setf OR-reg_rax.imm (make-instance 'x86-asm-instruction
:name "OR"
:operands "reg_rax,imm"
:code-string "[-i: o64 0d id,s]"
:arch-flags (list "X64" "SM")))

(setf OR-rm8.imm (make-instance 'x86-asm-instruction
:name "OR"
:operands "rm8,imm"
:code-string "[mi: hle 80 /1 ib]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf OR-rm16.sbyteword (make-instance 'x86-asm-instruction
:name "OR"
:operands "rm16,sbyteword"
:code-string "[mi: hle o16 83 /1 ib,s]"
:arch-flags (list "8086" "SM" "LOCK" "ND")))

(setf OR-rm16.imm (make-instance 'x86-asm-instruction
:name "OR"
:operands "rm16,imm"
:code-string "[mi: hle o16 81 /1 iw]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf OR-rm32.sbytedword (make-instance 'x86-asm-instruction
:name "OR"
:operands "rm32,sbytedword"
:code-string "[mi: hle o32 83 /1 ib,s]"
:arch-flags (list "386" "SM" "LOCK" "ND")))

(setf OR-rm32.imm (make-instance 'x86-asm-instruction
:name "OR"
:operands "rm32,imm"
:code-string "[mi: hle o32 81 /1 id]"
:arch-flags (list "386" "SM" "LOCK")))

(setf OR-rm64.sbytedword (make-instance 'x86-asm-instruction
:name "OR"
:operands "rm64,sbytedword"
:code-string "[mi: hle o64 83 /1 ib,s]"
:arch-flags (list "X64" "SM" "LOCK" "ND")))

(setf OR-rm64.imm (make-instance 'x86-asm-instruction
:name "OR"
:operands "rm64,imm"
:code-string "[mi: hle o64 81 /1 id,s]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf OR-mem.imm8 (make-instance 'x86-asm-instruction
:name "OR"
:operands "mem,imm8"
:code-string "[mi: hle 80 /1 ib]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf OR-mem.sbyteword16 (make-instance 'x86-asm-instruction
:name "OR"
:operands "mem,sbyteword16"
:code-string "[mi: hle o16 83 /1 ib,s]"
:arch-flags (list "8086" "SM" "LOCK" "ND")))

(setf OR-mem.imm16 (make-instance 'x86-asm-instruction
:name "OR"
:operands "mem,imm16"
:code-string "[mi: hle o16 81 /1 iw]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf OR-mem.sbytedword32 (make-instance 'x86-asm-instruction
:name "OR"
:operands "mem,sbytedword32"
:code-string "[mi: hle o32 83 /1 ib,s]"
:arch-flags (list "386" "SM" "LOCK" "ND")))

(setf OR-mem.imm32 (make-instance 'x86-asm-instruction
:name "OR"
:operands "mem,imm32"
:code-string "[mi: hle o32 81 /1 id]"
:arch-flags (list "386" "SM" "LOCK")))

(setf OUT-imm.reg_al (make-instance 'x86-asm-instruction
:name "OUT"
:operands "imm,reg_al"
:code-string "[i-: e6 ib,u]"
:arch-flags (list "8086" "SB")))

(setf OUT-imm.reg_ax (make-instance 'x86-asm-instruction
:name "OUT"
:operands "imm,reg_ax"
:code-string "[i-: o16 e7 ib,u]"
:arch-flags (list "8086" "SB")))

(setf OUT-imm.reg_eax (make-instance 'x86-asm-instruction
:name "OUT"
:operands "imm,reg_eax"
:code-string "[i-: o32 e7 ib,u]"
:arch-flags (list "386" "SB")))

(setf OUT-reg_dx.reg_al (make-instance 'x86-asm-instruction
:name "OUT"
:operands "reg_dx,reg_al"
:code-string "[--: ee]"
:arch-flags (list "8086")))

(setf OUT-reg_dx.reg_ax (make-instance 'x86-asm-instruction
:name "OUT"
:operands "reg_dx,reg_ax"
:code-string "[--: o16 ef]"
:arch-flags (list "8086")))

(setf OUT-reg_dx.reg_eax (make-instance 'x86-asm-instruction
:name "OUT"
:operands "reg_dx,reg_eax"
:code-string "[--: o32 ef]"
:arch-flags (list "386")))

(setf OUTSB-void (make-instance 'x86-asm-instruction
:name "OUTSB"
:operands "void"
:code-string "[ 6e]"
:arch-flags (list "186")))

(setf OUTSD-void (make-instance 'x86-asm-instruction
:name "OUTSD"
:operands "void"
:code-string "[ o32 6f]"
:arch-flags (list "386")))

(setf OUTSW-void (make-instance 'x86-asm-instruction
:name "OUTSW"
:operands "void"
:code-string "[ o16 6f]"
:arch-flags (list "186")))

(setf PACKSSDW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PACKSSDW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f 6b /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PACKSSWB-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PACKSSWB"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f 63 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PACKUSWB-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PACKUSWB"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f 67 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PADDB-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PADDB"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f fc /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PADDD-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PADDD"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f fe /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PADDSB-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PADDSB"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f ec /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PADDSIW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PADDSIW"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 51 /r]"
:arch-flags (list "PENT" "MMX" "SQ" "CYRIX")))

(setf PADDSW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PADDSW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f ed /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PADDUSB-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PADDUSB"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f dc /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PADDUSW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PADDUSW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f dd /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PADDW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PADDW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f fd /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PAND-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PAND"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f db /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PANDN-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PANDN"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f df /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PAUSE-void (make-instance 'x86-asm-instruction
:name "PAUSE"
:operands "void"
:code-string "[ f3i 90]"
:arch-flags (list "8086")))

(setf PAVEB-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PAVEB"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 50 /r]"
:arch-flags (list "PENT" "MMX" "SQ" "CYRIX")))

(setf PAVGUSB-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PAVGUSB"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r bf]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PCMPEQB-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PCMPEQB"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f 74 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PCMPEQD-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PCMPEQD"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f 76 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PCMPEQW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PCMPEQW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f 75 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PCMPGTB-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PCMPGTB"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f 64 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PCMPGTD-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PCMPGTD"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f 66 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PCMPGTW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PCMPGTW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f 65 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PDISTIB-mmxreg.mem (make-instance 'x86-asm-instruction
:name "PDISTIB"
:operands "mmxreg,mem"
:code-string "[rm: 0f 54 /r]"
:arch-flags (list "PENT" "MMX" "SM" "CYRIX")))

(setf PF2ID-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PF2ID"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r 1d]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PFACC-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PFACC"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r ae]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PFADD-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PFADD"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r 9e]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PFCMPEQ-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PFCMPEQ"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r b0]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PFCMPGE-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PFCMPGE"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r 90]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PFCMPGT-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PFCMPGT"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r a0]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PFMAX-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PFMAX"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r a4]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PFMIN-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PFMIN"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r 94]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PFMUL-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PFMUL"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r b4]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PFRCP-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PFRCP"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r 96]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PFRCPIT1-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PFRCPIT1"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r a6]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PFRCPIT2-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PFRCPIT2"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r b6]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PFRSQIT1-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PFRSQIT1"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r a7]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PFRSQRT-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PFRSQRT"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r 97]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PFSUB-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PFSUB"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r 9a]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PFSUBR-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PFSUBR"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r aa]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PI2FD-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PI2FD"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r 0d]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PMACHRIW-mmxreg.mem (make-instance 'x86-asm-instruction
:name "PMACHRIW"
:operands "mmxreg,mem"
:code-string "[rm: 0f 5e /r]"
:arch-flags (list "PENT" "MMX" "SM" "CYRIX")))

(setf PMADDWD-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PMADDWD"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f f5 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PMAGW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PMAGW"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 52 /r]"
:arch-flags (list "PENT" "MMX" "SQ" "CYRIX")))

(setf PMULHRIW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PMULHRIW"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 5d /r]"
:arch-flags (list "PENT" "MMX" "SQ" "CYRIX")))

(setf PMULHRWA-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PMULHRWA"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r b7]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PMULHRWC-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PMULHRWC"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 59 /r]"
:arch-flags (list "PENT" "MMX" "SQ" "CYRIX")))

(setf PMULHW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PMULHW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f e5 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PMULLW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PMULLW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f d5 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PMVGEZB-mmxreg.mem (make-instance 'x86-asm-instruction
:name "PMVGEZB"
:operands "mmxreg,mem"
:code-string "[rm: 0f 5c /r]"
:arch-flags (list "PENT" "MMX" "SQ" "CYRIX")))

(setf PMVLZB-mmxreg.mem (make-instance 'x86-asm-instruction
:name "PMVLZB"
:operands "mmxreg,mem"
:code-string "[rm: 0f 5b /r]"
:arch-flags (list "PENT" "MMX" "SQ" "CYRIX")))

(setf PMVNZB-mmxreg.mem (make-instance 'x86-asm-instruction
:name "PMVNZB"
:operands "mmxreg,mem"
:code-string "[rm: 0f 5a /r]"
:arch-flags (list "PENT" "MMX" "SQ" "CYRIX")))

(setf PMVZB-mmxreg.mem (make-instance 'x86-asm-instruction
:name "PMVZB"
:operands "mmxreg,mem"
:code-string "[rm: 0f 58 /r]"
:arch-flags (list "PENT" "MMX" "SQ" "CYRIX")))

(setf POP-reg16 (make-instance 'x86-asm-instruction
:name "POP"
:operands "reg16"
:code-string "[r: o16 58+r]"
:arch-flags (list "8086")))

(setf POP-reg64 (make-instance 'x86-asm-instruction
:name "POP"
:operands "reg64"
:code-string "[r: o64nw 58+r]"
:arch-flags (list "X64")))

(setf POP-rm16 (make-instance 'x86-asm-instruction
:name "POP"
:operands "rm16"
:code-string "[m: o16 8f /0]"
:arch-flags (list "8086")))

(setf POP-rm64 (make-instance 'x86-asm-instruction
:name "POP"
:operands "rm64"
:code-string "[m: o64nw 8f /0]"
:arch-flags (list "X64")))

(setf POP-reg_cs (make-instance 'x86-asm-instruction
:name "POP"
:operands "reg_cs"
:code-string "[-: 0f]"
:arch-flags (list "8086" "UNDOC" "ND")))

(setf POP-reg_fs (make-instance 'x86-asm-instruction
:name "POP"
:operands "reg_fs"
:code-string "[-: 0f a1]"
:arch-flags (list "386")))

(setf POP-reg_gs (make-instance 'x86-asm-instruction
:name "POP"
:operands "reg_gs"
:code-string "[-: 0f a9]"
:arch-flags (list "386")))

(setf POPF-void (make-instance 'x86-asm-instruction
:name "POPF"
:operands "void"
:code-string "[ odf 9d]"
:arch-flags (list "8086")))

(setf POPFQ-void (make-instance 'x86-asm-instruction
:name "POPFQ"
:operands "void"
:code-string "[ o32 9d]"
:arch-flags (list "X64")))

(setf POPFW-void (make-instance 'x86-asm-instruction
:name "POPFW"
:operands "void"
:code-string "[ o16 9d]"
:arch-flags (list "8086")))

(setf POR-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "POR"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f eb /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PREFETCH-mem (make-instance 'x86-asm-instruction
:name "PREFETCH"
:operands "mem"
:code-string "[m: 0f 0d /0]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PREFETCHW-mem (make-instance 'x86-asm-instruction
:name "PREFETCHW"
:operands "mem"
:code-string "[m: 0f 0d /1]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PSLLD-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSLLD"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f f2 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PSLLD-mmxreg.imm (make-instance 'x86-asm-instruction
:name "PSLLD"
:operands "mmxreg,imm"
:code-string "[mi: np 0f 72 /6 ib,u]"
:arch-flags (list "PENT" "MMX")))

(setf PSLLQ-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSLLQ"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f f3 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PSLLQ-mmxreg.imm (make-instance 'x86-asm-instruction
:name "PSLLQ"
:operands "mmxreg,imm"
:code-string "[mi: np 0f 73 /6 ib,u]"
:arch-flags (list "PENT" "MMX")))

(setf PSLLW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSLLW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f f1 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PSLLW-mmxreg.imm (make-instance 'x86-asm-instruction
:name "PSLLW"
:operands "mmxreg,imm"
:code-string "[mi: np 0f 71 /6 ib,u]"
:arch-flags (list "PENT" "MMX")))

(setf PSRAD-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSRAD"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f e2 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PSRAD-mmxreg.imm (make-instance 'x86-asm-instruction
:name "PSRAD"
:operands "mmxreg,imm"
:code-string "[mi: np 0f 72 /4 ib,u]"
:arch-flags (list "PENT" "MMX")))

(setf PSRAW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSRAW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f e1 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PSRAW-mmxreg.imm (make-instance 'x86-asm-instruction
:name "PSRAW"
:operands "mmxreg,imm"
:code-string "[mi: np 0f 71 /4 ib,u]"
:arch-flags (list "PENT" "MMX")))

(setf PSRLD-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSRLD"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f d2 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PSRLD-mmxreg.imm (make-instance 'x86-asm-instruction
:name "PSRLD"
:operands "mmxreg,imm"
:code-string "[mi: np 0f 72 /2 ib,u]"
:arch-flags (list "PENT" "MMX")))

(setf PSRLQ-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSRLQ"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f d3 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PSRLQ-mmxreg.imm (make-instance 'x86-asm-instruction
:name "PSRLQ"
:operands "mmxreg,imm"
:code-string "[mi: np 0f 73 /2 ib,u]"
:arch-flags (list "PENT" "MMX")))

(setf PSRLW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSRLW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f d1 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PSRLW-mmxreg.imm (make-instance 'x86-asm-instruction
:name "PSRLW"
:operands "mmxreg,imm"
:code-string "[mi: np 0f 71 /2 ib,u]"
:arch-flags (list "PENT" "MMX")))

(setf PSUBB-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSUBB"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f f8 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PSUBD-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSUBD"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f fa /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PSUBSB-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSUBSB"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f e8 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PSUBSIW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSUBSIW"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 55 /r]"
:arch-flags (list "PENT" "MMX" "SQ" "CYRIX")))

(setf PSUBSW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSUBSW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f e9 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PSUBUSB-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSUBUSB"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f d8 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PSUBUSW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSUBUSW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f d9 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PSUBW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSUBW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f f9 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PUNPCKHBW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PUNPCKHBW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f 68 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PUNPCKHDQ-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PUNPCKHDQ"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f 6a /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PUNPCKHWD-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PUNPCKHWD"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f 69 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PUNPCKLBW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PUNPCKLBW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f 60 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PUNPCKLDQ-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PUNPCKLDQ"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f 62 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PUNPCKLWD-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PUNPCKLWD"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f 61 /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf PUSH-reg16 (make-instance 'x86-asm-instruction
:name "PUSH"
:operands "reg16"
:code-string "[r: o16 50+r]"
:arch-flags (list "8086")))

(setf PUSH-reg64 (make-instance 'x86-asm-instruction
:name "PUSH"
:operands "reg64"
:code-string "[r: o64nw 50+r]"
:arch-flags (list "X64")))

(setf PUSH-rm16 (make-instance 'x86-asm-instruction
:name "PUSH"
:operands "rm16"
:code-string "[m: o16 ff /6]"
:arch-flags (list "8086")))

(setf PUSH-rm64 (make-instance 'x86-asm-instruction
:name "PUSH"
:operands "rm64"
:code-string "[m: o64nw ff /6]"
:arch-flags (list "X64")))

(setf PUSH-reg_fs (make-instance 'x86-asm-instruction
:name "PUSH"
:operands "reg_fs"
:code-string "[-: 0f a0]"
:arch-flags (list "386")))

(setf PUSH-reg_gs (make-instance 'x86-asm-instruction
:name "PUSH"
:operands "reg_gs"
:code-string "[-: 0f a8]"
:arch-flags (list "386")))

(setf PUSH-imm8 (make-instance 'x86-asm-instruction
:name "PUSH"
:operands "imm8"
:code-string "[i: 6a ib,s]"
:arch-flags (list "186")))

(setf PUSH-sbyteword16 (make-instance 'x86-asm-instruction
:name "PUSH"
:operands "sbyteword16"
:code-string "[i: o16 6a ib,s]"
:arch-flags (list "186" "AR0" "SIZE" "ND")))

(setf PUSH-imm16 (make-instance 'x86-asm-instruction
:name "PUSH"
:operands "imm16"
:code-string "[i: o16 68 iw]"
:arch-flags (list "186" "AR0" "SIZE")))

(setf PUSH-sbytedword64 (make-instance 'x86-asm-instruction
:name "PUSH"
:operands "sbytedword64"
:code-string "[i: o64nw 6a ib,s]"
:arch-flags (list "X64" "AR0" "SIZE" "ND")))

(setf PUSH-imm64 (make-instance 'x86-asm-instruction
:name "PUSH"
:operands "imm64"
:code-string "[i: o64nw 68 id,s]"
:arch-flags (list "X64" "AR0" "SIZE")))

(setf PUSH-sbytedword32 (make-instance 'x86-asm-instruction
:name "PUSH"
:operands "sbytedword32"
:code-string "[i: o64nw 6a ib,s]"
:arch-flags (list "X64" "AR0" "SIZE" "ND")))

(setf PUSH-imm32 (make-instance 'x86-asm-instruction
:name "PUSH"
:operands "imm32"
:code-string "[i: o64nw 68 id,s]"
:arch-flags (list "X64" "AR0" "SIZE")))

(setf PUSHF-void (make-instance 'x86-asm-instruction
:name "PUSHF"
:operands "void"
:code-string "[ odf 9c]"
:arch-flags (list "8086")))

(setf PUSHFQ-void (make-instance 'x86-asm-instruction
:name "PUSHFQ"
:operands "void"
:code-string "[ o32 9c]"
:arch-flags (list "X64")))

(setf PUSHFW-void (make-instance 'x86-asm-instruction
:name "PUSHFW"
:operands "void"
:code-string "[ o16 9c]"
:arch-flags (list "8086")))

(setf PXOR-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PXOR"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f ef /r]"
:arch-flags (list "PENT" "MMX" "SQ")))

(setf RCL-rm8.unity (make-instance 'x86-asm-instruction
:name "RCL"
:operands "rm8,unity"
:code-string "[m-: d0 /2]"
:arch-flags (list "8086")))

(setf RCL-rm8.reg_cl (make-instance 'x86-asm-instruction
:name "RCL"
:operands "rm8,reg_cl"
:code-string "[m-: d2 /2]"
:arch-flags (list "8086")))

(setf RCL-rm8.imm8 (make-instance 'x86-asm-instruction
:name "RCL"
:operands "rm8,imm8"
:code-string "[mi: c0 /2 ib,u]"
:arch-flags (list "186")))

(setf RCL-rm16.unity (make-instance 'x86-asm-instruction
:name "RCL"
:operands "rm16,unity"
:code-string "[m-: o16 d1 /2]"
:arch-flags (list "8086")))

(setf RCL-rm16.reg_cl (make-instance 'x86-asm-instruction
:name "RCL"
:operands "rm16,reg_cl"
:code-string "[m-: o16 d3 /2]"
:arch-flags (list "8086")))

(setf RCL-rm16.imm8 (make-instance 'x86-asm-instruction
:name "RCL"
:operands "rm16,imm8"
:code-string "[mi: o16 c1 /2 ib,u]"
:arch-flags (list "186")))

(setf RCL-rm32.unity (make-instance 'x86-asm-instruction
:name "RCL"
:operands "rm32,unity"
:code-string "[m-: o32 d1 /2]"
:arch-flags (list "386")))

(setf RCL-rm32.reg_cl (make-instance 'x86-asm-instruction
:name "RCL"
:operands "rm32,reg_cl"
:code-string "[m-: o32 d3 /2]"
:arch-flags (list "386")))

(setf RCL-rm32.imm8 (make-instance 'x86-asm-instruction
:name "RCL"
:operands "rm32,imm8"
:code-string "[mi: o32 c1 /2 ib,u]"
:arch-flags (list "386")))

(setf RCL-rm64.unity (make-instance 'x86-asm-instruction
:name "RCL"
:operands "rm64,unity"
:code-string "[m-: o64 d1 /2]"
:arch-flags (list "X64")))

(setf RCL-rm64.reg_cl (make-instance 'x86-asm-instruction
:name "RCL"
:operands "rm64,reg_cl"
:code-string "[m-: o64 d3 /2]"
:arch-flags (list "X64")))

(setf RCL-rm64.imm8 (make-instance 'x86-asm-instruction
:name "RCL"
:operands "rm64,imm8"
:code-string "[mi: o64 c1 /2 ib,u]"
:arch-flags (list "X64")))

(setf RCR-rm8.unity (make-instance 'x86-asm-instruction
:name "RCR"
:operands "rm8,unity"
:code-string "[m-: d0 /3]"
:arch-flags (list "8086")))

(setf RCR-rm8.reg_cl (make-instance 'x86-asm-instruction
:name "RCR"
:operands "rm8,reg_cl"
:code-string "[m-: d2 /3]"
:arch-flags (list "8086")))

(setf RCR-rm8.imm8 (make-instance 'x86-asm-instruction
:name "RCR"
:operands "rm8,imm8"
:code-string "[mi: c0 /3 ib,u]"
:arch-flags (list "186")))

(setf RCR-rm16.unity (make-instance 'x86-asm-instruction
:name "RCR"
:operands "rm16,unity"
:code-string "[m-: o16 d1 /3]"
:arch-flags (list "8086")))

(setf RCR-rm16.reg_cl (make-instance 'x86-asm-instruction
:name "RCR"
:operands "rm16,reg_cl"
:code-string "[m-: o16 d3 /3]"
:arch-flags (list "8086")))

(setf RCR-rm16.imm8 (make-instance 'x86-asm-instruction
:name "RCR"
:operands "rm16,imm8"
:code-string "[mi: o16 c1 /3 ib,u]"
:arch-flags (list "186")))

(setf RCR-rm32.unity (make-instance 'x86-asm-instruction
:name "RCR"
:operands "rm32,unity"
:code-string "[m-: o32 d1 /3]"
:arch-flags (list "386")))

(setf RCR-rm32.reg_cl (make-instance 'x86-asm-instruction
:name "RCR"
:operands "rm32,reg_cl"
:code-string "[m-: o32 d3 /3]"
:arch-flags (list "386")))

(setf RCR-rm32.imm8 (make-instance 'x86-asm-instruction
:name "RCR"
:operands "rm32,imm8"
:code-string "[mi: o32 c1 /3 ib,u]"
:arch-flags (list "386")))

(setf RCR-rm64.unity (make-instance 'x86-asm-instruction
:name "RCR"
:operands "rm64,unity"
:code-string "[m-: o64 d1 /3]"
:arch-flags (list "X64")))

(setf RCR-rm64.reg_cl (make-instance 'x86-asm-instruction
:name "RCR"
:operands "rm64,reg_cl"
:code-string "[m-: o64 d3 /3]"
:arch-flags (list "X64")))

(setf RCR-rm64.imm8 (make-instance 'x86-asm-instruction
:name "RCR"
:operands "rm64,imm8"
:code-string "[mi: o64 c1 /3 ib,u]"
:arch-flags (list "X64")))

(setf RDSHR-rm32 (make-instance 'x86-asm-instruction
:name "RDSHR"
:operands "rm32"
:code-string "[m: o32 0f 36 /0]"
:arch-flags (list "P6" "CYRIX" "SMM")))

(setf RDMSR-void (make-instance 'x86-asm-instruction
:name "RDMSR"
:operands "void"
:code-string "[ 0f 32]"
:arch-flags (list "PENT" "PRIV")))

(setf RDPMC-void (make-instance 'x86-asm-instruction
:name "RDPMC"
:operands "void"
:code-string "[ 0f 33]"
:arch-flags (list "P6")))

(setf RDTSC-void (make-instance 'x86-asm-instruction
:name "RDTSC"
:operands "void"
:code-string "[ 0f 31]"
:arch-flags (list "PENT")))

(setf RDTSCP-void (make-instance 'x86-asm-instruction
:name "RDTSCP"
:operands "void"
:code-string "[ 0f 01 f9]"
:arch-flags (list "X86_64")))

(setf RET-void (make-instance 'x86-asm-instruction
:name "RET"
:operands "void"
:code-string "[ c3]"
:arch-flags (list "8086" "BND")))

(setf RET-imm (make-instance 'x86-asm-instruction
:name "RET"
:operands "imm"
:code-string "[i: c2 iw]"
:arch-flags (list "8086" "SW" "BND")))

(setf RETF-void (make-instance 'x86-asm-instruction
:name "RETF"
:operands "void"
:code-string "[ cb]"
:arch-flags (list "8086")))

(setf RETF-imm (make-instance 'x86-asm-instruction
:name "RETF"
:operands "imm"
:code-string "[i: ca iw]"
:arch-flags (list "8086" "SW")))

(setf RETN-void (make-instance 'x86-asm-instruction
:name "RETN"
:operands "void"
:code-string "[ c3]"
:arch-flags (list "8086" "BND")))

(setf RETN-imm (make-instance 'x86-asm-instruction
:name "RETN"
:operands "imm"
:code-string "[i: c2 iw]"
:arch-flags (list "8086" "SW" "BND")))

(setf ROL-rm8.unity (make-instance 'x86-asm-instruction
:name "ROL"
:operands "rm8,unity"
:code-string "[m-: d0 /0]"
:arch-flags (list "8086")))

(setf ROL-rm8.reg_cl (make-instance 'x86-asm-instruction
:name "ROL"
:operands "rm8,reg_cl"
:code-string "[m-: d2 /0]"
:arch-flags (list "8086")))

(setf ROL-rm8.imm8 (make-instance 'x86-asm-instruction
:name "ROL"
:operands "rm8,imm8"
:code-string "[mi: c0 /0 ib,u]"
:arch-flags (list "186")))

(setf ROL-rm16.unity (make-instance 'x86-asm-instruction
:name "ROL"
:operands "rm16,unity"
:code-string "[m-: o16 d1 /0]"
:arch-flags (list "8086")))

(setf ROL-rm16.reg_cl (make-instance 'x86-asm-instruction
:name "ROL"
:operands "rm16,reg_cl"
:code-string "[m-: o16 d3 /0]"
:arch-flags (list "8086")))

(setf ROL-rm16.imm8 (make-instance 'x86-asm-instruction
:name "ROL"
:operands "rm16,imm8"
:code-string "[mi: o16 c1 /0 ib,u]"
:arch-flags (list "186")))

(setf ROL-rm32.unity (make-instance 'x86-asm-instruction
:name "ROL"
:operands "rm32,unity"
:code-string "[m-: o32 d1 /0]"
:arch-flags (list "386")))

(setf ROL-rm32.reg_cl (make-instance 'x86-asm-instruction
:name "ROL"
:operands "rm32,reg_cl"
:code-string "[m-: o32 d3 /0]"
:arch-flags (list "386")))

(setf ROL-rm32.imm8 (make-instance 'x86-asm-instruction
:name "ROL"
:operands "rm32,imm8"
:code-string "[mi: o32 c1 /0 ib,u]"
:arch-flags (list "386")))

(setf ROL-rm64.unity (make-instance 'x86-asm-instruction
:name "ROL"
:operands "rm64,unity"
:code-string "[m-: o64 d1 /0]"
:arch-flags (list "X64")))

(setf ROL-rm64.reg_cl (make-instance 'x86-asm-instruction
:name "ROL"
:operands "rm64,reg_cl"
:code-string "[m-: o64 d3 /0]"
:arch-flags (list "X64")))

(setf ROL-rm64.imm8 (make-instance 'x86-asm-instruction
:name "ROL"
:operands "rm64,imm8"
:code-string "[mi: o64 c1 /0 ib,u]"
:arch-flags (list "X64")))

(setf ROR-rm8.unity (make-instance 'x86-asm-instruction
:name "ROR"
:operands "rm8,unity"
:code-string "[m-: d0 /1]"
:arch-flags (list "8086")))

(setf ROR-rm8.reg_cl (make-instance 'x86-asm-instruction
:name "ROR"
:operands "rm8,reg_cl"
:code-string "[m-: d2 /1]"
:arch-flags (list "8086")))

(setf ROR-rm8.imm8 (make-instance 'x86-asm-instruction
:name "ROR"
:operands "rm8,imm8"
:code-string "[mi: c0 /1 ib,u]"
:arch-flags (list "186")))

(setf ROR-rm16.unity (make-instance 'x86-asm-instruction
:name "ROR"
:operands "rm16,unity"
:code-string "[m-: o16 d1 /1]"
:arch-flags (list "8086")))

(setf ROR-rm16.reg_cl (make-instance 'x86-asm-instruction
:name "ROR"
:operands "rm16,reg_cl"
:code-string "[m-: o16 d3 /1]"
:arch-flags (list "8086")))

(setf ROR-rm16.imm8 (make-instance 'x86-asm-instruction
:name "ROR"
:operands "rm16,imm8"
:code-string "[mi: o16 c1 /1 ib,u]"
:arch-flags (list "186")))

(setf ROR-rm32.unity (make-instance 'x86-asm-instruction
:name "ROR"
:operands "rm32,unity"
:code-string "[m-: o32 d1 /1]"
:arch-flags (list "386")))

(setf ROR-rm32.reg_cl (make-instance 'x86-asm-instruction
:name "ROR"
:operands "rm32,reg_cl"
:code-string "[m-: o32 d3 /1]"
:arch-flags (list "386")))

(setf ROR-rm32.imm8 (make-instance 'x86-asm-instruction
:name "ROR"
:operands "rm32,imm8"
:code-string "[mi: o32 c1 /1 ib,u]"
:arch-flags (list "386")))

(setf ROR-rm64.unity (make-instance 'x86-asm-instruction
:name "ROR"
:operands "rm64,unity"
:code-string "[m-: o64 d1 /1]"
:arch-flags (list "X64")))

(setf ROR-rm64.reg_cl (make-instance 'x86-asm-instruction
:name "ROR"
:operands "rm64,reg_cl"
:code-string "[m-: o64 d3 /1]"
:arch-flags (list "X64")))

(setf ROR-rm64.imm8 (make-instance 'x86-asm-instruction
:name "ROR"
:operands "rm64,imm8"
:code-string "[mi: o64 c1 /1 ib,u]"
:arch-flags (list "X64")))

(setf RDM-void (make-instance 'x86-asm-instruction
:name "RDM"
:operands "void"
:code-string "[ 0f 3a]"
:arch-flags (list "P6" "CYRIX" "ND")))

(setf RSDC-reg_sreg.mem80 (make-instance 'x86-asm-instruction
:name "RSDC"
:operands "reg_sreg,mem80"
:code-string "[rm: 0f 79 /r]"
:arch-flags (list "486" "CYRIX" "SMM")))

(setf RSLDT-mem80 (make-instance 'x86-asm-instruction
:name "RSLDT"
:operands "mem80"
:code-string "[m: 0f 7b /0]"
:arch-flags (list "486" "CYRIX" "SMM")))

(setf RSM-void (make-instance 'x86-asm-instruction
:name "RSM"
:operands "void"
:code-string "[ 0f aa]"
:arch-flags (list "PENT" "SMM")))

(setf RSTS-mem80 (make-instance 'x86-asm-instruction
:name "RSTS"
:operands "mem80"
:code-string "[m: 0f 7d /0]"
:arch-flags (list "486" "CYRIX" "SMM")))

(setf SAHF-void (make-instance 'x86-asm-instruction
:name "SAHF"
:operands "void"
:code-string "[ 9e]"
:arch-flags (list "8086")))

(setf SAL-rm8.unity (make-instance 'x86-asm-instruction
:name "SAL"
:operands "rm8,unity"
:code-string "[m-: d0 /4]"
:arch-flags (list "8086" "ND")))

(setf SAL-rm8.reg_cl (make-instance 'x86-asm-instruction
:name "SAL"
:operands "rm8,reg_cl"
:code-string "[m-: d2 /4]"
:arch-flags (list "8086" "ND")))

(setf SAL-rm8.imm8 (make-instance 'x86-asm-instruction
:name "SAL"
:operands "rm8,imm8"
:code-string "[mi: c0 /4 ib,u]"
:arch-flags (list "186" "ND")))

(setf SAL-rm16.unity (make-instance 'x86-asm-instruction
:name "SAL"
:operands "rm16,unity"
:code-string "[m-: o16 d1 /4]"
:arch-flags (list "8086" "ND")))

(setf SAL-rm16.reg_cl (make-instance 'x86-asm-instruction
:name "SAL"
:operands "rm16,reg_cl"
:code-string "[m-: o16 d3 /4]"
:arch-flags (list "8086" "ND")))

(setf SAL-rm16.imm8 (make-instance 'x86-asm-instruction
:name "SAL"
:operands "rm16,imm8"
:code-string "[mi: o16 c1 /4 ib,u]"
:arch-flags (list "186" "ND")))

(setf SAL-rm32.unity (make-instance 'x86-asm-instruction
:name "SAL"
:operands "rm32,unity"
:code-string "[m-: o32 d1 /4]"
:arch-flags (list "386" "ND")))

(setf SAL-rm32.reg_cl (make-instance 'x86-asm-instruction
:name "SAL"
:operands "rm32,reg_cl"
:code-string "[m-: o32 d3 /4]"
:arch-flags (list "386" "ND")))

(setf SAL-rm32.imm8 (make-instance 'x86-asm-instruction
:name "SAL"
:operands "rm32,imm8"
:code-string "[mi: o32 c1 /4 ib,u]"
:arch-flags (list "386" "ND")))

(setf SAL-rm64.unity (make-instance 'x86-asm-instruction
:name "SAL"
:operands "rm64,unity"
:code-string "[m-: o64 d1 /4]"
:arch-flags (list "X64" "ND")))

(setf SAL-rm64.reg_cl (make-instance 'x86-asm-instruction
:name "SAL"
:operands "rm64,reg_cl"
:code-string "[m-: o64 d3 /4]"
:arch-flags (list "X64" "ND")))

(setf SAL-rm64.imm8 (make-instance 'x86-asm-instruction
:name "SAL"
:operands "rm64,imm8"
:code-string "[mi: o64 c1 /4 ib,u]"
:arch-flags (list "X64" "ND")))

(setf SALC-void (make-instance 'x86-asm-instruction
:name "SALC"
:operands "void"
:code-string "[ d6]"
:arch-flags (list "8086" "UNDOC")))

(setf SAR-rm8.unity (make-instance 'x86-asm-instruction
:name "SAR"
:operands "rm8,unity"
:code-string "[m-: d0 /7]"
:arch-flags (list "8086")))

(setf SAR-rm8.reg_cl (make-instance 'x86-asm-instruction
:name "SAR"
:operands "rm8,reg_cl"
:code-string "[m-: d2 /7]"
:arch-flags (list "8086")))

(setf SAR-rm8.imm8 (make-instance 'x86-asm-instruction
:name "SAR"
:operands "rm8,imm8"
:code-string "[mi: c0 /7 ib,u]"
:arch-flags (list "186")))

(setf SAR-rm16.unity (make-instance 'x86-asm-instruction
:name "SAR"
:operands "rm16,unity"
:code-string "[m-: o16 d1 /7]"
:arch-flags (list "8086")))

(setf SAR-rm16.reg_cl (make-instance 'x86-asm-instruction
:name "SAR"
:operands "rm16,reg_cl"
:code-string "[m-: o16 d3 /7]"
:arch-flags (list "8086")))

(setf SAR-rm16.imm8 (make-instance 'x86-asm-instruction
:name "SAR"
:operands "rm16,imm8"
:code-string "[mi: o16 c1 /7 ib,u]"
:arch-flags (list "186")))

(setf SAR-rm32.unity (make-instance 'x86-asm-instruction
:name "SAR"
:operands "rm32,unity"
:code-string "[m-: o32 d1 /7]"
:arch-flags (list "386")))

(setf SAR-rm32.reg_cl (make-instance 'x86-asm-instruction
:name "SAR"
:operands "rm32,reg_cl"
:code-string "[m-: o32 d3 /7]"
:arch-flags (list "386")))

(setf SAR-rm32.imm8 (make-instance 'x86-asm-instruction
:name "SAR"
:operands "rm32,imm8"
:code-string "[mi: o32 c1 /7 ib,u]"
:arch-flags (list "386")))

(setf SAR-rm64.unity (make-instance 'x86-asm-instruction
:name "SAR"
:operands "rm64,unity"
:code-string "[m-: o64 d1 /7]"
:arch-flags (list "X64")))

(setf SAR-rm64.reg_cl (make-instance 'x86-asm-instruction
:name "SAR"
:operands "rm64,reg_cl"
:code-string "[m-: o64 d3 /7]"
:arch-flags (list "X64")))

(setf SAR-rm64.imm8 (make-instance 'x86-asm-instruction
:name "SAR"
:operands "rm64,imm8"
:code-string "[mi: o64 c1 /7 ib,u]"
:arch-flags (list "X64")))

(setf SBB-mem.reg8 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "mem,reg8"
:code-string "[mr: hle 18 /r]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf SBB-reg8.reg8 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "reg8,reg8"
:code-string "[mr: 18 /r]"
:arch-flags (list "8086")))

(setf SBB-mem.reg16 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "mem,reg16"
:code-string "[mr: hle o16 19 /r]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf SBB-reg16.reg16 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "reg16,reg16"
:code-string "[mr: o16 19 /r]"
:arch-flags (list "8086")))

(setf SBB-mem.reg32 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "mem,reg32"
:code-string "[mr: hle o32 19 /r]"
:arch-flags (list "386" "SM" "LOCK")))

(setf SBB-reg32.reg32 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "reg32,reg32"
:code-string "[mr: o32 19 /r]"
:arch-flags (list "386")))

(setf SBB-mem.reg64 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "mem,reg64"
:code-string "[mr: hle o64 19 /r]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf SBB-reg64.reg64 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "reg64,reg64"
:code-string "[mr: o64 19 /r]"
:arch-flags (list "X64")))

(setf SBB-reg8.mem (make-instance 'x86-asm-instruction
:name "SBB"
:operands "reg8,mem"
:code-string "[rm: 1a /r]"
:arch-flags (list "8086" "SM")))

(setf SBB-reg8.reg8 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "reg8,reg8"
:code-string "[rm: 1a /r]"
:arch-flags (list "8086")))

(setf SBB-reg16.mem (make-instance 'x86-asm-instruction
:name "SBB"
:operands "reg16,mem"
:code-string "[rm: o16 1b /r]"
:arch-flags (list "8086" "SM")))

(setf SBB-reg16.reg16 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "reg16,reg16"
:code-string "[rm: o16 1b /r]"
:arch-flags (list "8086")))

(setf SBB-reg32.mem (make-instance 'x86-asm-instruction
:name "SBB"
:operands "reg32,mem"
:code-string "[rm: o32 1b /r]"
:arch-flags (list "386" "SM")))

(setf SBB-reg32.reg32 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "reg32,reg32"
:code-string "[rm: o32 1b /r]"
:arch-flags (list "386")))

(setf SBB-reg64.mem (make-instance 'x86-asm-instruction
:name "SBB"
:operands "reg64,mem"
:code-string "[rm: o64 1b /r]"
:arch-flags (list "X64" "SM")))

(setf SBB-reg64.reg64 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "reg64,reg64"
:code-string "[rm: o64 1b /r]"
:arch-flags (list "X64")))

(setf SBB-rm16.imm8 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "rm16,imm8"
:code-string "[mi: hle o16 83 /3 ib,s]"
:arch-flags (list "8086" "LOCK")))

(setf SBB-rm32.imm8 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "rm32,imm8"
:code-string "[mi: hle o32 83 /3 ib,s]"
:arch-flags (list "386" "LOCK")))

(setf SBB-rm64.imm8 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "rm64,imm8"
:code-string "[mi: hle o64 83 /3 ib,s]"
:arch-flags (list "X64" "LOCK")))

(setf SBB-reg_al.imm (make-instance 'x86-asm-instruction
:name "SBB"
:operands "reg_al,imm"
:code-string "[-i: 1c ib]"
:arch-flags (list "8086" "SM")))

(setf SBB-reg_ax.sbyteword (make-instance 'x86-asm-instruction
:name "SBB"
:operands "reg_ax,sbyteword"
:code-string "[mi: o16 83 /3 ib,s]"
:arch-flags (list "8086" "SM" "ND")))

(setf SBB-reg_ax.imm (make-instance 'x86-asm-instruction
:name "SBB"
:operands "reg_ax,imm"
:code-string "[-i: o16 1d iw]"
:arch-flags (list "8086" "SM")))

(setf SBB-reg_eax.sbytedword (make-instance 'x86-asm-instruction
:name "SBB"
:operands "reg_eax,sbytedword"
:code-string "[mi: o32 83 /3 ib,s]"
:arch-flags (list "386" "SM" "ND")))

(setf SBB-reg_eax.imm (make-instance 'x86-asm-instruction
:name "SBB"
:operands "reg_eax,imm"
:code-string "[-i: o32 1d id]"
:arch-flags (list "386" "SM")))

(setf SBB-reg_rax.sbytedword (make-instance 'x86-asm-instruction
:name "SBB"
:operands "reg_rax,sbytedword"
:code-string "[mi: o64 83 /3 ib,s]"
:arch-flags (list "X64" "SM" "ND")))

(setf SBB-reg_rax.imm (make-instance 'x86-asm-instruction
:name "SBB"
:operands "reg_rax,imm"
:code-string "[-i: o64 1d id,s]"
:arch-flags (list "X64" "SM")))

(setf SBB-rm8.imm (make-instance 'x86-asm-instruction
:name "SBB"
:operands "rm8,imm"
:code-string "[mi: hle 80 /3 ib]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf SBB-rm16.sbyteword (make-instance 'x86-asm-instruction
:name "SBB"
:operands "rm16,sbyteword"
:code-string "[mi: hle o16 83 /3 ib,s]"
:arch-flags (list "8086" "SM" "LOCK" "ND")))

(setf SBB-rm16.imm (make-instance 'x86-asm-instruction
:name "SBB"
:operands "rm16,imm"
:code-string "[mi: hle o16 81 /3 iw]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf SBB-rm32.sbytedword (make-instance 'x86-asm-instruction
:name "SBB"
:operands "rm32,sbytedword"
:code-string "[mi: hle o32 83 /3 ib,s]"
:arch-flags (list "386" "SM" "LOCK" "ND")))

(setf SBB-rm32.imm (make-instance 'x86-asm-instruction
:name "SBB"
:operands "rm32,imm"
:code-string "[mi: hle o32 81 /3 id]"
:arch-flags (list "386" "SM" "LOCK")))

(setf SBB-rm64.sbytedword (make-instance 'x86-asm-instruction
:name "SBB"
:operands "rm64,sbytedword"
:code-string "[mi: hle o64 83 /3 ib,s]"
:arch-flags (list "X64" "SM" "LOCK" "ND")))

(setf SBB-rm64.imm (make-instance 'x86-asm-instruction
:name "SBB"
:operands "rm64,imm"
:code-string "[mi: hle o64 81 /3 id,s]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf SBB-mem.imm8 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "mem,imm8"
:code-string "[mi: hle 80 /3 ib]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf SBB-mem.sbyteword16 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "mem,sbyteword16"
:code-string "[mi: hle o16 83 /3 ib,s]"
:arch-flags (list "8086" "SM" "LOCK" "ND")))

(setf SBB-mem.imm16 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "mem,imm16"
:code-string "[mi: hle o16 81 /3 iw]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf SBB-mem.sbytedword32 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "mem,sbytedword32"
:code-string "[mi: hle o32 83 /3 ib,s]"
:arch-flags (list "386" "SM" "LOCK" "ND")))

(setf SBB-mem.imm32 (make-instance 'x86-asm-instruction
:name "SBB"
:operands "mem,imm32"
:code-string "[mi: hle o32 81 /3 id]"
:arch-flags (list "386" "SM" "LOCK")))

(setf SCASB-void (make-instance 'x86-asm-instruction
:name "SCASB"
:operands "void"
:code-string "[ repe ae]"
:arch-flags (list "8086")))

(setf SCASD-void (make-instance 'x86-asm-instruction
:name "SCASD"
:operands "void"
:code-string "[ repe o32 af]"
:arch-flags (list "386")))

(setf SCASQ-void (make-instance 'x86-asm-instruction
:name "SCASQ"
:operands "void"
:code-string "[ repe o64 af]"
:arch-flags (list "X64")))

(setf SCASW-void (make-instance 'x86-asm-instruction
:name "SCASW"
:operands "void"
:code-string "[ repe o16 af]"
:arch-flags (list "8086")))

(setf SFENCE-void (make-instance 'x86-asm-instruction
:name "SFENCE"
:operands "void"
:code-string "[ np 0f ae f8]"
:arch-flags (list "X64" "AMD")))

(setf SGDT-mem (make-instance 'x86-asm-instruction
:name "SGDT"
:operands "mem"
:code-string "[m: 0f 01 /0]"
:arch-flags (list "286")))

(setf SHL-rm8.unity (make-instance 'x86-asm-instruction
:name "SHL"
:operands "rm8,unity"
:code-string "[m-: d0 /4]"
:arch-flags (list "8086")))

(setf SHL-rm8.reg_cl (make-instance 'x86-asm-instruction
:name "SHL"
:operands "rm8,reg_cl"
:code-string "[m-: d2 /4]"
:arch-flags (list "8086")))

(setf SHL-rm8.imm8 (make-instance 'x86-asm-instruction
:name "SHL"
:operands "rm8,imm8"
:code-string "[mi: c0 /4 ib,u]"
:arch-flags (list "186")))

(setf SHL-rm16.unity (make-instance 'x86-asm-instruction
:name "SHL"
:operands "rm16,unity"
:code-string "[m-: o16 d1 /4]"
:arch-flags (list "8086")))

(setf SHL-rm16.reg_cl (make-instance 'x86-asm-instruction
:name "SHL"
:operands "rm16,reg_cl"
:code-string "[m-: o16 d3 /4]"
:arch-flags (list "8086")))

(setf SHL-rm16.imm8 (make-instance 'x86-asm-instruction
:name "SHL"
:operands "rm16,imm8"
:code-string "[mi: o16 c1 /4 ib,u]"
:arch-flags (list "186")))

(setf SHL-rm32.unity (make-instance 'x86-asm-instruction
:name "SHL"
:operands "rm32,unity"
:code-string "[m-: o32 d1 /4]"
:arch-flags (list "386")))

(setf SHL-rm32.reg_cl (make-instance 'x86-asm-instruction
:name "SHL"
:operands "rm32,reg_cl"
:code-string "[m-: o32 d3 /4]"
:arch-flags (list "386")))

(setf SHL-rm32.imm8 (make-instance 'x86-asm-instruction
:name "SHL"
:operands "rm32,imm8"
:code-string "[mi: o32 c1 /4 ib,u]"
:arch-flags (list "386")))

(setf SHL-rm64.unity (make-instance 'x86-asm-instruction
:name "SHL"
:operands "rm64,unity"
:code-string "[m-: o64 d1 /4]"
:arch-flags (list "X64")))

(setf SHL-rm64.reg_cl (make-instance 'x86-asm-instruction
:name "SHL"
:operands "rm64,reg_cl"
:code-string "[m-: o64 d3 /4]"
:arch-flags (list "X64")))

(setf SHL-rm64.imm8 (make-instance 'x86-asm-instruction
:name "SHL"
:operands "rm64,imm8"
:code-string "[mi: o64 c1 /4 ib,u]"
:arch-flags (list "X64")))

(setf SHLD-mem.reg16.imm (make-instance 'x86-asm-instruction
:name "SHLD"
:operands "mem,reg16,imm"
:code-string "[mri: o16 0f a4 /r ib,u]"
:arch-flags (list "386" "SM2" "SB" "AR2")))

(setf SHLD-reg16.reg16.imm (make-instance 'x86-asm-instruction
:name "SHLD"
:operands "reg16,reg16,imm"
:code-string "[mri: o16 0f a4 /r ib,u]"
:arch-flags (list "386" "SM2" "SB" "AR2")))

(setf SHLD-mem.reg32.imm (make-instance 'x86-asm-instruction
:name "SHLD"
:operands "mem,reg32,imm"
:code-string "[mri: o32 0f a4 /r ib,u]"
:arch-flags (list "386" "SM2" "SB" "AR2")))

(setf SHLD-reg32.reg32.imm (make-instance 'x86-asm-instruction
:name "SHLD"
:operands "reg32,reg32,imm"
:code-string "[mri: o32 0f a4 /r ib,u]"
:arch-flags (list "386" "SM2" "SB" "AR2")))

(setf SHLD-mem.reg64.imm (make-instance 'x86-asm-instruction
:name "SHLD"
:operands "mem,reg64,imm"
:code-string "[mri: o64 0f a4 /r ib,u]"
:arch-flags (list "X64" "SM2" "SB" "AR2")))

(setf SHLD-reg64.reg64.imm (make-instance 'x86-asm-instruction
:name "SHLD"
:operands "reg64,reg64,imm"
:code-string "[mri: o64 0f a4 /r ib,u]"
:arch-flags (list "X64" "SM2" "SB" "AR2")))

(setf SHLD-mem.reg16.reg_cl (make-instance 'x86-asm-instruction
:name "SHLD"
:operands "mem,reg16,reg_cl"
:code-string "[mr-: o16 0f a5 /r]"
:arch-flags (list "386" "SM")))

(setf SHLD-reg16.reg16.reg_cl (make-instance 'x86-asm-instruction
:name "SHLD"
:operands "reg16,reg16,reg_cl"
:code-string "[mr-: o16 0f a5 /r]"
:arch-flags (list "386")))

(setf SHLD-mem.reg32.reg_cl (make-instance 'x86-asm-instruction
:name "SHLD"
:operands "mem,reg32,reg_cl"
:code-string "[mr-: o32 0f a5 /r]"
:arch-flags (list "386" "SM")))

(setf SHLD-reg32.reg32.reg_cl (make-instance 'x86-asm-instruction
:name "SHLD"
:operands "reg32,reg32,reg_cl"
:code-string "[mr-: o32 0f a5 /r]"
:arch-flags (list "386")))

(setf SHLD-mem.reg64.reg_cl (make-instance 'x86-asm-instruction
:name "SHLD"
:operands "mem,reg64,reg_cl"
:code-string "[mr-: o64 0f a5 /r]"
:arch-flags (list "X64" "SM")))

(setf SHLD-reg64.reg64.reg_cl (make-instance 'x86-asm-instruction
:name "SHLD"
:operands "reg64,reg64,reg_cl"
:code-string "[mr-: o64 0f a5 /r]"
:arch-flags (list "X64")))

(setf SHR-rm8.unity (make-instance 'x86-asm-instruction
:name "SHR"
:operands "rm8,unity"
:code-string "[m-: d0 /5]"
:arch-flags (list "8086")))

(setf SHR-rm8.reg_cl (make-instance 'x86-asm-instruction
:name "SHR"
:operands "rm8,reg_cl"
:code-string "[m-: d2 /5]"
:arch-flags (list "8086")))

(setf SHR-rm8.imm8 (make-instance 'x86-asm-instruction
:name "SHR"
:operands "rm8,imm8"
:code-string "[mi: c0 /5 ib,u]"
:arch-flags (list "186")))

(setf SHR-rm16.unity (make-instance 'x86-asm-instruction
:name "SHR"
:operands "rm16,unity"
:code-string "[m-: o16 d1 /5]"
:arch-flags (list "8086")))

(setf SHR-rm16.reg_cl (make-instance 'x86-asm-instruction
:name "SHR"
:operands "rm16,reg_cl"
:code-string "[m-: o16 d3 /5]"
:arch-flags (list "8086")))

(setf SHR-rm16.imm8 (make-instance 'x86-asm-instruction
:name "SHR"
:operands "rm16,imm8"
:code-string "[mi: o16 c1 /5 ib,u]"
:arch-flags (list "186")))

(setf SHR-rm32.unity (make-instance 'x86-asm-instruction
:name "SHR"
:operands "rm32,unity"
:code-string "[m-: o32 d1 /5]"
:arch-flags (list "386")))

(setf SHR-rm32.reg_cl (make-instance 'x86-asm-instruction
:name "SHR"
:operands "rm32,reg_cl"
:code-string "[m-: o32 d3 /5]"
:arch-flags (list "386")))

(setf SHR-rm32.imm8 (make-instance 'x86-asm-instruction
:name "SHR"
:operands "rm32,imm8"
:code-string "[mi: o32 c1 /5 ib,u]"
:arch-flags (list "386")))

(setf SHR-rm64.unity (make-instance 'x86-asm-instruction
:name "SHR"
:operands "rm64,unity"
:code-string "[m-: o64 d1 /5]"
:arch-flags (list "X64")))

(setf SHR-rm64.reg_cl (make-instance 'x86-asm-instruction
:name "SHR"
:operands "rm64,reg_cl"
:code-string "[m-: o64 d3 /5]"
:arch-flags (list "X64")))

(setf SHR-rm64.imm8 (make-instance 'x86-asm-instruction
:name "SHR"
:operands "rm64,imm8"
:code-string "[mi: o64 c1 /5 ib,u]"
:arch-flags (list "X64")))

(setf SHRD-mem.reg16.imm (make-instance 'x86-asm-instruction
:name "SHRD"
:operands "mem,reg16,imm"
:code-string "[mri: o16 0f ac /r ib,u]"
:arch-flags (list "386" "SM2" "SB" "AR2")))

(setf SHRD-reg16.reg16.imm (make-instance 'x86-asm-instruction
:name "SHRD"
:operands "reg16,reg16,imm"
:code-string "[mri: o16 0f ac /r ib,u]"
:arch-flags (list "386" "SM2" "SB" "AR2")))

(setf SHRD-mem.reg32.imm (make-instance 'x86-asm-instruction
:name "SHRD"
:operands "mem,reg32,imm"
:code-string "[mri: o32 0f ac /r ib,u]"
:arch-flags (list "386" "SM2" "SB" "AR2")))

(setf SHRD-reg32.reg32.imm (make-instance 'x86-asm-instruction
:name "SHRD"
:operands "reg32,reg32,imm"
:code-string "[mri: o32 0f ac /r ib,u]"
:arch-flags (list "386" "SM2" "SB" "AR2")))

(setf SHRD-mem.reg64.imm (make-instance 'x86-asm-instruction
:name "SHRD"
:operands "mem,reg64,imm"
:code-string "[mri: o64 0f ac /r ib,u]"
:arch-flags (list "X64" "SM2" "SB" "AR2")))

(setf SHRD-reg64.reg64.imm (make-instance 'x86-asm-instruction
:name "SHRD"
:operands "reg64,reg64,imm"
:code-string "[mri: o64 0f ac /r ib,u]"
:arch-flags (list "X64" "SM2" "SB" "AR2")))

(setf SHRD-mem.reg16.reg_cl (make-instance 'x86-asm-instruction
:name "SHRD"
:operands "mem,reg16,reg_cl"
:code-string "[mr-: o16 0f ad /r]"
:arch-flags (list "386" "SM")))

(setf SHRD-reg16.reg16.reg_cl (make-instance 'x86-asm-instruction
:name "SHRD"
:operands "reg16,reg16,reg_cl"
:code-string "[mr-: o16 0f ad /r]"
:arch-flags (list "386")))

(setf SHRD-mem.reg32.reg_cl (make-instance 'x86-asm-instruction
:name "SHRD"
:operands "mem,reg32,reg_cl"
:code-string "[mr-: o32 0f ad /r]"
:arch-flags (list "386" "SM")))

(setf SHRD-reg32.reg32.reg_cl (make-instance 'x86-asm-instruction
:name "SHRD"
:operands "reg32,reg32,reg_cl"
:code-string "[mr-: o32 0f ad /r]"
:arch-flags (list "386")))

(setf SHRD-mem.reg64.reg_cl (make-instance 'x86-asm-instruction
:name "SHRD"
:operands "mem,reg64,reg_cl"
:code-string "[mr-: o64 0f ad /r]"
:arch-flags (list "X64" "SM")))

(setf SHRD-reg64.reg64.reg_cl (make-instance 'x86-asm-instruction
:name "SHRD"
:operands "reg64,reg64,reg_cl"
:code-string "[mr-: o64 0f ad /r]"
:arch-flags (list "X64")))

(setf SIDT-mem (make-instance 'x86-asm-instruction
:name "SIDT"
:operands "mem"
:code-string "[m: 0f 01 /1]"
:arch-flags (list "286")))

(setf SLDT-mem (make-instance 'x86-asm-instruction
:name "SLDT"
:operands "mem"
:code-string "[m: 0f 00 /0]"
:arch-flags (list "286")))

(setf SLDT-mem16 (make-instance 'x86-asm-instruction
:name "SLDT"
:operands "mem16"
:code-string "[m: 0f 00 /0]"
:arch-flags (list "286")))

(setf SLDT-reg16 (make-instance 'x86-asm-instruction
:name "SLDT"
:operands "reg16"
:code-string "[m: o16 0f 00 /0]"
:arch-flags (list "286")))

(setf SLDT-reg32 (make-instance 'x86-asm-instruction
:name "SLDT"
:operands "reg32"
:code-string "[m: o32 0f 00 /0]"
:arch-flags (list "386")))

(setf SLDT-reg64 (make-instance 'x86-asm-instruction
:name "SLDT"
:operands "reg64"
:code-string "[m: o64nw 0f 00 /0]"
:arch-flags (list "X64" "ND")))

(setf SLDT-reg64 (make-instance 'x86-asm-instruction
:name "SLDT"
:operands "reg64"
:code-string "[m: o64 0f 00 /0]"
:arch-flags (list "X64")))

(setf SKINIT-void (make-instance 'x86-asm-instruction
:name "SKINIT"
:operands "void"
:code-string "[ 0f 01 de]"
:arch-flags (list "X64")))

(setf SMI-void (make-instance 'x86-asm-instruction
:name "SMI"
:operands "void"
:code-string "[ f1]"
:arch-flags (list "386" "UNDOC")))

(setf SMINT-void (make-instance 'x86-asm-instruction
:name "SMINT"
:operands "void"
:code-string "[ 0f 38]"
:arch-flags (list "P6" "CYRIX" "ND")))

(setf SMINTOLD-void (make-instance 'x86-asm-instruction
:name "SMINTOLD"
:operands "void"
:code-string "[ 0f 7e]"
:arch-flags (list "486" "CYRIX" "ND")))

(setf SMSW-mem (make-instance 'x86-asm-instruction
:name "SMSW"
:operands "mem"
:code-string "[m: 0f 01 /4]"
:arch-flags (list "286")))

(setf SMSW-mem16 (make-instance 'x86-asm-instruction
:name "SMSW"
:operands "mem16"
:code-string "[m: 0f 01 /4]"
:arch-flags (list "286")))

(setf SMSW-reg16 (make-instance 'x86-asm-instruction
:name "SMSW"
:operands "reg16"
:code-string "[m: o16 0f 01 /4]"
:arch-flags (list "286")))

(setf SMSW-reg32 (make-instance 'x86-asm-instruction
:name "SMSW"
:operands "reg32"
:code-string "[m: o32 0f 01 /4]"
:arch-flags (list "386")))

(setf STC-void (make-instance 'x86-asm-instruction
:name "STC"
:operands "void"
:code-string "[ f9]"
:arch-flags (list "8086")))

(setf STD-void (make-instance 'x86-asm-instruction
:name "STD"
:operands "void"
:code-string "[ fd]"
:arch-flags (list "8086")))

(setf STI-void (make-instance 'x86-asm-instruction
:name "STI"
:operands "void"
:code-string "[ fb]"
:arch-flags (list "8086")))

(setf STOSB-void (make-instance 'x86-asm-instruction
:name "STOSB"
:operands "void"
:code-string "[ aa]"
:arch-flags (list "8086")))

(setf STOSD-void (make-instance 'x86-asm-instruction
:name "STOSD"
:operands "void"
:code-string "[ o32 ab]"
:arch-flags (list "386")))

(setf STOSQ-void (make-instance 'x86-asm-instruction
:name "STOSQ"
:operands "void"
:code-string "[ o64 ab]"
:arch-flags (list "X64")))

(setf STOSW-void (make-instance 'x86-asm-instruction
:name "STOSW"
:operands "void"
:code-string "[ o16 ab]"
:arch-flags (list "8086")))

(setf STR-mem (make-instance 'x86-asm-instruction
:name "STR"
:operands "mem"
:code-string "[m: 0f 00 /1]"
:arch-flags (list "286" "PROT")))

(setf STR-mem16 (make-instance 'x86-asm-instruction
:name "STR"
:operands "mem16"
:code-string "[m: 0f 00 /1]"
:arch-flags (list "286" "PROT")))

(setf STR-reg16 (make-instance 'x86-asm-instruction
:name "STR"
:operands "reg16"
:code-string "[m: o16 0f 00 /1]"
:arch-flags (list "286" "PROT")))

(setf STR-reg32 (make-instance 'x86-asm-instruction
:name "STR"
:operands "reg32"
:code-string "[m: o32 0f 00 /1]"
:arch-flags (list "386" "PROT")))

(setf STR-reg64 (make-instance 'x86-asm-instruction
:name "STR"
:operands "reg64"
:code-string "[m: o64 0f 00 /1]"
:arch-flags (list "X64")))

(setf SUB-mem.reg8 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "mem,reg8"
:code-string "[mr: hle 28 /r]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf SUB-reg8.reg8 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "reg8,reg8"
:code-string "[mr: 28 /r]"
:arch-flags (list "8086")))

(setf SUB-mem.reg16 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "mem,reg16"
:code-string "[mr: hle o16 29 /r]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf SUB-reg16.reg16 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "reg16,reg16"
:code-string "[mr: o16 29 /r]"
:arch-flags (list "8086")))

(setf SUB-mem.reg32 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "mem,reg32"
:code-string "[mr: hle o32 29 /r]"
:arch-flags (list "386" "SM" "LOCK")))

(setf SUB-reg32.reg32 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "reg32,reg32"
:code-string "[mr: o32 29 /r]"
:arch-flags (list "386")))

(setf SUB-mem.reg64 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "mem,reg64"
:code-string "[mr: hle o64 29 /r]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf SUB-reg64.reg64 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "reg64,reg64"
:code-string "[mr: o64 29 /r]"
:arch-flags (list "X64")))

(setf SUB-reg8.mem (make-instance 'x86-asm-instruction
:name "SUB"
:operands "reg8,mem"
:code-string "[rm: 2a /r]"
:arch-flags (list "8086" "SM")))

(setf SUB-reg8.reg8 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "reg8,reg8"
:code-string "[rm: 2a /r]"
:arch-flags (list "8086")))

(setf SUB-reg16.mem (make-instance 'x86-asm-instruction
:name "SUB"
:operands "reg16,mem"
:code-string "[rm: o16 2b /r]"
:arch-flags (list "8086" "SM")))

(setf SUB-reg16.reg16 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "reg16,reg16"
:code-string "[rm: o16 2b /r]"
:arch-flags (list "8086")))

(setf SUB-reg32.mem (make-instance 'x86-asm-instruction
:name "SUB"
:operands "reg32,mem"
:code-string "[rm: o32 2b /r]"
:arch-flags (list "386" "SM")))

(setf SUB-reg32.reg32 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "reg32,reg32"
:code-string "[rm: o32 2b /r]"
:arch-flags (list "386")))

(setf SUB-reg64.mem (make-instance 'x86-asm-instruction
:name "SUB"
:operands "reg64,mem"
:code-string "[rm: o64 2b /r]"
:arch-flags (list "X64" "SM")))

(setf SUB-reg64.reg64 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "reg64,reg64"
:code-string "[rm: o64 2b /r]"
:arch-flags (list "X64")))

(setf SUB-rm16.imm8 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "rm16,imm8"
:code-string "[mi: hle o16 83 /5 ib,s]"
:arch-flags (list "8086" "LOCK")))

(setf SUB-rm32.imm8 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "rm32,imm8"
:code-string "[mi: hle o32 83 /5 ib,s]"
:arch-flags (list "386" "LOCK")))

(setf SUB-rm64.imm8 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "rm64,imm8"
:code-string "[mi: hle o64 83 /5 ib,s]"
:arch-flags (list "X64" "LOCK")))

(setf SUB-reg_al.imm (make-instance 'x86-asm-instruction
:name "SUB"
:operands "reg_al,imm"
:code-string "[-i: 2c ib]"
:arch-flags (list "8086" "SM")))

(setf SUB-reg_ax.sbyteword (make-instance 'x86-asm-instruction
:name "SUB"
:operands "reg_ax,sbyteword"
:code-string "[mi: o16 83 /5 ib,s]"
:arch-flags (list "8086" "SM" "ND")))

(setf SUB-reg_ax.imm (make-instance 'x86-asm-instruction
:name "SUB"
:operands "reg_ax,imm"
:code-string "[-i: o16 2d iw]"
:arch-flags (list "8086" "SM")))

(setf SUB-reg_eax.sbytedword (make-instance 'x86-asm-instruction
:name "SUB"
:operands "reg_eax,sbytedword"
:code-string "[mi: o32 83 /5 ib,s]"
:arch-flags (list "386" "SM" "ND")))

(setf SUB-reg_eax.imm (make-instance 'x86-asm-instruction
:name "SUB"
:operands "reg_eax,imm"
:code-string "[-i: o32 2d id]"
:arch-flags (list "386" "SM")))

(setf SUB-reg_rax.sbytedword (make-instance 'x86-asm-instruction
:name "SUB"
:operands "reg_rax,sbytedword"
:code-string "[mi: o64 83 /5 ib,s]"
:arch-flags (list "X64" "SM" "ND")))

(setf SUB-reg_rax.imm (make-instance 'x86-asm-instruction
:name "SUB"
:operands "reg_rax,imm"
:code-string "[-i: o64 2d id,s]"
:arch-flags (list "X64" "SM")))

(setf SUB-rm8.imm (make-instance 'x86-asm-instruction
:name "SUB"
:operands "rm8,imm"
:code-string "[mi: hle 80 /5 ib]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf SUB-rm16.sbyteword (make-instance 'x86-asm-instruction
:name "SUB"
:operands "rm16,sbyteword"
:code-string "[mi: hle o16 83 /5 ib,s]"
:arch-flags (list "8086" "SM" "LOCK" "ND")))

(setf SUB-rm16.imm (make-instance 'x86-asm-instruction
:name "SUB"
:operands "rm16,imm"
:code-string "[mi: hle o16 81 /5 iw]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf SUB-rm32.sbytedword (make-instance 'x86-asm-instruction
:name "SUB"
:operands "rm32,sbytedword"
:code-string "[mi: hle o32 83 /5 ib,s]"
:arch-flags (list "386" "SM" "LOCK" "ND")))

(setf SUB-rm32.imm (make-instance 'x86-asm-instruction
:name "SUB"
:operands "rm32,imm"
:code-string "[mi: hle o32 81 /5 id]"
:arch-flags (list "386" "SM" "LOCK")))

(setf SUB-rm64.sbytedword (make-instance 'x86-asm-instruction
:name "SUB"
:operands "rm64,sbytedword"
:code-string "[mi: hle o64 83 /5 ib,s]"
:arch-flags (list "X64" "SM" "LOCK" "ND")))

(setf SUB-rm64.imm (make-instance 'x86-asm-instruction
:name "SUB"
:operands "rm64,imm"
:code-string "[mi: hle o64 81 /5 id,s]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf SUB-mem.imm8 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "mem,imm8"
:code-string "[mi: hle 80 /5 ib]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf SUB-mem.sbyteword16 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "mem,sbyteword16"
:code-string "[mi: hle o16 83 /5 ib,s]"
:arch-flags (list "8086" "SM" "LOCK" "ND")))

(setf SUB-mem.imm16 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "mem,imm16"
:code-string "[mi: hle o16 81 /5 iw]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf SUB-mem.sbytedword32 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "mem,sbytedword32"
:code-string "[mi: hle o32 83 /5 ib,s]"
:arch-flags (list "386" "SM" "LOCK" "ND")))

(setf SUB-mem.imm32 (make-instance 'x86-asm-instruction
:name "SUB"
:operands "mem,imm32"
:code-string "[mi: hle o32 81 /5 id]"
:arch-flags (list "386" "SM" "LOCK")))

(setf SVDC-mem80.reg_sreg (make-instance 'x86-asm-instruction
:name "SVDC"
:operands "mem80,reg_sreg"
:code-string "[mr: 0f 78 /r]"
:arch-flags (list "486" "CYRIX" "SMM")))

(setf SVLDT-mem80 (make-instance 'x86-asm-instruction
:name "SVLDT"
:operands "mem80"
:code-string "[m: 0f 7a /0]"
:arch-flags (list "486" "CYRIX" "SMM" "ND")))

(setf SVTS-mem80 (make-instance 'x86-asm-instruction
:name "SVTS"
:operands "mem80"
:code-string "[m: 0f 7c /0]"
:arch-flags (list "486" "CYRIX" "SMM")))

(setf SWAPGS-void (make-instance 'x86-asm-instruction
:name "SWAPGS"
:operands "void"
:code-string "[ 0f 01 f8]"
:arch-flags (list "X64")))

(setf SYSCALL-void (make-instance 'x86-asm-instruction
:name "SYSCALL"
:operands "void"
:code-string "[ 0f 05]"
:arch-flags (list "P6" "AMD")))

(setf SYSENTER-void (make-instance 'x86-asm-instruction
:name "SYSENTER"
:operands "void"
:code-string "[ 0f 34]"
:arch-flags (list "P6")))

(setf SYSEXIT-void (make-instance 'x86-asm-instruction
:name "SYSEXIT"
:operands "void"
:code-string "[ 0f 35]"
:arch-flags (list "P6" "PRIV")))

(setf SYSRET-void (make-instance 'x86-asm-instruction
:name "SYSRET"
:operands "void"
:code-string "[ 0f 07]"
:arch-flags (list "P6" "PRIV" "AMD")))

(setf TEST-mem.reg8 (make-instance 'x86-asm-instruction
:name "TEST"
:operands "mem,reg8"
:code-string "[mr: 84 /r]"
:arch-flags (list "8086" "SM")))

(setf TEST-reg8.reg8 (make-instance 'x86-asm-instruction
:name "TEST"
:operands "reg8,reg8"
:code-string "[mr: 84 /r]"
:arch-flags (list "8086")))

(setf TEST-mem.reg16 (make-instance 'x86-asm-instruction
:name "TEST"
:operands "mem,reg16"
:code-string "[mr: o16 85 /r]"
:arch-flags (list "8086" "SM")))

(setf TEST-reg16.reg16 (make-instance 'x86-asm-instruction
:name "TEST"
:operands "reg16,reg16"
:code-string "[mr: o16 85 /r]"
:arch-flags (list "8086")))

(setf TEST-mem.reg32 (make-instance 'x86-asm-instruction
:name "TEST"
:operands "mem,reg32"
:code-string "[mr: o32 85 /r]"
:arch-flags (list "386" "SM")))

(setf TEST-reg32.reg32 (make-instance 'x86-asm-instruction
:name "TEST"
:operands "reg32,reg32"
:code-string "[mr: o32 85 /r]"
:arch-flags (list "386")))

(setf TEST-mem.reg64 (make-instance 'x86-asm-instruction
:name "TEST"
:operands "mem,reg64"
:code-string "[mr: o64 85 /r]"
:arch-flags (list "X64" "SM")))

(setf TEST-reg64.reg64 (make-instance 'x86-asm-instruction
:name "TEST"
:operands "reg64,reg64"
:code-string "[mr: o64 85 /r]"
:arch-flags (list "X64")))

(setf TEST-reg8.mem (make-instance 'x86-asm-instruction
:name "TEST"
:operands "reg8,mem"
:code-string "[rm: 84 /r]"
:arch-flags (list "8086" "SM")))

(setf TEST-reg16.mem (make-instance 'x86-asm-instruction
:name "TEST"
:operands "reg16,mem"
:code-string "[rm: o16 85 /r]"
:arch-flags (list "8086" "SM")))

(setf TEST-reg32.mem (make-instance 'x86-asm-instruction
:name "TEST"
:operands "reg32,mem"
:code-string "[rm: o32 85 /r]"
:arch-flags (list "386" "SM")))

(setf TEST-reg64.mem (make-instance 'x86-asm-instruction
:name "TEST"
:operands "reg64,mem"
:code-string "[rm: o64 85 /r]"
:arch-flags (list "X64" "SM")))

(setf TEST-reg_al.imm (make-instance 'x86-asm-instruction
:name "TEST"
:operands "reg_al,imm"
:code-string "[-i: a8 ib]"
:arch-flags (list "8086" "SM")))

(setf TEST-reg_ax.imm (make-instance 'x86-asm-instruction
:name "TEST"
:operands "reg_ax,imm"
:code-string "[-i: o16 a9 iw]"
:arch-flags (list "8086" "SM")))

(setf TEST-reg_eax.imm (make-instance 'x86-asm-instruction
:name "TEST"
:operands "reg_eax,imm"
:code-string "[-i: o32 a9 id]"
:arch-flags (list "386" "SM")))

(setf TEST-reg_rax.imm (make-instance 'x86-asm-instruction
:name "TEST"
:operands "reg_rax,imm"
:code-string "[-i: o64 a9 id,s]"
:arch-flags (list "X64" "SM")))

(setf TEST-rm8.imm (make-instance 'x86-asm-instruction
:name "TEST"
:operands "rm8,imm"
:code-string "[mi: f6 /0 ib]"
:arch-flags (list "8086" "SM")))

(setf TEST-rm16.imm (make-instance 'x86-asm-instruction
:name "TEST"
:operands "rm16,imm"
:code-string "[mi: o16 f7 /0 iw]"
:arch-flags (list "8086" "SM")))

(setf TEST-rm32.imm (make-instance 'x86-asm-instruction
:name "TEST"
:operands "rm32,imm"
:code-string "[mi: o32 f7 /0 id]"
:arch-flags (list "386" "SM")))

(setf TEST-rm64.imm (make-instance 'x86-asm-instruction
:name "TEST"
:operands "rm64,imm"
:code-string "[mi: o64 f7 /0 id,s]"
:arch-flags (list "X64" "SM")))

(setf TEST-mem.imm8 (make-instance 'x86-asm-instruction
:name "TEST"
:operands "mem,imm8"
:code-string "[mi: f6 /0 ib]"
:arch-flags (list "8086" "SM")))

(setf TEST-mem.imm16 (make-instance 'x86-asm-instruction
:name "TEST"
:operands "mem,imm16"
:code-string "[mi: o16 f7 /0 iw]"
:arch-flags (list "8086" "SM")))

(setf TEST-mem.imm32 (make-instance 'x86-asm-instruction
:name "TEST"
:operands "mem,imm32"
:code-string "[mi: o32 f7 /0 id]"
:arch-flags (list "386" "SM")))

(setf UD0-void (make-instance 'x86-asm-instruction
:name "UD0"
:operands "void"
:code-string "[ 0f ff]"
:arch-flags (list "186" "UNDOC")))

(setf UD1-void (make-instance 'x86-asm-instruction
:name "UD1"
:operands "void"
:code-string "[ 0f b9]"
:arch-flags (list "186" "UNDOC")))

(setf UD2B-void (make-instance 'x86-asm-instruction
:name "UD2B"
:operands "void"
:code-string "[ 0f b9]"
:arch-flags (list "186" "UNDOC" "ND")))

(setf UD2-void (make-instance 'x86-asm-instruction
:name "UD2"
:operands "void"
:code-string "[ 0f 0b]"
:arch-flags (list "186")))

(setf UD2A-void (make-instance 'x86-asm-instruction
:name "UD2A"
:operands "void"
:code-string "[ 0f 0b]"
:arch-flags (list "186" "ND")))

(setf UMOV-mem.reg8 (make-instance 'x86-asm-instruction
:name "UMOV"
:operands "mem,reg8"
:code-string "[mr: np 0f 10 /r]"
:arch-flags (list "386" "UNDOC" "SM" "ND")))

(setf UMOV-reg8.reg8 (make-instance 'x86-asm-instruction
:name "UMOV"
:operands "reg8,reg8"
:code-string "[mr: np 0f 10 /r]"
:arch-flags (list "386" "UNDOC" "ND")))

(setf UMOV-mem.reg16 (make-instance 'x86-asm-instruction
:name "UMOV"
:operands "mem,reg16"
:code-string "[mr: np o16 0f 11 /r]"
:arch-flags (list "386" "UNDOC" "SM" "ND")))

(setf UMOV-reg16.reg16 (make-instance 'x86-asm-instruction
:name "UMOV"
:operands "reg16,reg16"
:code-string "[mr: np o16 0f 11 /r]"
:arch-flags (list "386" "UNDOC" "ND")))

(setf UMOV-mem.reg32 (make-instance 'x86-asm-instruction
:name "UMOV"
:operands "mem,reg32"
:code-string "[mr: np o32 0f 11 /r]"
:arch-flags (list "386" "UNDOC" "SM" "ND")))

(setf UMOV-reg32.reg32 (make-instance 'x86-asm-instruction
:name "UMOV"
:operands "reg32,reg32"
:code-string "[mr: np o32 0f 11 /r]"
:arch-flags (list "386" "UNDOC" "ND")))

(setf UMOV-reg8.mem (make-instance 'x86-asm-instruction
:name "UMOV"
:operands "reg8,mem"
:code-string "[rm: np 0f 12 /r]"
:arch-flags (list "386" "UNDOC" "SM" "ND")))

(setf UMOV-reg8.reg8 (make-instance 'x86-asm-instruction
:name "UMOV"
:operands "reg8,reg8"
:code-string "[rm: np 0f 12 /r]"
:arch-flags (list "386" "UNDOC" "ND")))

(setf UMOV-reg16.mem (make-instance 'x86-asm-instruction
:name "UMOV"
:operands "reg16,mem"
:code-string "[rm: np o16 0f 13 /r]"
:arch-flags (list "386" "UNDOC" "SM" "ND")))

(setf UMOV-reg16.reg16 (make-instance 'x86-asm-instruction
:name "UMOV"
:operands "reg16,reg16"
:code-string "[rm: np o16 0f 13 /r]"
:arch-flags (list "386" "UNDOC" "ND")))

(setf UMOV-reg32.mem (make-instance 'x86-asm-instruction
:name "UMOV"
:operands "reg32,mem"
:code-string "[rm: np o32 0f 13 /r]"
:arch-flags (list "386" "UNDOC" "SM" "ND")))

(setf UMOV-reg32.reg32 (make-instance 'x86-asm-instruction
:name "UMOV"
:operands "reg32,reg32"
:code-string "[rm: np o32 0f 13 /r]"
:arch-flags (list "386" "UNDOC" "ND")))

(setf VERR-mem (make-instance 'x86-asm-instruction
:name "VERR"
:operands "mem"
:code-string "[m: 0f 00 /4]"
:arch-flags (list "286" "PROT")))

(setf VERR-mem16 (make-instance 'x86-asm-instruction
:name "VERR"
:operands "mem16"
:code-string "[m: 0f 00 /4]"
:arch-flags (list "286" "PROT")))

(setf VERR-reg16 (make-instance 'x86-asm-instruction
:name "VERR"
:operands "reg16"
:code-string "[m: 0f 00 /4]"
:arch-flags (list "286" "PROT")))

(setf VERW-mem (make-instance 'x86-asm-instruction
:name "VERW"
:operands "mem"
:code-string "[m: 0f 00 /5]"
:arch-flags (list "286" "PROT")))

(setf VERW-mem16 (make-instance 'x86-asm-instruction
:name "VERW"
:operands "mem16"
:code-string "[m: 0f 00 /5]"
:arch-flags (list "286" "PROT")))

(setf VERW-reg16 (make-instance 'x86-asm-instruction
:name "VERW"
:operands "reg16"
:code-string "[m: 0f 00 /5]"
:arch-flags (list "286" "PROT")))

(setf FWAIT-void (make-instance 'x86-asm-instruction
:name "FWAIT"
:operands "void"
:code-string "[ wait]"
:arch-flags (list "8086")))

(setf WBINVD-void (make-instance 'x86-asm-instruction
:name "WBINVD"
:operands "void"
:code-string "[ 0f 09]"
:arch-flags (list "486" "PRIV")))

(setf WRSHR-rm32 (make-instance 'x86-asm-instruction
:name "WRSHR"
:operands "rm32"
:code-string "[m: o32 0f 37 /0]"
:arch-flags (list "P6" "CYRIX" "SMM")))

(setf WRMSR-void (make-instance 'x86-asm-instruction
:name "WRMSR"
:operands "void"
:code-string "[ 0f 30]"
:arch-flags (list "PENT" "PRIV")))

(setf XADD-mem.reg8 (make-instance 'x86-asm-instruction
:name "XADD"
:operands "mem,reg8"
:code-string "[mr: hle 0f c0 /r]"
:arch-flags (list "486" "SM" "LOCK")))

(setf XADD-reg8.reg8 (make-instance 'x86-asm-instruction
:name "XADD"
:operands "reg8,reg8"
:code-string "[mr: 0f c0 /r]"
:arch-flags (list "486")))

(setf XADD-mem.reg16 (make-instance 'x86-asm-instruction
:name "XADD"
:operands "mem,reg16"
:code-string "[mr: hle o16 0f c1 /r]"
:arch-flags (list "486" "SM" "LOCK")))

(setf XADD-reg16.reg16 (make-instance 'x86-asm-instruction
:name "XADD"
:operands "reg16,reg16"
:code-string "[mr: o16 0f c1 /r]"
:arch-flags (list "486")))

(setf XADD-mem.reg32 (make-instance 'x86-asm-instruction
:name "XADD"
:operands "mem,reg32"
:code-string "[mr: hle o32 0f c1 /r]"
:arch-flags (list "486" "SM" "LOCK")))

(setf XADD-reg32.reg32 (make-instance 'x86-asm-instruction
:name "XADD"
:operands "reg32,reg32"
:code-string "[mr: o32 0f c1 /r]"
:arch-flags (list "486")))

(setf XADD-mem.reg64 (make-instance 'x86-asm-instruction
:name "XADD"
:operands "mem,reg64"
:code-string "[mr: hle o64 0f c1 /r]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf XADD-reg64.reg64 (make-instance 'x86-asm-instruction
:name "XADD"
:operands "reg64,reg64"
:code-string "[mr: o64 0f c1 /r]"
:arch-flags (list "X64")))

(setf XBTS-reg16.mem (make-instance 'x86-asm-instruction
:name "XBTS"
:operands "reg16,mem"
:code-string "[rm: o16 0f a6 /r]"
:arch-flags (list "386" "SW" "UNDOC" "ND")))

(setf XBTS-reg16.reg16 (make-instance 'x86-asm-instruction
:name "XBTS"
:operands "reg16,reg16"
:code-string "[rm: o16 0f a6 /r]"
:arch-flags (list "386" "UNDOC" "ND")))

(setf XBTS-reg32.mem (make-instance 'x86-asm-instruction
:name "XBTS"
:operands "reg32,mem"
:code-string "[rm: o32 0f a6 /r]"
:arch-flags (list "386" "SD" "UNDOC" "ND")))

(setf XBTS-reg32.reg32 (make-instance 'x86-asm-instruction
:name "XBTS"
:operands "reg32,reg32"
:code-string "[rm: o32 0f a6 /r]"
:arch-flags (list "386" "UNDOC" "ND")))

(setf XCHG-reg_ax.reg16 (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "reg_ax,reg16"
:code-string "[-r: o16 90+r]"
:arch-flags (list "8086")))

(setf XCHG-reg_eax.reg32na (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "reg_eax,reg32na"
:code-string "[-r: o32 90+r]"
:arch-flags (list "386")))

(setf XCHG-reg_rax.reg64 (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "reg_rax,reg64"
:code-string "[-r: o64 90+r]"
:arch-flags (list "X64")))

(setf XCHG-reg16.reg_ax (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "reg16,reg_ax"
:code-string "[r-: o16 90+r]"
:arch-flags (list "8086")))

(setf XCHG-reg32na.reg_eax (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "reg32na,reg_eax"
:code-string "[r-: o32 90+r]"
:arch-flags (list "386")))

(setf XCHG-reg64.reg_rax (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "reg64,reg_rax"
:code-string "[r-: o64 90+r]"
:arch-flags (list "X64")))

(setf XCHG-reg8.mem (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "reg8,mem"
:code-string "[rm: hlenl 86 /r]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf XCHG-reg8.reg8 (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "reg8,reg8"
:code-string "[rm: 86 /r]"
:arch-flags (list "8086")))

(setf XCHG-reg16.mem (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "reg16,mem"
:code-string "[rm: hlenl o16 87 /r]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf XCHG-reg16.reg16 (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "reg16,reg16"
:code-string "[rm: o16 87 /r]"
:arch-flags (list "8086")))

(setf XCHG-reg32.mem (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "reg32,mem"
:code-string "[rm: hlenl o32 87 /r]"
:arch-flags (list "386" "SM" "LOCK")))

(setf XCHG-reg32.reg32 (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "reg32,reg32"
:code-string "[rm: o32 87 /r]"
:arch-flags (list "386")))

(setf XCHG-reg64.mem (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "reg64,mem"
:code-string "[rm: hlenl o64 87 /r]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf XCHG-reg64.reg64 (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "reg64,reg64"
:code-string "[rm: o64 87 /r]"
:arch-flags (list "X64")))

(setf XCHG-mem.reg8 (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "mem,reg8"
:code-string "[mr: hlenl 86 /r]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf XCHG-reg8.reg8 (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "reg8,reg8"
:code-string "[mr: 86 /r]"
:arch-flags (list "8086")))

(setf XCHG-mem.reg16 (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "mem,reg16"
:code-string "[mr: hlenl o16 87 /r]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf XCHG-reg16.reg16 (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "reg16,reg16"
:code-string "[mr: o16 87 /r]"
:arch-flags (list "8086")))

(setf XCHG-mem.reg32 (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "mem,reg32"
:code-string "[mr: hlenl o32 87 /r]"
:arch-flags (list "386" "SM" "LOCK")))

(setf XCHG-reg32.reg32 (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "reg32,reg32"
:code-string "[mr: o32 87 /r]"
:arch-flags (list "386")))

(setf XCHG-mem.reg64 (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "mem,reg64"
:code-string "[mr: hlenl o64 87 /r]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf XCHG-reg64.reg64 (make-instance 'x86-asm-instruction
:name "XCHG"
:operands "reg64,reg64"
:code-string "[mr: o64 87 /r]"
:arch-flags (list "X64")))

(setf XLATB-void (make-instance 'x86-asm-instruction
:name "XLATB"
:operands "void"
:code-string "[ d7]"
:arch-flags (list "8086")))

(setf XLAT-void (make-instance 'x86-asm-instruction
:name "XLAT"
:operands "void"
:code-string "[ d7]"
:arch-flags (list "8086")))

(setf XOR-mem.reg8 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "mem,reg8"
:code-string "[mr: hle 30 /r]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf XOR-reg8.reg8 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "reg8,reg8"
:code-string "[mr: 30 /r]"
:arch-flags (list "8086")))

(setf XOR-mem.reg16 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "mem,reg16"
:code-string "[mr: hle o16 31 /r]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf XOR-reg16.reg16 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "reg16,reg16"
:code-string "[mr: o16 31 /r]"
:arch-flags (list "8086")))

(setf XOR-mem.reg32 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "mem,reg32"
:code-string "[mr: hle o32 31 /r]"
:arch-flags (list "386" "SM" "LOCK")))

(setf XOR-reg32.reg32 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "reg32,reg32"
:code-string "[mr: o32 31 /r]"
:arch-flags (list "386")))

(setf XOR-mem.reg64 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "mem,reg64"
:code-string "[mr: hle o64 31 /r]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf XOR-reg64.reg64 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "reg64,reg64"
:code-string "[mr: o64 31 /r]"
:arch-flags (list "X64")))

(setf XOR-reg8.mem (make-instance 'x86-asm-instruction
:name "XOR"
:operands "reg8,mem"
:code-string "[rm: 32 /r]"
:arch-flags (list "8086" "SM")))

(setf XOR-reg8.reg8 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "reg8,reg8"
:code-string "[rm: 32 /r]"
:arch-flags (list "8086")))

(setf XOR-reg16.mem (make-instance 'x86-asm-instruction
:name "XOR"
:operands "reg16,mem"
:code-string "[rm: o16 33 /r]"
:arch-flags (list "8086" "SM")))

(setf XOR-reg16.reg16 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "reg16,reg16"
:code-string "[rm: o16 33 /r]"
:arch-flags (list "8086")))

(setf XOR-reg32.mem (make-instance 'x86-asm-instruction
:name "XOR"
:operands "reg32,mem"
:code-string "[rm: o32 33 /r]"
:arch-flags (list "386" "SM")))

(setf XOR-reg32.reg32 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "reg32,reg32"
:code-string "[rm: o32 33 /r]"
:arch-flags (list "386")))

(setf XOR-reg64.mem (make-instance 'x86-asm-instruction
:name "XOR"
:operands "reg64,mem"
:code-string "[rm: o64 33 /r]"
:arch-flags (list "X64" "SM")))

(setf XOR-reg64.reg64 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "reg64,reg64"
:code-string "[rm: o64 33 /r]"
:arch-flags (list "X64")))

(setf XOR-rm16.imm8 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "rm16,imm8"
:code-string "[mi: hle o16 83 /6 ib,s]"
:arch-flags (list "8086" "LOCK")))

(setf XOR-rm32.imm8 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "rm32,imm8"
:code-string "[mi: hle o32 83 /6 ib,s]"
:arch-flags (list "386" "LOCK")))

(setf XOR-rm64.imm8 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "rm64,imm8"
:code-string "[mi: hle o64 83 /6 ib,s]"
:arch-flags (list "X64" "LOCK")))

(setf XOR-reg_al.imm (make-instance 'x86-asm-instruction
:name "XOR"
:operands "reg_al,imm"
:code-string "[-i: 34 ib]"
:arch-flags (list "8086" "SM")))

(setf XOR-reg_ax.sbyteword (make-instance 'x86-asm-instruction
:name "XOR"
:operands "reg_ax,sbyteword"
:code-string "[mi: o16 83 /6 ib,s]"
:arch-flags (list "8086" "SM" "ND")))

(setf XOR-reg_ax.imm (make-instance 'x86-asm-instruction
:name "XOR"
:operands "reg_ax,imm"
:code-string "[-i: o16 35 iw]"
:arch-flags (list "8086" "SM")))

(setf XOR-reg_eax.sbytedword (make-instance 'x86-asm-instruction
:name "XOR"
:operands "reg_eax,sbytedword"
:code-string "[mi: o32 83 /6 ib,s]"
:arch-flags (list "386" "SM" "ND")))

(setf XOR-reg_eax.imm (make-instance 'x86-asm-instruction
:name "XOR"
:operands "reg_eax,imm"
:code-string "[-i: o32 35 id]"
:arch-flags (list "386" "SM")))

(setf XOR-reg_rax.sbytedword (make-instance 'x86-asm-instruction
:name "XOR"
:operands "reg_rax,sbytedword"
:code-string "[mi: o64 83 /6 ib,s]"
:arch-flags (list "X64" "SM" "ND")))

(setf XOR-reg_rax.imm (make-instance 'x86-asm-instruction
:name "XOR"
:operands "reg_rax,imm"
:code-string "[-i: o64 35 id,s]"
:arch-flags (list "X64" "SM")))

(setf XOR-rm8.imm (make-instance 'x86-asm-instruction
:name "XOR"
:operands "rm8,imm"
:code-string "[mi: hle 80 /6 ib]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf XOR-rm16.sbyteword (make-instance 'x86-asm-instruction
:name "XOR"
:operands "rm16,sbyteword"
:code-string "[mi: hle o16 83 /6 ib,s]"
:arch-flags (list "8086" "SM" "LOCK" "ND")))

(setf XOR-rm16.imm (make-instance 'x86-asm-instruction
:name "XOR"
:operands "rm16,imm"
:code-string "[mi: hle o16 81 /6 iw]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf XOR-rm32.sbytedword (make-instance 'x86-asm-instruction
:name "XOR"
:operands "rm32,sbytedword"
:code-string "[mi: hle o32 83 /6 ib,s]"
:arch-flags (list "386" "SM" "LOCK" "ND")))

(setf XOR-rm32.imm (make-instance 'x86-asm-instruction
:name "XOR"
:operands "rm32,imm"
:code-string "[mi: hle o32 81 /6 id]"
:arch-flags (list "386" "SM" "LOCK")))

(setf XOR-rm64.sbytedword (make-instance 'x86-asm-instruction
:name "XOR"
:operands "rm64,sbytedword"
:code-string "[mi: hle o64 83 /6 ib,s]"
:arch-flags (list "X64" "SM" "LOCK" "ND")))

(setf XOR-rm64.imm (make-instance 'x86-asm-instruction
:name "XOR"
:operands "rm64,imm"
:code-string "[mi: hle o64 81 /6 id,s]"
:arch-flags (list "X64" "SM" "LOCK")))

(setf XOR-mem.imm8 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "mem,imm8"
:code-string "[mi: hle 80 /6 ib]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf XOR-mem.sbyteword16 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "mem,sbyteword16"
:code-string "[mi: hle o16 83 /6 ib,s]"
:arch-flags (list "8086" "SM" "LOCK" "ND")))

(setf XOR-mem.imm16 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "mem,imm16"
:code-string "[mi: hle o16 81 /6 iw]"
:arch-flags (list "8086" "SM" "LOCK")))

(setf XOR-mem.sbytedword32 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "mem,sbytedword32"
:code-string "[mi: hle o32 83 /6 ib,s]"
:arch-flags (list "386" "SM" "LOCK" "ND")))

(setf XOR-mem.imm32 (make-instance 'x86-asm-instruction
:name "XOR"
:operands "mem,imm32"
:code-string "[mi: hle o32 81 /6 id]"
:arch-flags (list "386" "SM" "LOCK")))

(setf CMOVcc-reg16.mem (make-instance 'x86-asm-instruction
:name "CMOVcc"
:operands "reg16,mem"
:code-string "[rm: o16 0f 40+c /r]"
:arch-flags (list "P6" "SM")))

(setf CMOVcc-reg16.reg16 (make-instance 'x86-asm-instruction
:name "CMOVcc"
:operands "reg16,reg16"
:code-string "[rm: o16 0f 40+c /r]"
:arch-flags (list "P6")))

(setf CMOVcc-reg32.mem (make-instance 'x86-asm-instruction
:name "CMOVcc"
:operands "reg32,mem"
:code-string "[rm: o32 0f 40+c /r]"
:arch-flags (list "P6" "SM")))

(setf CMOVcc-reg32.reg32 (make-instance 'x86-asm-instruction
:name "CMOVcc"
:operands "reg32,reg32"
:code-string "[rm: o32 0f 40+c /r]"
:arch-flags (list "P6")))

(setf CMOVcc-reg64.mem (make-instance 'x86-asm-instruction
:name "CMOVcc"
:operands "reg64,mem"
:code-string "[rm: o64 0f 40+c /r]"
:arch-flags (list "X64" "SM")))

(setf CMOVcc-reg64.reg64 (make-instance 'x86-asm-instruction
:name "CMOVcc"
:operands "reg64,reg64"
:code-string "[rm: o64 0f 40+c /r]"
:arch-flags (list "X64")))

(setf Jcc-imm-near (make-instance 'x86-asm-instruction
:name "Jcc"
:operands "imm|near"
:code-string "[i: odf 0f 80+c rel]"
:arch-flags (list "386" "BND")))

(setf Jcc-imm64-near (make-instance 'x86-asm-instruction
:name "Jcc"
:operands "imm64|near"
:code-string "[i: o64nw 0f 80+c rel]"
:arch-flags (list "X64" "BND")))

(setf Jcc-imm-short (make-instance 'x86-asm-instruction
:name "Jcc"
:operands "imm|short"
:code-string "[i: 70+c rel8]"
:arch-flags (list "8086" "ND" "BND")))

(setf Jcc-imm (make-instance 'x86-asm-instruction
:name "Jcc"
:operands "imm"
:code-string "[i: jcc8 70+c rel8]"
:arch-flags (list "8086" "ND" "BND")))

(setf Jcc-imm (make-instance 'x86-asm-instruction
:name "Jcc"
:operands "imm"
:code-string "[i: 0f 80+c rel]"
:arch-flags (list "386" "ND" "BND")))

(setf Jcc-imm (make-instance 'x86-asm-instruction
:name "Jcc"
:operands "imm"
:code-string "[i: 71+c jlen e9 rel]"
:arch-flags (list "8086" "ND" "BND")))

(setf Jcc-imm (make-instance 'x86-asm-instruction
:name "Jcc"
:operands "imm"
:code-string "[i: 70+c rel8]"
:arch-flags (list "8086" "BND")))

(setf SETcc-mem (make-instance 'x86-asm-instruction
:name "SETcc"
:operands "mem"
:code-string "[m: 0f 90+c /0]"
:arch-flags (list "386" "SB")))

(setf SETcc-reg8 (make-instance 'x86-asm-instruction
:name "SETcc"
:operands "reg8"
:code-string "[m: 0f 90+c /0]"
:arch-flags (list "386")))

(setf ADDPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "ADDPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f 58 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf ADDSS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "ADDSS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: f3 0f 58 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf ANDNPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "ANDNPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f 55 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf ANDPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "ANDPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f 54 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf CMPEQPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "CMPEQPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f c2 /r 00]"
:arch-flags (list "KATMAI" "SSE")))

(setf CMPEQSS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "CMPEQSS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: f3 0f c2 /r 00]"
:arch-flags (list "KATMAI" "SSE")))

(setf CMPLEPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "CMPLEPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f c2 /r 02]"
:arch-flags (list "KATMAI" "SSE")))

(setf CMPLESS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "CMPLESS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: f3 0f c2 /r 02]"
:arch-flags (list "KATMAI" "SSE")))

(setf CMPLTPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "CMPLTPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f c2 /r 01]"
:arch-flags (list "KATMAI" "SSE")))

(setf CMPLTSS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "CMPLTSS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: f3 0f c2 /r 01]"
:arch-flags (list "KATMAI" "SSE")))

(setf CMPNEQPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "CMPNEQPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f c2 /r 04]"
:arch-flags (list "KATMAI" "SSE")))

(setf CMPNEQSS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "CMPNEQSS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: f3 0f c2 /r 04]"
:arch-flags (list "KATMAI" "SSE")))

(setf CMPNLEPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "CMPNLEPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f c2 /r 06]"
:arch-flags (list "KATMAI" "SSE")))

(setf CMPNLESS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "CMPNLESS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: f3 0f c2 /r 06]"
:arch-flags (list "KATMAI" "SSE")))

(setf CMPNLTPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "CMPNLTPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f c2 /r 05]"
:arch-flags (list "KATMAI" "SSE")))

(setf CMPNLTSS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "CMPNLTSS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: f3 0f c2 /r 05]"
:arch-flags (list "KATMAI" "SSE")))

(setf CMPORDPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "CMPORDPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f c2 /r 07]"
:arch-flags (list "KATMAI" "SSE")))

(setf CMPORDSS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "CMPORDSS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: f3 0f c2 /r 07]"
:arch-flags (list "KATMAI" "SSE")))

(setf CMPUNORDPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "CMPUNORDPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f c2 /r 03]"
:arch-flags (list "KATMAI" "SSE")))

(setf CMPUNORDSS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "CMPUNORDSS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: f3 0f c2 /r 03]"
:arch-flags (list "KATMAI" "SSE")))

(setf CMPPS-xmmreg.mem.imm (make-instance 'x86-asm-instruction
:name "CMPPS"
:operands "xmmreg,mem,imm"
:code-string "[rmi: np 0f c2 /r ib,u]"
:arch-flags (list "KATMAI" "SSE" "SB" "AR2")))

(setf CMPPS-xmmreg.xmmreg.imm (make-instance 'x86-asm-instruction
:name "CMPPS"
:operands "xmmreg,xmmreg,imm"
:code-string "[rmi: np 0f c2 /r ib,u]"
:arch-flags (list "KATMAI" "SSE" "SB" "AR2")))

(setf CMPSS-xmmreg.mem.imm (make-instance 'x86-asm-instruction
:name "CMPSS"
:operands "xmmreg,mem,imm"
:code-string "[rmi: f3 0f c2 /r ib,u]"
:arch-flags (list "KATMAI" "SSE" "SB" "AR2")))

(setf CMPSS-xmmreg.xmmreg.imm (make-instance 'x86-asm-instruction
:name "CMPSS"
:operands "xmmreg,xmmreg,imm"
:code-string "[rmi: f3 0f c2 /r ib,u]"
:arch-flags (list "KATMAI" "SSE" "SB" "AR2")))

(setf COMISS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "COMISS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: np 0f 2f /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf CVTPI2PS-xmmreg.mmxrm64 (make-instance 'x86-asm-instruction
:name "CVTPI2PS"
:operands "xmmreg,mmxrm64"
:code-string "[rm: np 0f 2a /r]"
:arch-flags (list "KATMAI" "SSE" "MMX")))

(setf CVTPS2PI-mmxreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "CVTPS2PI"
:operands "mmxreg,xmmrm64"
:code-string "[rm: np 0f 2d /r]"
:arch-flags (list "KATMAI" "SSE" "MMX")))

(setf CVTSI2SS-xmmreg.mem (make-instance 'x86-asm-instruction
:name "CVTSI2SS"
:operands "xmmreg,mem"
:code-string "[rm: f3 0f 2a /r]"
:arch-flags (list "KATMAI" "SSE" "SD" "AR1" "ND")))

(setf CVTSI2SS-xmmreg.rm32 (make-instance 'x86-asm-instruction
:name "CVTSI2SS"
:operands "xmmreg,rm32"
:code-string "[rm: f3 0f 2a /r]"
:arch-flags (list "KATMAI" "SSE" "SD" "AR1")))

(setf CVTSI2SS-xmmreg.rm64 (make-instance 'x86-asm-instruction
:name "CVTSI2SS"
:operands "xmmreg,rm64"
:code-string "[rm: o64 f3 0f 2a /r]"
:arch-flags (list "X64" "SSE" "SQ" "AR1")))

(setf CVTSS2SI-reg32.xmmreg (make-instance 'x86-asm-instruction
:name "CVTSS2SI"
:operands "reg32,xmmreg"
:code-string "[rm: f3 0f 2d /r]"
:arch-flags (list "KATMAI" "SSE" "SD" "AR1")))

(setf CVTSS2SI-reg32.mem (make-instance 'x86-asm-instruction
:name "CVTSS2SI"
:operands "reg32,mem"
:code-string "[rm: f3 0f 2d /r]"
:arch-flags (list "KATMAI" "SSE" "SD" "AR1")))

(setf CVTSS2SI-reg64.xmmreg (make-instance 'x86-asm-instruction
:name "CVTSS2SI"
:operands "reg64,xmmreg"
:code-string "[rm: o64 f3 0f 2d /r]"
:arch-flags (list "X64" "SSE" "SD" "AR1")))

(setf CVTSS2SI-reg64.mem (make-instance 'x86-asm-instruction
:name "CVTSS2SI"
:operands "reg64,mem"
:code-string "[rm: o64 f3 0f 2d /r]"
:arch-flags (list "X64" "SSE" "SD" "AR1")))

(setf CVTTPS2PI-mmxreg.xmmrm (make-instance 'x86-asm-instruction
:name "CVTTPS2PI"
:operands "mmxreg,xmmrm"
:code-string "[rm: np 0f 2c /r]"
:arch-flags (list "KATMAI" "SSE" "MMX" "SQ")))

(setf CVTTSS2SI-reg32.xmmrm (make-instance 'x86-asm-instruction
:name "CVTTSS2SI"
:operands "reg32,xmmrm"
:code-string "[rm: f3 0f 2c /r]"
:arch-flags (list "KATMAI" "SSE" "SD" "AR1")))

(setf CVTTSS2SI-reg64.xmmrm (make-instance 'x86-asm-instruction
:name "CVTTSS2SI"
:operands "reg64,xmmrm"
:code-string "[rm: o64 f3 0f 2c /r]"
:arch-flags (list "X64" "SSE" "SD" "AR1")))

(setf DIVPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "DIVPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f 5e /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf DIVSS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "DIVSS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: f3 0f 5e /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf LDMXCSR-mem32 (make-instance 'x86-asm-instruction
:name "LDMXCSR"
:operands "mem32"
:code-string "[m: np 0f ae /2]"
:arch-flags (list "KATMAI" "SSE")))

(setf MAXPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "MAXPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f 5f /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MAXSS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "MAXSS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: f3 0f 5f /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MINPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "MINPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f 5d /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MINSS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "MINSS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: f3 0f 5d /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MOVAPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "MOVAPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f 28 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MOVAPS-xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "MOVAPS"
:operands "xmmrm128,xmmreg"
:code-string "[mr: np 0f 29 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MOVHPS-xmmreg.mem64 (make-instance 'x86-asm-instruction
:name "MOVHPS"
:operands "xmmreg,mem64"
:code-string "[rm: np 0f 16 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MOVHPS-mem64.xmmreg (make-instance 'x86-asm-instruction
:name "MOVHPS"
:operands "mem64,xmmreg"
:code-string "[mr: np 0f 17 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MOVLHPS-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "MOVLHPS"
:operands "xmmreg,xmmreg"
:code-string "[rm: np 0f 16 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MOVLPS-xmmreg.mem64 (make-instance 'x86-asm-instruction
:name "MOVLPS"
:operands "xmmreg,mem64"
:code-string "[rm: np 0f 12 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MOVLPS-mem64.xmmreg (make-instance 'x86-asm-instruction
:name "MOVLPS"
:operands "mem64,xmmreg"
:code-string "[mr: np 0f 13 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MOVHLPS-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "MOVHLPS"
:operands "xmmreg,xmmreg"
:code-string "[rm: np 0f 12 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MOVMSKPS-reg32.xmmreg (make-instance 'x86-asm-instruction
:name "MOVMSKPS"
:operands "reg32,xmmreg"
:code-string "[rm: np 0f 50 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MOVMSKPS-reg64.xmmreg (make-instance 'x86-asm-instruction
:name "MOVMSKPS"
:operands "reg64,xmmreg"
:code-string "[rm: np o64 0f 50 /r]"
:arch-flags (list "X64" "SSE")))

(setf MOVNTPS-mem128.xmmreg (make-instance 'x86-asm-instruction
:name "MOVNTPS"
:operands "mem128,xmmreg"
:code-string "[mr: np 0f 2b /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MOVSS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "MOVSS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: f3 0f 10 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MOVSS-mem32.xmmreg (make-instance 'x86-asm-instruction
:name "MOVSS"
:operands "mem32,xmmreg"
:code-string "[mr: f3 0f 11 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MOVSS-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "MOVSS"
:operands "xmmreg,xmmreg"
:code-string "[rm: f3 0f 10 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MOVUPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "MOVUPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f 10 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MOVUPS-xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "MOVUPS"
:operands "xmmrm128,xmmreg"
:code-string "[mr: np 0f 11 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MULPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "MULPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f 59 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf MULSS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "MULSS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: f3 0f 59 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf ORPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "ORPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f 56 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf RCPPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "RCPPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f 53 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf RCPSS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "RCPSS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: f3 0f 53 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf RSQRTPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "RSQRTPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f 52 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf RSQRTSS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "RSQRTSS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: f3 0f 52 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf SHUFPS-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "SHUFPS"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[rmi: np 0f c6 /r ib,u]"
:arch-flags (list "KATMAI" "SSE")))

(setf SQRTPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "SQRTPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f 51 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf SQRTSS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "SQRTSS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: f3 0f 51 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf STMXCSR-mem32 (make-instance 'x86-asm-instruction
:name "STMXCSR"
:operands "mem32"
:code-string "[m: np 0f ae /3]"
:arch-flags (list "KATMAI" "SSE")))

(setf SUBPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "SUBPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f 5c /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf SUBSS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "SUBSS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: f3 0f 5c /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf UCOMISS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "UCOMISS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: np 0f 2e /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf UNPCKHPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "UNPCKHPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f 15 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf UNPCKLPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "UNPCKLPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f 14 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf XORPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "XORPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: np 0f 57 /r]"
:arch-flags (list "KATMAI" "SSE")))

(setf FXRSTOR-mem (make-instance 'x86-asm-instruction
:name "FXRSTOR"
:operands "mem"
:code-string "[m: np 0f ae /1]"
:arch-flags (list "P6" "SSE" "FPU")))

(setf FXRSTOR64-mem (make-instance 'x86-asm-instruction
:name "FXRSTOR64"
:operands "mem"
:code-string "[m: o64 np 0f ae /1]"
:arch-flags (list "X64" "SSE" "FPU")))

(setf FXSAVE-mem (make-instance 'x86-asm-instruction
:name "FXSAVE"
:operands "mem"
:code-string "[m: np 0f ae /0]"
:arch-flags (list "P6" "SSE" "FPU")))

(setf FXSAVE64-mem (make-instance 'x86-asm-instruction
:name "FXSAVE64"
:operands "mem"
:code-string "[m: o64 np 0f ae /0]"
:arch-flags (list "X64" "SSE" "FPU")))

(setf XGETBV-void (make-instance 'x86-asm-instruction
:name "XGETBV"
:operands "void"
:code-string "[ 0f 01 d0]"
:arch-flags (list "NEHALEM")))

(setf XSETBV-void (make-instance 'x86-asm-instruction
:name "XSETBV"
:operands "void"
:code-string "[ 0f 01 d1]"
:arch-flags (list "NEHALEM" "PRIV")))

(setf XSAVE-mem (make-instance 'x86-asm-instruction
:name "XSAVE"
:operands "mem"
:code-string "[m: np 0f ae /4]"
:arch-flags (list "NEHALEM")))

(setf XSAVE64-mem (make-instance 'x86-asm-instruction
:name "XSAVE64"
:operands "mem"
:code-string "[m: o64 np 0f ae /4]"
:arch-flags (list "LONG" "NEHALEM")))

(setf XSAVEC-mem (make-instance 'x86-asm-instruction
:name "XSAVEC"
:operands "mem"
:code-string "[m: np 0f c7 /4]"
:arch-flags (list "FUTURE")))

(setf XSAVEC64-mem (make-instance 'x86-asm-instruction
:name "XSAVEC64"
:operands "mem"
:code-string "[m: o64 np 0f c7 /4]"
:arch-flags (list "LONG" "FUTURE")))

(setf XSAVEOPT-mem (make-instance 'x86-asm-instruction
:name "XSAVEOPT"
:operands "mem"
:code-string "[m: np 0f ae /6]"
:arch-flags (list "FUTURE")))

(setf XSAVEOPT64-mem (make-instance 'x86-asm-instruction
:name "XSAVEOPT64"
:operands "mem"
:code-string "[m: o64 np 0f ae /6]"
:arch-flags (list "LONG" "FUTURE")))

(setf XSAVES-mem (make-instance 'x86-asm-instruction
:name "XSAVES"
:operands "mem"
:code-string "[m: np 0f c7 /5]"
:arch-flags (list "FUTURE")))

(setf XSAVES64-mem (make-instance 'x86-asm-instruction
:name "XSAVES64"
:operands "mem"
:code-string "[m: o64 np 0f c7 /5]"
:arch-flags (list "LONG" "FUTURE")))

(setf XRSTOR-mem (make-instance 'x86-asm-instruction
:name "XRSTOR"
:operands "mem"
:code-string "[m: np 0f ae /5]"
:arch-flags (list "NEHALEM")))

(setf XRSTOR64-mem (make-instance 'x86-asm-instruction
:name "XRSTOR64"
:operands "mem"
:code-string "[m: o64 np 0f ae /5]"
:arch-flags (list "LONG" "NEHALEM")))

(setf XRSTORS-mem (make-instance 'x86-asm-instruction
:name "XRSTORS"
:operands "mem"
:code-string "[m: np 0f c7 /3]"
:arch-flags (list "FUTURE")))

(setf XRSTORS64-mem (make-instance 'x86-asm-instruction
:name "XRSTORS64"
:operands "mem"
:code-string "[m: o64 np 0f c7 /3]"
:arch-flags (list "LONG" "FUTURE")))

(setf PREFETCHNTA-mem8 (make-instance 'x86-asm-instruction
:name "PREFETCHNTA"
:operands "mem8"
:code-string "[m: 0f 18 /0]"
:arch-flags (list "KATMAI")))

(setf PREFETCHT0-mem8 (make-instance 'x86-asm-instruction
:name "PREFETCHT0"
:operands "mem8"
:code-string "[m: 0f 18 /1]"
:arch-flags (list "KATMAI")))

(setf PREFETCHT1-mem8 (make-instance 'x86-asm-instruction
:name "PREFETCHT1"
:operands "mem8"
:code-string "[m: 0f 18 /2]"
:arch-flags (list "KATMAI")))

(setf PREFETCHT2-mem8 (make-instance 'x86-asm-instruction
:name "PREFETCHT2"
:operands "mem8"
:code-string "[m: 0f 18 /3]"
:arch-flags (list "KATMAI")))

(setf SFENCE-void (make-instance 'x86-asm-instruction
:name "SFENCE"
:operands "void"
:code-string "[ np 0f ae f8]"
:arch-flags (list "KATMAI")))

(setf MASKMOVQ-mmxreg.mmxreg (make-instance 'x86-asm-instruction
:name "MASKMOVQ"
:operands "mmxreg,mmxreg"
:code-string "[rm: np 0f f7 /r]"
:arch-flags (list "KATMAI" "MMX")))

(setf MOVNTQ-mem.mmxreg (make-instance 'x86-asm-instruction
:name "MOVNTQ"
:operands "mem,mmxreg"
:code-string "[mr: np 0f e7 /r]"
:arch-flags (list "KATMAI" "MMX" "SQ")))

(setf PAVGB-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PAVGB"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f e0 /r]"
:arch-flags (list "KATMAI" "MMX" "SQ")))

(setf PAVGW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PAVGW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f e3 /r]"
:arch-flags (list "KATMAI" "MMX" "SQ")))

(setf PEXTRW-reg32.mmxreg.imm (make-instance 'x86-asm-instruction
:name "PEXTRW"
:operands "reg32,mmxreg,imm"
:code-string "[rmi: np 0f c5 /r ib,u]"
:arch-flags (list "KATMAI" "MMX" "SB" "AR2")))

(setf PINSRW-mmxreg.mem.imm (make-instance 'x86-asm-instruction
:name "PINSRW"
:operands "mmxreg,mem,imm"
:code-string "[rmi: np 0f c4 /r ib,u]"
:arch-flags (list "KATMAI" "MMX" "SB" "AR2")))

(setf PINSRW-mmxreg.rm16.imm (make-instance 'x86-asm-instruction
:name "PINSRW"
:operands "mmxreg,rm16,imm"
:code-string "[rmi: np 0f c4 /r ib,u]"
:arch-flags (list "KATMAI" "MMX" "SB" "AR2")))

(setf PINSRW-mmxreg.reg32.imm (make-instance 'x86-asm-instruction
:name "PINSRW"
:operands "mmxreg,reg32,imm"
:code-string "[rmi: np 0f c4 /r ib,u]"
:arch-flags (list "KATMAI" "MMX" "SB" "AR2")))

(setf PMAXSW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PMAXSW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f ee /r]"
:arch-flags (list "KATMAI" "MMX" "SQ")))

(setf PMAXUB-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PMAXUB"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f de /r]"
:arch-flags (list "KATMAI" "MMX" "SQ")))

(setf PMINSW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PMINSW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f ea /r]"
:arch-flags (list "KATMAI" "MMX" "SQ")))

(setf PMINUB-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PMINUB"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f da /r]"
:arch-flags (list "KATMAI" "MMX" "SQ")))

(setf PMOVMSKB-reg32.mmxreg (make-instance 'x86-asm-instruction
:name "PMOVMSKB"
:operands "reg32,mmxreg"
:code-string "[rm: np 0f d7 /r]"
:arch-flags (list "KATMAI" "MMX")))

(setf PMULHUW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PMULHUW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f e4 /r]"
:arch-flags (list "KATMAI" "MMX" "SQ")))

(setf PSADBW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSADBW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f f6 /r]"
:arch-flags (list "KATMAI" "MMX" "SQ")))

(setf PSHUFW-mmxreg.mmxrm.imm (make-instance 'x86-asm-instruction
:name "PSHUFW"
:operands "mmxreg,mmxrm,imm"
:code-string "[rmi: np o64nw 0f 70 /r ib]"
:arch-flags (list "KATMAI" "MMX" "SM2" "SB" "AR2")))

(setf PF2IW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PF2IW"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r 1c]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PFNACC-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PFNACC"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r 8a]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PFPNACC-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PFPNACC"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r 8e]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PI2FW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PI2FW"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r 0c]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf PSWAPD-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSWAPD"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r bb]"
:arch-flags (list "PENT" "3DNOW" "SQ")))

(setf MASKMOVDQU-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "MASKMOVDQU"
:operands "xmmreg,xmmreg"
:code-string "[rm: 66 0f f7 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf CLFLUSH-mem (make-instance 'x86-asm-instruction
:name "CLFLUSH"
:operands "mem"
:code-string "[m: np 0f ae /7]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVNTDQ-mem.xmmreg (make-instance 'x86-asm-instruction
:name "MOVNTDQ"
:operands "mem,xmmreg"
:code-string "[mr: 66 0f e7 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf MOVNTI-mem.reg32 (make-instance 'x86-asm-instruction
:name "MOVNTI"
:operands "mem,reg32"
:code-string "[mr: np 0f c3 /r]"
:arch-flags (list "WILLAMETTE" "SD")))

(setf MOVNTI-mem.reg64 (make-instance 'x86-asm-instruction
:name "MOVNTI"
:operands "mem,reg64"
:code-string "[mr: o64 np 0f c3 /r]"
:arch-flags (list "X64" "SQ")))

(setf MOVNTPD-mem.xmmreg (make-instance 'x86-asm-instruction
:name "MOVNTPD"
:operands "mem,xmmreg"
:code-string "[mr: 66 0f 2b /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf LFENCE-void (make-instance 'x86-asm-instruction
:name "LFENCE"
:operands "void"
:code-string "[ np 0f ae e8]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MFENCE-void (make-instance 'x86-asm-instruction
:name "MFENCE"
:operands "void"
:code-string "[ np 0f ae f0]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVD-mem.xmmreg (make-instance 'x86-asm-instruction
:name "MOVD"
:operands "mem,xmmreg"
:code-string "[mr: 66 norexw 0f 7e /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SD")))

(setf MOVD-xmmreg.mem (make-instance 'x86-asm-instruction
:name "MOVD"
:operands "xmmreg,mem"
:code-string "[rm: 66 norexw 0f 6e /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SD")))

(setf MOVD-xmmreg.rm32 (make-instance 'x86-asm-instruction
:name "MOVD"
:operands "xmmreg,rm32"
:code-string "[rm: 66 norexw 0f 6e /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVD-rm32.xmmreg (make-instance 'x86-asm-instruction
:name "MOVD"
:operands "rm32,xmmreg"
:code-string "[mr: 66 norexw 0f 7e /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVDQA-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "MOVDQA"
:operands "xmmreg,xmmreg"
:code-string "[rm: 66 0f 6f /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVDQA-mem.xmmreg (make-instance 'x86-asm-instruction
:name "MOVDQA"
:operands "mem,xmmreg"
:code-string "[mr: 66 0f 7f /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf MOVDQA-xmmreg.mem (make-instance 'x86-asm-instruction
:name "MOVDQA"
:operands "xmmreg,mem"
:code-string "[rm: 66 0f 6f /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf MOVDQA-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "MOVDQA"
:operands "xmmreg,xmmreg"
:code-string "[mr: 66 0f 7f /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVDQU-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "MOVDQU"
:operands "xmmreg,xmmreg"
:code-string "[rm: f3 0f 6f /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVDQU-mem.xmmreg (make-instance 'x86-asm-instruction
:name "MOVDQU"
:operands "mem,xmmreg"
:code-string "[mr: f3 0f 7f /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf MOVDQU-xmmreg.mem (make-instance 'x86-asm-instruction
:name "MOVDQU"
:operands "xmmreg,mem"
:code-string "[rm: f3 0f 6f /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf MOVDQU-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "MOVDQU"
:operands "xmmreg,xmmreg"
:code-string "[mr: f3 0f 7f /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVDQ2Q-mmxreg.xmmreg (make-instance 'x86-asm-instruction
:name "MOVDQ2Q"
:operands "mmxreg,xmmreg"
:code-string "[rm: f2 0f d6 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVQ-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "MOVQ"
:operands "xmmreg,xmmreg"
:code-string "[rm: f3 0f 7e /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVQ-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "MOVQ"
:operands "xmmreg,xmmreg"
:code-string "[mr: 66 0f d6 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVQ-mem.xmmreg (make-instance 'x86-asm-instruction
:name "MOVQ"
:operands "mem,xmmreg"
:code-string "[mr: 66 0f d6 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SQ")))

(setf MOVQ-xmmreg.mem (make-instance 'x86-asm-instruction
:name "MOVQ"
:operands "xmmreg,mem"
:code-string "[rm: f3 0f 7e /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SQ")))

(setf MOVQ-xmmreg.rm64 (make-instance 'x86-asm-instruction
:name "MOVQ"
:operands "xmmreg,rm64"
:code-string "[rm: 66 o64 0f 6e /r]"
:arch-flags (list "X64" "SSE2")))

(setf MOVQ-rm64.xmmreg (make-instance 'x86-asm-instruction
:name "MOVQ"
:operands "rm64,xmmreg"
:code-string "[mr: 66 o64 0f 7e /r]"
:arch-flags (list "X64" "SSE2")))

(setf MOVQ2DQ-xmmreg.mmxreg (make-instance 'x86-asm-instruction
:name "MOVQ2DQ"
:operands "xmmreg,mmxreg"
:code-string "[rm: f3 0f d6 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf PACKSSWB-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PACKSSWB"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 63 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PACKSSDW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PACKSSDW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 6b /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PACKUSWB-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PACKUSWB"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 67 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PADDB-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PADDB"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f fc /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PADDW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PADDW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f fd /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PADDD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PADDD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f fe /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PADDQ-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PADDQ"
:operands "mmxreg,mmxrm"
:code-string "[rm: np 0f d4 /r]"
:arch-flags (list "WILLAMETTE" "MMX" "SQ")))

(setf PADDQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PADDQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f d4 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PADDSB-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PADDSB"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f ec /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PADDSW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PADDSW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f ed /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PADDUSB-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PADDUSB"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f dc /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PADDUSW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PADDUSW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f dd /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PAND-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PAND"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f db /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PANDN-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PANDN"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f df /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PAVGB-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PAVGB"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f e0 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PAVGW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PAVGW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f e3 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PCMPEQB-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PCMPEQB"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 74 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PCMPEQW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PCMPEQW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 75 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PCMPEQD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PCMPEQD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 76 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PCMPGTB-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PCMPGTB"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 64 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PCMPGTW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PCMPGTW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 65 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PCMPGTD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PCMPGTD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 66 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PEXTRW-reg32.xmmreg.imm (make-instance 'x86-asm-instruction
:name "PEXTRW"
:operands "reg32,xmmreg,imm"
:code-string "[rmi: 66 0f c5 /r ib,u]"
:arch-flags (list "WILLAMETTE" "SSE2" "SB" "AR2")))

(setf PINSRW-xmmreg.reg16.imm (make-instance 'x86-asm-instruction
:name "PINSRW"
:operands "xmmreg,reg16,imm"
:code-string "[rmi: 66 0f c4 /r ib,u]"
:arch-flags (list "WILLAMETTE" "SSE2" "SB" "AR2")))

(setf PINSRW-xmmreg.reg32.imm (make-instance 'x86-asm-instruction
:name "PINSRW"
:operands "xmmreg,reg32,imm"
:code-string "[rmi: 66 0f c4 /r ib,u]"
:arch-flags (list "WILLAMETTE" "SSE2" "SB" "AR2" "ND")))

(setf PINSRW-xmmreg.mem.imm (make-instance 'x86-asm-instruction
:name "PINSRW"
:operands "xmmreg,mem,imm"
:code-string "[rmi: 66 0f c4 /r ib,u]"
:arch-flags (list "WILLAMETTE" "SSE2" "SB" "AR2")))

(setf PINSRW-xmmreg.mem16.imm (make-instance 'x86-asm-instruction
:name "PINSRW"
:operands "xmmreg,mem16,imm"
:code-string "[rmi: 66 0f c4 /r ib,u]"
:arch-flags (list "WILLAMETTE" "SSE2" "SB" "AR2")))

(setf PMADDWD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMADDWD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f f5 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PMAXSW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMAXSW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f ee /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PMAXUB-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMAXUB"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f de /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PMINSW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMINSW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f ea /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PMINUB-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMINUB"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f da /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PMOVMSKB-reg32.xmmreg (make-instance 'x86-asm-instruction
:name "PMOVMSKB"
:operands "reg32,xmmreg"
:code-string "[rm: 66 0f d7 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf PMULHUW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMULHUW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f e4 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PMULHW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMULHW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f e5 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PMULLW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMULLW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f d5 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PMULUDQ-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PMULUDQ"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f f4 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PMULUDQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMULUDQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f f4 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf POR-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "POR"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f eb /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PSADBW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSADBW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f f6 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PSHUFD-xmmreg.xmmreg.imm (make-instance 'x86-asm-instruction
:name "PSHUFD"
:operands "xmmreg,xmmreg,imm"
:code-string "[rmi: 66 0f 70 /r ib]"
:arch-flags (list "WILLAMETTE" "SSE2" "SB" "AR2")))

(setf PSHUFD-xmmreg.mem.imm (make-instance 'x86-asm-instruction
:name "PSHUFD"
:operands "xmmreg,mem,imm"
:code-string "[rmi: 66 0f 70 /r ib]"
:arch-flags (list "WILLAMETTE" "SSE2" "SM2" "SB" "AR2")))

(setf PSHUFHW-xmmreg.xmmreg.imm (make-instance 'x86-asm-instruction
:name "PSHUFHW"
:operands "xmmreg,xmmreg,imm"
:code-string "[rmi: f3 0f 70 /r ib]"
:arch-flags (list "WILLAMETTE" "SSE2" "SB" "AR2")))

(setf PSHUFHW-xmmreg.mem.imm (make-instance 'x86-asm-instruction
:name "PSHUFHW"
:operands "xmmreg,mem,imm"
:code-string "[rmi: f3 0f 70 /r ib]"
:arch-flags (list "WILLAMETTE" "SSE2" "SM2" "SB" "AR2")))

(setf PSHUFLW-xmmreg.xmmreg.imm (make-instance 'x86-asm-instruction
:name "PSHUFLW"
:operands "xmmreg,xmmreg,imm"
:code-string "[rmi: f2 0f 70 /r ib]"
:arch-flags (list "WILLAMETTE" "SSE2" "SB" "AR2")))

(setf PSHUFLW-xmmreg.mem.imm (make-instance 'x86-asm-instruction
:name "PSHUFLW"
:operands "xmmreg,mem,imm"
:code-string "[rmi: f2 0f 70 /r ib]"
:arch-flags (list "WILLAMETTE" "SSE2" "SM2" "SB" "AR2")))

(setf PSLLDQ-xmmreg.imm (make-instance 'x86-asm-instruction
:name "PSLLDQ"
:operands "xmmreg,imm"
:code-string "[mi: 66 0f 73 /7 ib,u]"
:arch-flags (list "WILLAMETTE" "SSE2" "SB" "AR1")))

(setf PSLLW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSLLW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f f1 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PSLLW-xmmreg.imm (make-instance 'x86-asm-instruction
:name "PSLLW"
:operands "xmmreg,imm"
:code-string "[mi: 66 0f 71 /6 ib,u]"
:arch-flags (list "WILLAMETTE" "SSE2" "SB" "AR1")))

(setf PSLLD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSLLD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f f2 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PSLLD-xmmreg.imm (make-instance 'x86-asm-instruction
:name "PSLLD"
:operands "xmmreg,imm"
:code-string "[mi: 66 0f 72 /6 ib,u]"
:arch-flags (list "WILLAMETTE" "SSE2" "SB" "AR1")))

(setf PSLLQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSLLQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f f3 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PSLLQ-xmmreg.imm (make-instance 'x86-asm-instruction
:name "PSLLQ"
:operands "xmmreg,imm"
:code-string "[mi: 66 0f 73 /6 ib,u]"
:arch-flags (list "WILLAMETTE" "SSE2" "SB" "AR1")))

(setf PSRAW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSRAW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f e1 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PSRAW-xmmreg.imm (make-instance 'x86-asm-instruction
:name "PSRAW"
:operands "xmmreg,imm"
:code-string "[mi: 66 0f 71 /4 ib,u]"
:arch-flags (list "WILLAMETTE" "SSE2" "SB" "AR1")))

(setf PSRAD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSRAD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f e2 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PSRAD-xmmreg.imm (make-instance 'x86-asm-instruction
:name "PSRAD"
:operands "xmmreg,imm"
:code-string "[mi: 66 0f 72 /4 ib,u]"
:arch-flags (list "WILLAMETTE" "SSE2" "SB" "AR1")))

(setf PSRLDQ-xmmreg.imm (make-instance 'x86-asm-instruction
:name "PSRLDQ"
:operands "xmmreg,imm"
:code-string "[mi: 66 0f 73 /3 ib,u]"
:arch-flags (list "WILLAMETTE" "SSE2" "SB" "AR1")))

(setf PSRLW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSRLW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f d1 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PSRLW-xmmreg.imm (make-instance 'x86-asm-instruction
:name "PSRLW"
:operands "xmmreg,imm"
:code-string "[mi: 66 0f 71 /2 ib,u]"
:arch-flags (list "WILLAMETTE" "SSE2" "SB" "AR1")))

(setf PSRLD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSRLD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f d2 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PSRLD-xmmreg.imm (make-instance 'x86-asm-instruction
:name "PSRLD"
:operands "xmmreg,imm"
:code-string "[mi: 66 0f 72 /2 ib,u]"
:arch-flags (list "WILLAMETTE" "SSE2" "SB" "AR1")))

(setf PSRLQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSRLQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f d3 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PSRLQ-xmmreg.imm (make-instance 'x86-asm-instruction
:name "PSRLQ"
:operands "xmmreg,imm"
:code-string "[mi: 66 0f 73 /2 ib,u]"
:arch-flags (list "WILLAMETTE" "SSE2" "SB" "AR1")))

(setf PSUBB-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSUBB"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f f8 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PSUBW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSUBW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f f9 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PSUBD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSUBD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f fa /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PSUBQ-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSUBQ"
:operands "mmxreg,mmxrm"
:code-string "[rm: np o64nw 0f fb /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PSUBQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSUBQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f fb /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PSUBSB-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSUBSB"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f e8 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PSUBSW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSUBSW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f e9 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PSUBUSB-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSUBUSB"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f d8 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PSUBUSW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSUBUSW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f d9 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PUNPCKHBW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PUNPCKHBW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 68 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PUNPCKHWD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PUNPCKHWD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 69 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PUNPCKHDQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PUNPCKHDQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 6a /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PUNPCKHQDQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PUNPCKHQDQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 6d /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PUNPCKLBW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PUNPCKLBW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 60 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PUNPCKLWD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PUNPCKLWD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 61 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PUNPCKLDQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PUNPCKLDQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 62 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PUNPCKLQDQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PUNPCKLQDQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 6c /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf PXOR-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PXOR"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f ef /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf ADDPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "ADDPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 58 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf ADDSD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "ADDSD"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f 58 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SQ")))

(setf ANDNPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "ANDNPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 55 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf ANDPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "ANDPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 54 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf CMPEQPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CMPEQPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f c2 /r 00]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf CMPEQSD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CMPEQSD"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f c2 /r 00]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf CMPLEPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CMPLEPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f c2 /r 02]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf CMPLESD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CMPLESD"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f c2 /r 02]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf CMPLTPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CMPLTPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f c2 /r 01]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf CMPLTSD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CMPLTSD"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f c2 /r 01]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf CMPNEQPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CMPNEQPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f c2 /r 04]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf CMPNEQSD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CMPNEQSD"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f c2 /r 04]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf CMPNLEPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CMPNLEPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f c2 /r 06]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf CMPNLESD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CMPNLESD"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f c2 /r 06]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf CMPNLTPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CMPNLTPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f c2 /r 05]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf CMPNLTSD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CMPNLTSD"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f c2 /r 05]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf CMPORDPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CMPORDPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f c2 /r 07]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf CMPORDSD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CMPORDSD"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f c2 /r 07]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf CMPUNORDPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CMPUNORDPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f c2 /r 03]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf CMPUNORDSD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CMPUNORDSD"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f c2 /r 03]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf CMPPD-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "CMPPD"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[rmi: 66 0f c2 /r ib,u]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf CMPSD-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "CMPSD"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[rmi: f2 0f c2 /r ib,u]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf COMISD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "COMISD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 2f /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf CVTDQ2PD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CVTDQ2PD"
:operands "xmmreg,xmmrm"
:code-string "[rm: f3 0f e6 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SQ")))

(setf CVTDQ2PS-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CVTDQ2PS"
:operands "xmmreg,xmmrm"
:code-string "[rm: np 0f 5b /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf CVTPD2DQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CVTPD2DQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f e6 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf CVTPD2PI-mmxreg.xmmrm (make-instance 'x86-asm-instruction
:name "CVTPD2PI"
:operands "mmxreg,xmmrm"
:code-string "[rm: 66 0f 2d /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf CVTPD2PS-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CVTPD2PS"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 5a /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf CVTPI2PD-xmmreg.mmxrm (make-instance 'x86-asm-instruction
:name "CVTPI2PD"
:operands "xmmreg,mmxrm"
:code-string "[rm: 66 0f 2a /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SQ")))

(setf CVTPS2DQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CVTPS2DQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 5b /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf CVTPS2PD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CVTPS2PD"
:operands "xmmreg,xmmrm"
:code-string "[rm: np 0f 5a /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SQ")))

(setf CVTSD2SI-reg32.xmmreg (make-instance 'x86-asm-instruction
:name "CVTSD2SI"
:operands "reg32,xmmreg"
:code-string "[rm: norexw f2 0f 2d /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SQ" "AR1")))

(setf CVTSD2SI-reg32.mem (make-instance 'x86-asm-instruction
:name "CVTSD2SI"
:operands "reg32,mem"
:code-string "[rm: norexw f2 0f 2d /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SQ" "AR1")))

(setf CVTSD2SI-reg64.xmmreg (make-instance 'x86-asm-instruction
:name "CVTSD2SI"
:operands "reg64,xmmreg"
:code-string "[rm: o64 f2 0f 2d /r]"
:arch-flags (list "X64" "SSE2" "SQ" "AR1")))

(setf CVTSD2SI-reg64.mem (make-instance 'x86-asm-instruction
:name "CVTSD2SI"
:operands "reg64,mem"
:code-string "[rm: o64 f2 0f 2d /r]"
:arch-flags (list "X64" "SSE2" "SQ" "AR1")))

(setf CVTSD2SS-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CVTSD2SS"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f 5a /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SQ")))

(setf CVTSI2SD-xmmreg.mem (make-instance 'x86-asm-instruction
:name "CVTSI2SD"
:operands "xmmreg,mem"
:code-string "[rm: f2 0f 2a /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SD" "AR1" "ND")))

(setf CVTSI2SD-xmmreg.rm32 (make-instance 'x86-asm-instruction
:name "CVTSI2SD"
:operands "xmmreg,rm32"
:code-string "[rm: norexw f2 0f 2a /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SD" "AR1")))

(setf CVTSI2SD-xmmreg.rm64 (make-instance 'x86-asm-instruction
:name "CVTSI2SD"
:operands "xmmreg,rm64"
:code-string "[rm: o64 f2 0f 2a /r]"
:arch-flags (list "X64" "SSE2" "SQ" "AR1")))

(setf CVTSS2SD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CVTSS2SD"
:operands "xmmreg,xmmrm"
:code-string "[rm: f3 0f 5a /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SD")))

(setf CVTTPD2PI-mmxreg.xmmrm (make-instance 'x86-asm-instruction
:name "CVTTPD2PI"
:operands "mmxreg,xmmrm"
:code-string "[rm: 66 0f 2c /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf CVTTPD2DQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CVTTPD2DQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f e6 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf CVTTPS2DQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "CVTTPS2DQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: f3 0f 5b /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf CVTTSD2SI-reg32.xmmreg (make-instance 'x86-asm-instruction
:name "CVTTSD2SI"
:operands "reg32,xmmreg"
:code-string "[rm: norexw f2 0f 2c /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SQ" "AR1")))

(setf CVTTSD2SI-reg32.mem (make-instance 'x86-asm-instruction
:name "CVTTSD2SI"
:operands "reg32,mem"
:code-string "[rm: norexw f2 0f 2c /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SQ" "AR1")))

(setf CVTTSD2SI-reg64.xmmreg (make-instance 'x86-asm-instruction
:name "CVTTSD2SI"
:operands "reg64,xmmreg"
:code-string "[rm: o64 f2 0f 2c /r]"
:arch-flags (list "X64" "SSE2" "SQ" "AR1")))

(setf CVTTSD2SI-reg64.mem (make-instance 'x86-asm-instruction
:name "CVTTSD2SI"
:operands "reg64,mem"
:code-string "[rm: o64 f2 0f 2c /r]"
:arch-flags (list "X64" "SSE2" "SQ" "AR1")))

(setf DIVPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "DIVPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 5e /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf DIVSD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "DIVSD"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f 5e /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MAXPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "MAXPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 5f /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf MAXSD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "MAXSD"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f 5f /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MINPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "MINPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 5d /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf MINSD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "MINSD"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f 5d /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVAPD-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "MOVAPD"
:operands "xmmreg,xmmreg"
:code-string "[rm: 66 0f 28 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVAPD-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "MOVAPD"
:operands "xmmreg,xmmreg"
:code-string "[mr: 66 0f 29 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVAPD-mem.xmmreg (make-instance 'x86-asm-instruction
:name "MOVAPD"
:operands "mem,xmmreg"
:code-string "[mr: 66 0f 29 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf MOVAPD-xmmreg.mem (make-instance 'x86-asm-instruction
:name "MOVAPD"
:operands "xmmreg,mem"
:code-string "[rm: 66 0f 28 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf MOVHPD-mem.xmmreg (make-instance 'x86-asm-instruction
:name "MOVHPD"
:operands "mem,xmmreg"
:code-string "[mr: 66 0f 17 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVHPD-xmmreg.mem (make-instance 'x86-asm-instruction
:name "MOVHPD"
:operands "xmmreg,mem"
:code-string "[rm: 66 0f 16 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVLPD-mem64.xmmreg (make-instance 'x86-asm-instruction
:name "MOVLPD"
:operands "mem64,xmmreg"
:code-string "[mr: 66 0f 13 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVLPD-xmmreg.mem64 (make-instance 'x86-asm-instruction
:name "MOVLPD"
:operands "xmmreg,mem64"
:code-string "[rm: 66 0f 12 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVMSKPD-reg32.xmmreg (make-instance 'x86-asm-instruction
:name "MOVMSKPD"
:operands "reg32,xmmreg"
:code-string "[rm: 66 0f 50 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVMSKPD-reg64.xmmreg (make-instance 'x86-asm-instruction
:name "MOVMSKPD"
:operands "reg64,xmmreg"
:code-string "[rm: 66 o64 0f 50 /r]"
:arch-flags (list "X64" "SSE2")))

(setf MOVSD-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "MOVSD"
:operands "xmmreg,xmmreg"
:code-string "[rm: f2 0f 10 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVSD-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "MOVSD"
:operands "xmmreg,xmmreg"
:code-string "[mr: f2 0f 11 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVSD-mem64.xmmreg (make-instance 'x86-asm-instruction
:name "MOVSD"
:operands "mem64,xmmreg"
:code-string "[mr: f2 0f 11 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVSD-xmmreg.mem64 (make-instance 'x86-asm-instruction
:name "MOVSD"
:operands "xmmreg,mem64"
:code-string "[rm: f2 0f 10 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVUPD-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "MOVUPD"
:operands "xmmreg,xmmreg"
:code-string "[rm: 66 0f 10 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVUPD-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "MOVUPD"
:operands "xmmreg,xmmreg"
:code-string "[mr: 66 0f 11 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf MOVUPD-mem.xmmreg (make-instance 'x86-asm-instruction
:name "MOVUPD"
:operands "mem,xmmreg"
:code-string "[mr: 66 0f 11 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf MOVUPD-xmmreg.mem (make-instance 'x86-asm-instruction
:name "MOVUPD"
:operands "xmmreg,mem"
:code-string "[rm: 66 0f 10 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf MULPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "MULPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 59 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf MULSD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "MULSD"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f 59 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf ORPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "ORPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 56 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf SHUFPD-xmmreg.xmmreg.imm (make-instance 'x86-asm-instruction
:name "SHUFPD"
:operands "xmmreg,xmmreg,imm"
:code-string "[rmi: 66 0f c6 /r ib,u]"
:arch-flags (list "WILLAMETTE" "SSE2" "SB" "AR2")))

(setf SHUFPD-xmmreg.mem.imm (make-instance 'x86-asm-instruction
:name "SHUFPD"
:operands "xmmreg,mem,imm"
:code-string "[rmi: 66 0f c6 /r ib,u]"
:arch-flags (list "WILLAMETTE" "SSE2" "SM" "SB" "AR2")))

(setf SQRTPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "SQRTPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 51 /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf SQRTSD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "SQRTSD"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f 51 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf SUBPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "SUBPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 5c /r]"
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(setf SUBSD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "SUBSD"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f 5c /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf UCOMISD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "UCOMISD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 2e /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf UNPCKHPD-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "UNPCKHPD"
:operands "xmmreg,xmmrm128"
:code-string "[rm: 66 0f 15 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf UNPCKLPD-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "UNPCKLPD"
:operands "xmmreg,xmmrm128"
:code-string "[rm: 66 0f 14 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf XORPD-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "XORPD"
:operands "xmmreg,xmmrm128"
:code-string "[rm: 66 0f 57 /r]"
:arch-flags (list "WILLAMETTE" "SSE2")))

(setf ADDSUBPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "ADDSUBPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f d0 /r]"
:arch-flags (list "PRESCOTT" "SSE3" "SO")))

(setf ADDSUBPS-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "ADDSUBPS"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f d0 /r]"
:arch-flags (list "PRESCOTT" "SSE3" "SO")))

(setf HADDPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "HADDPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 7c /r]"
:arch-flags (list "PRESCOTT" "SSE3" "SO")))

(setf HADDPS-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "HADDPS"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f 7c /r]"
:arch-flags (list "PRESCOTT" "SSE3" "SO")))

(setf HSUBPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "HSUBPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 7d /r]"
:arch-flags (list "PRESCOTT" "SSE3" "SO")))

(setf HSUBPS-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "HSUBPS"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f 7d /r]"
:arch-flags (list "PRESCOTT" "SSE3" "SO")))

(setf LDDQU-xmmreg.mem (make-instance 'x86-asm-instruction
:name "LDDQU"
:operands "xmmreg,mem"
:code-string "[rm: f2 0f f0 /r]"
:arch-flags (list "PRESCOTT" "SSE3" "SO")))

(setf MOVDDUP-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "MOVDDUP"
:operands "xmmreg,xmmrm"
:code-string "[rm: f2 0f 12 /r]"
:arch-flags (list "PRESCOTT" "SSE3")))

(setf MOVSHDUP-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "MOVSHDUP"
:operands "xmmreg,xmmrm"
:code-string "[rm: f3 0f 16 /r]"
:arch-flags (list "PRESCOTT" "SSE3")))

(setf MOVSLDUP-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "MOVSLDUP"
:operands "xmmreg,xmmrm"
:code-string "[rm: f3 0f 12 /r]"
:arch-flags (list "PRESCOTT" "SSE3")))

(setf CLGI-void (make-instance 'x86-asm-instruction
:name "CLGI"
:operands "void"
:code-string "[ 0f 01 dd]"
:arch-flags (list "VMX" "AMD")))

(setf STGI-void (make-instance 'x86-asm-instruction
:name "STGI"
:operands "void"
:code-string "[ 0f 01 dc]"
:arch-flags (list "VMX" "AMD")))

(setf VMCALL-void (make-instance 'x86-asm-instruction
:name "VMCALL"
:operands "void"
:code-string "[ 0f 01 c1]"
:arch-flags (list "VMX")))

(setf VMCLEAR-mem (make-instance 'x86-asm-instruction
:name "VMCLEAR"
:operands "mem"
:code-string "[m: 66 0f c7 /6]"
:arch-flags (list "VMX")))

(setf VMFUNC-void (make-instance 'x86-asm-instruction
:name "VMFUNC"
:operands "void"
:code-string "[ 0f 01 d4]"
:arch-flags (list "VMX")))

(setf VMLAUNCH-void (make-instance 'x86-asm-instruction
:name "VMLAUNCH"
:operands "void"
:code-string "[ 0f 01 c2]"
:arch-flags (list "VMX")))

(setf VMLOAD-void (make-instance 'x86-asm-instruction
:name "VMLOAD"
:operands "void"
:code-string "[ 0f 01 da]"
:arch-flags (list "VMX" "AMD")))

(setf VMMCALL-void (make-instance 'x86-asm-instruction
:name "VMMCALL"
:operands "void"
:code-string "[ 0f 01 d9]"
:arch-flags (list "VMX" "AMD")))

(setf VMPTRLD-mem (make-instance 'x86-asm-instruction
:name "VMPTRLD"
:operands "mem"
:code-string "[m: np 0f c7 /6]"
:arch-flags (list "VMX")))

(setf VMPTRST-mem (make-instance 'x86-asm-instruction
:name "VMPTRST"
:operands "mem"
:code-string "[m: np 0f c7 /7]"
:arch-flags (list "VMX")))

(setf VMREAD-rm64.reg64 (make-instance 'x86-asm-instruction
:name "VMREAD"
:operands "rm64,reg64"
:code-string "[mr: o64nw np 0f 78 /r]"
:arch-flags (list "X64" "VMX" "SQ")))

(setf VMRESUME-void (make-instance 'x86-asm-instruction
:name "VMRESUME"
:operands "void"
:code-string "[ 0f 01 c3]"
:arch-flags (list "VMX")))

(setf VMRUN-void (make-instance 'x86-asm-instruction
:name "VMRUN"
:operands "void"
:code-string "[ 0f 01 d8]"
:arch-flags (list "VMX" "AMD")))

(setf VMSAVE-void (make-instance 'x86-asm-instruction
:name "VMSAVE"
:operands "void"
:code-string "[ 0f 01 db]"
:arch-flags (list "VMX" "AMD")))

(setf VMWRITE-reg64.rm64 (make-instance 'x86-asm-instruction
:name "VMWRITE"
:operands "reg64,rm64"
:code-string "[rm: o64nw np 0f 79 /r]"
:arch-flags (list "X64" "VMX" "SQ")))

(setf VMXOFF-void (make-instance 'x86-asm-instruction
:name "VMXOFF"
:operands "void"
:code-string "[ 0f 01 c4]"
:arch-flags (list "VMX")))

(setf VMXON-mem (make-instance 'x86-asm-instruction
:name "VMXON"
:operands "mem"
:code-string "[m: f3 0f c7 /6]"
:arch-flags (list "VMX")))

(setf INVEPT-reg64.mem (make-instance 'x86-asm-instruction
:name "INVEPT"
:operands "reg64,mem"
:code-string "[rm: o64nw 66 0f 38 80 /r]"
:arch-flags (list "VMX" "SO" "LONG")))

(setf INVVPID-reg64.mem (make-instance 'x86-asm-instruction
:name "INVVPID"
:operands "reg64,mem"
:code-string "[rm: o64nw 66 0f 38 81 /r]"
:arch-flags (list "VMX" "SO" "LONG")))

(setf PABSB-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PABSB"
:operands "mmxreg,mmxrm"
:code-string "[rm: np 0f 38 1c /r]"
:arch-flags (list "SSSE3" "MMX" "SQ")))

(setf PABSB-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PABSB"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 1c /r]"
:arch-flags (list "SSSE3")))

(setf PABSW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PABSW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np 0f 38 1d /r]"
:arch-flags (list "SSSE3" "MMX" "SQ")))

(setf PABSW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PABSW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 1d /r]"
:arch-flags (list "SSSE3")))

(setf PABSD-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PABSD"
:operands "mmxreg,mmxrm"
:code-string "[rm: np 0f 38 1e /r]"
:arch-flags (list "SSSE3" "MMX" "SQ")))

(setf PABSD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PABSD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 1e /r]"
:arch-flags (list "SSSE3")))

(setf PALIGNR-mmxreg.mmxrm.imm (make-instance 'x86-asm-instruction
:name "PALIGNR"
:operands "mmxreg,mmxrm,imm"
:code-string "[rmi: np 0f 3a 0f /r ib,u]"
:arch-flags (list "SSSE3" "MMX" "SQ")))

(setf PALIGNR-xmmreg.xmmrm.imm (make-instance 'x86-asm-instruction
:name "PALIGNR"
:operands "xmmreg,xmmrm,imm"
:code-string "[rmi: 66 0f 3a 0f /r ib,u]"
:arch-flags (list "SSSE3")))

(setf PHADDW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PHADDW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np 0f 38 01 /r]"
:arch-flags (list "SSSE3" "MMX" "SQ")))

(setf PHADDW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PHADDW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 01 /r]"
:arch-flags (list "SSSE3")))

(setf PHADDD-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PHADDD"
:operands "mmxreg,mmxrm"
:code-string "[rm: np 0f 38 02 /r]"
:arch-flags (list "SSSE3" "MMX" "SQ")))

(setf PHADDD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PHADDD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 02 /r]"
:arch-flags (list "SSSE3")))

(setf PHADDSW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PHADDSW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np 0f 38 03 /r]"
:arch-flags (list "SSSE3" "MMX" "SQ")))

(setf PHADDSW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PHADDSW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 03 /r]"
:arch-flags (list "SSSE3")))

(setf PHSUBW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PHSUBW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np 0f 38 05 /r]"
:arch-flags (list "SSSE3" "MMX" "SQ")))

(setf PHSUBW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PHSUBW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 05 /r]"
:arch-flags (list "SSSE3")))

(setf PHSUBD-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PHSUBD"
:operands "mmxreg,mmxrm"
:code-string "[rm: np 0f 38 06 /r]"
:arch-flags (list "SSSE3" "MMX" "SQ")))

(setf PHSUBD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PHSUBD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 06 /r]"
:arch-flags (list "SSSE3")))

(setf PHSUBSW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PHSUBSW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np 0f 38 07 /r]"
:arch-flags (list "SSSE3" "MMX" "SQ")))

(setf PHSUBSW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PHSUBSW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 07 /r]"
:arch-flags (list "SSSE3")))

(setf PMADDUBSW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PMADDUBSW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np 0f 38 04 /r]"
:arch-flags (list "SSSE3" "MMX" "SQ")))

(setf PMADDUBSW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMADDUBSW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 04 /r]"
:arch-flags (list "SSSE3")))

(setf PMULHRSW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PMULHRSW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np 0f 38 0b /r]"
:arch-flags (list "SSSE3" "MMX" "SQ")))

(setf PMULHRSW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMULHRSW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 0b /r]"
:arch-flags (list "SSSE3")))

(setf PSHUFB-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSHUFB"
:operands "mmxreg,mmxrm"
:code-string "[rm: np 0f 38 00 /r]"
:arch-flags (list "SSSE3" "MMX" "SQ")))

(setf PSHUFB-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSHUFB"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 00 /r]"
:arch-flags (list "SSSE3")))

(setf PSIGNB-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSIGNB"
:operands "mmxreg,mmxrm"
:code-string "[rm: np 0f 38 08 /r]"
:arch-flags (list "SSSE3" "MMX" "SQ")))

(setf PSIGNB-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSIGNB"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 08 /r]"
:arch-flags (list "SSSE3")))

(setf PSIGNW-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSIGNW"
:operands "mmxreg,mmxrm"
:code-string "[rm: np 0f 38 09 /r]"
:arch-flags (list "SSSE3" "MMX" "SQ")))

(setf PSIGNW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSIGNW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 09 /r]"
:arch-flags (list "SSSE3")))

(setf PSIGND-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PSIGND"
:operands "mmxreg,mmxrm"
:code-string "[rm: np 0f 38 0a /r]"
:arch-flags (list "SSSE3" "MMX" "SQ")))

(setf PSIGND-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PSIGND"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 0a /r]"
:arch-flags (list "SSSE3")))

(setf EXTRQ-xmmreg.imm.imm (make-instance 'x86-asm-instruction
:name "EXTRQ"
:operands "xmmreg,imm,imm"
:code-string "[mij: 66 0f 78 /0 ib,u ib,u]"
:arch-flags (list "SSE4A" "AMD")))

(setf EXTRQ-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "EXTRQ"
:operands "xmmreg,xmmreg"
:code-string "[rm: 66 0f 79 /r]"
:arch-flags (list "SSE4A" "AMD")))

(setf INSERTQ-xmmreg.xmmreg.imm.imm (make-instance 'x86-asm-instruction
:name "INSERTQ"
:operands "xmmreg,xmmreg,imm,imm"
:code-string "[rmij: f2 0f 78 /r ib,u ib,u]"
:arch-flags (list "SSE4A" "AMD")))

(setf INSERTQ-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "INSERTQ"
:operands "xmmreg,xmmreg"
:code-string "[rm: f2 0f 79 /r]"
:arch-flags (list "SSE4A" "AMD")))

(setf MOVNTSD-mem.xmmreg (make-instance 'x86-asm-instruction
:name "MOVNTSD"
:operands "mem,xmmreg"
:code-string "[mr: f2 0f 2b /r]"
:arch-flags (list "SSE4A" "AMD" "SQ")))

(setf MOVNTSS-mem.xmmreg (make-instance 'x86-asm-instruction
:name "MOVNTSS"
:operands "mem,xmmreg"
:code-string "[mr: f3 0f 2b /r]"
:arch-flags (list "SSE4A" "AMD" "SD")))

(setf LZCNT-reg16.rm16 (make-instance 'x86-asm-instruction
:name "LZCNT"
:operands "reg16,rm16"
:code-string "[rm: o16 f3i 0f bd /r]"
:arch-flags (list "P6" "AMD")))

(setf LZCNT-reg32.rm32 (make-instance 'x86-asm-instruction
:name "LZCNT"
:operands "reg32,rm32"
:code-string "[rm: o32 f3i 0f bd /r]"
:arch-flags (list "P6" "AMD")))

(setf LZCNT-reg64.rm64 (make-instance 'x86-asm-instruction
:name "LZCNT"
:operands "reg64,rm64"
:code-string "[rm: o64 f3i 0f bd /r]"
:arch-flags (list "X64" "AMD")))

(setf BLENDPD-xmmreg.xmmrm.imm (make-instance 'x86-asm-instruction
:name "BLENDPD"
:operands "xmmreg,xmmrm,imm"
:code-string "[rmi: 66 0f 3a 0d /r ib,u]"
:arch-flags (list "SSE41")))

(setf BLENDPS-xmmreg.xmmrm.imm (make-instance 'x86-asm-instruction
:name "BLENDPS"
:operands "xmmreg,xmmrm,imm"
:code-string "[rmi: 66 0f 3a 0c /r ib,u]"
:arch-flags (list "SSE41")))

(setf BLENDVPD-xmmreg.xmmrm.xmm0 (make-instance 'x86-asm-instruction
:name "BLENDVPD"
:operands "xmmreg,xmmrm,xmm0"
:code-string "[rm-: 66 0f 38 15 /r]"
:arch-flags (list "SSE41")))

(setf BLENDVPD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "BLENDVPD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 15 /r]"
:arch-flags (list "SSE41")))

(setf BLENDVPS-xmmreg.xmmrm.xmm0 (make-instance 'x86-asm-instruction
:name "BLENDVPS"
:operands "xmmreg,xmmrm,xmm0"
:code-string "[rm-: 66 0f 38 14 /r]"
:arch-flags (list "SSE41")))

(setf BLENDVPS-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "BLENDVPS"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 14 /r]"
:arch-flags (list "SSE41")))

(setf DPPD-xmmreg.xmmrm.imm (make-instance 'x86-asm-instruction
:name "DPPD"
:operands "xmmreg,xmmrm,imm"
:code-string "[rmi: 66 0f 3a 41 /r ib,u]"
:arch-flags (list "SSE41")))

(setf DPPS-xmmreg.xmmrm.imm (make-instance 'x86-asm-instruction
:name "DPPS"
:operands "xmmreg,xmmrm,imm"
:code-string "[rmi: 66 0f 3a 40 /r ib,u]"
:arch-flags (list "SSE41")))

(setf EXTRACTPS-rm32.xmmreg.imm (make-instance 'x86-asm-instruction
:name "EXTRACTPS"
:operands "rm32,xmmreg,imm"
:code-string "[mri: 66 0f 3a 17 /r ib,u]"
:arch-flags (list "SSE41")))

(setf EXTRACTPS-reg64.xmmreg.imm (make-instance 'x86-asm-instruction
:name "EXTRACTPS"
:operands "reg64,xmmreg,imm"
:code-string "[mri: o64 66 0f 3a 17 /r ib,u]"
:arch-flags (list "SSE41" "X64")))

(setf INSERTPS-xmmreg.xmmrm.imm (make-instance 'x86-asm-instruction
:name "INSERTPS"
:operands "xmmreg,xmmrm,imm"
:code-string "[rmi: 66 0f 3a 21 /r ib,u]"
:arch-flags (list "SSE41" "SD")))

(setf MOVNTDQA-xmmreg.mem128 (make-instance 'x86-asm-instruction
:name "MOVNTDQA"
:operands "xmmreg,mem128"
:code-string "[rm: 66 0f 38 2a /r]"
:arch-flags (list "SSE41")))

(setf MPSADBW-xmmreg.xmmrm.imm (make-instance 'x86-asm-instruction
:name "MPSADBW"
:operands "xmmreg,xmmrm,imm"
:code-string "[rmi: 66 0f 3a 42 /r ib,u]"
:arch-flags (list "SSE41")))

(setf PACKUSDW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PACKUSDW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 2b /r]"
:arch-flags (list "SSE41")))

(setf PBLENDVB-xmmreg.xmmrm.xmm0 (make-instance 'x86-asm-instruction
:name "PBLENDVB"
:operands "xmmreg,xmmrm,xmm0"
:code-string "[rm-: 66 0f 38 10 /r]"
:arch-flags (list "SSE41")))

(setf PBLENDVB-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PBLENDVB"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 10 /r]"
:arch-flags (list "SSE41")))

(setf PBLENDW-xmmreg.xmmrm.imm (make-instance 'x86-asm-instruction
:name "PBLENDW"
:operands "xmmreg,xmmrm,imm"
:code-string "[rmi: 66 0f 3a 0e /r ib,u]"
:arch-flags (list "SSE41")))

(setf PCMPEQQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PCMPEQQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 29 /r]"
:arch-flags (list "SSE41")))

(setf PEXTRB-reg32.xmmreg.imm (make-instance 'x86-asm-instruction
:name "PEXTRB"
:operands "reg32,xmmreg,imm"
:code-string "[mri: 66 0f 3a 14 /r ib,u]"
:arch-flags (list "SSE41")))

(setf PEXTRB-mem8.xmmreg.imm (make-instance 'x86-asm-instruction
:name "PEXTRB"
:operands "mem8,xmmreg,imm"
:code-string "[mri: 66 0f 3a 14 /r ib,u]"
:arch-flags (list "SSE41")))

(setf PEXTRB-reg64.xmmreg.imm (make-instance 'x86-asm-instruction
:name "PEXTRB"
:operands "reg64,xmmreg,imm"
:code-string "[mri: o64 66 0f 3a 14 /r ib,u]"
:arch-flags (list "SSE41" "X64")))

(setf PEXTRD-rm32.xmmreg.imm (make-instance 'x86-asm-instruction
:name "PEXTRD"
:operands "rm32,xmmreg,imm"
:code-string "[mri: norexw 66 0f 3a 16 /r ib,u]"
:arch-flags (list "SSE41")))

(setf PEXTRQ-rm64.xmmreg.imm (make-instance 'x86-asm-instruction
:name "PEXTRQ"
:operands "rm64,xmmreg,imm"
:code-string "[mri: o64 66 0f 3a 16 /r ib,u]"
:arch-flags (list "SSE41" "X64")))

(setf PEXTRW-reg32.xmmreg.imm (make-instance 'x86-asm-instruction
:name "PEXTRW"
:operands "reg32,xmmreg,imm"
:code-string "[mri: 66 0f 3a 15 /r ib,u]"
:arch-flags (list "SSE41")))

(setf PEXTRW-mem16.xmmreg.imm (make-instance 'x86-asm-instruction
:name "PEXTRW"
:operands "mem16,xmmreg,imm"
:code-string "[mri: 66 0f 3a 15 /r ib,u]"
:arch-flags (list "SSE41")))

(setf PEXTRW-reg64.xmmreg.imm (make-instance 'x86-asm-instruction
:name "PEXTRW"
:operands "reg64,xmmreg,imm"
:code-string "[mri: o64 66 0f 3a 15 /r ib,u]"
:arch-flags (list "SSE41" "X64")))

(setf PHMINPOSUW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PHMINPOSUW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 41 /r]"
:arch-flags (list "SSE41")))

(setf PINSRB-xmmreg.mem.imm (make-instance 'x86-asm-instruction
:name "PINSRB"
:operands "xmmreg,mem,imm"
:code-string "[rmi: 66 0f 3a 20 /r ib,u]"
:arch-flags (list "SSE41" "SB" "AR2")))

(setf PINSRB-xmmreg.rm8.imm (make-instance 'x86-asm-instruction
:name "PINSRB"
:operands "xmmreg,rm8,imm"
:code-string "[rmi: nohi 66 0f 3a 20 /r ib,u]"
:arch-flags (list "SSE41" "SB" "AR2")))

(setf PINSRB-xmmreg.reg32.imm (make-instance 'x86-asm-instruction
:name "PINSRB"
:operands "xmmreg,reg32,imm"
:code-string "[rmi: 66 0f 3a 20 /r ib,u]"
:arch-flags (list "SSE41" "SB" "AR2")))

(setf PINSRD-xmmreg.mem.imm (make-instance 'x86-asm-instruction
:name "PINSRD"
:operands "xmmreg,mem,imm"
:code-string "[rmi: norexw 66 0f 3a 22 /r ib,u]"
:arch-flags (list "SSE41" "SB" "AR2")))

(setf PINSRD-xmmreg.rm32.imm (make-instance 'x86-asm-instruction
:name "PINSRD"
:operands "xmmreg,rm32,imm"
:code-string "[rmi: norexw 66 0f 3a 22 /r ib,u]"
:arch-flags (list "SSE41" "SB" "AR2")))

(setf PINSRQ-xmmreg.mem.imm (make-instance 'x86-asm-instruction
:name "PINSRQ"
:operands "xmmreg,mem,imm"
:code-string "[rmi: o64 66 0f 3a 22 /r ib,u]"
:arch-flags (list "SSE41" "X64" "SB" "AR2")))

(setf PINSRQ-xmmreg.rm64.imm (make-instance 'x86-asm-instruction
:name "PINSRQ"
:operands "xmmreg,rm64,imm"
:code-string "[rmi: o64 66 0f 3a 22 /r ib,u]"
:arch-flags (list "SSE41" "X64" "SB" "AR2")))

(setf PMAXSB-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMAXSB"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 3c /r]"
:arch-flags (list "SSE41")))

(setf PMAXSD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMAXSD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 3d /r]"
:arch-flags (list "SSE41")))

(setf PMAXUD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMAXUD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 3f /r]"
:arch-flags (list "SSE41")))

(setf PMAXUW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMAXUW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 3e /r]"
:arch-flags (list "SSE41")))

(setf PMINSB-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMINSB"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 38 /r]"
:arch-flags (list "SSE41")))

(setf PMINSD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMINSD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 39 /r]"
:arch-flags (list "SSE41")))

(setf PMINUD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMINUD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 3b /r]"
:arch-flags (list "SSE41")))

(setf PMINUW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMINUW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 3a /r]"
:arch-flags (list "SSE41")))

(setf PMOVSXBW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMOVSXBW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 20 /r]"
:arch-flags (list "SSE41" "SQ")))

(setf PMOVSXBD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMOVSXBD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 21 /r]"
:arch-flags (list "SSE41" "SD")))

(setf PMOVSXBQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMOVSXBQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 22 /r]"
:arch-flags (list "SSE41" "SW")))

(setf PMOVSXWD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMOVSXWD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 23 /r]"
:arch-flags (list "SSE41" "SQ")))

(setf PMOVSXWQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMOVSXWQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 24 /r]"
:arch-flags (list "SSE41" "SD")))

(setf PMOVSXDQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMOVSXDQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 25 /r]"
:arch-flags (list "SSE41" "SQ")))

(setf PMOVZXBW-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMOVZXBW"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 30 /r]"
:arch-flags (list "SSE41" "SQ")))

(setf PMOVZXBD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMOVZXBD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 31 /r]"
:arch-flags (list "SSE41" "SD")))

(setf PMOVZXBQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMOVZXBQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 32 /r]"
:arch-flags (list "SSE41" "SW")))

(setf PMOVZXWD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMOVZXWD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 33 /r]"
:arch-flags (list "SSE41" "SQ")))

(setf PMOVZXWQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMOVZXWQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 34 /r]"
:arch-flags (list "SSE41" "SD")))

(setf PMOVZXDQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMOVZXDQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 35 /r]"
:arch-flags (list "SSE41" "SQ")))

(setf PMULDQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMULDQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 28 /r]"
:arch-flags (list "SSE41")))

(setf PMULLD-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PMULLD"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 40 /r]"
:arch-flags (list "SSE41")))

(setf PTEST-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PTEST"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 17 /r]"
:arch-flags (list "SSE41")))

(setf ROUNDPD-xmmreg.xmmrm.imm (make-instance 'x86-asm-instruction
:name "ROUNDPD"
:operands "xmmreg,xmmrm,imm"
:code-string "[rmi: 66 0f 3a 09 /r ib,u]"
:arch-flags (list "SSE41")))

(setf ROUNDPS-xmmreg.xmmrm.imm (make-instance 'x86-asm-instruction
:name "ROUNDPS"
:operands "xmmreg,xmmrm,imm"
:code-string "[rmi: 66 0f 3a 08 /r ib,u]"
:arch-flags (list "SSE41")))

(setf ROUNDSD-xmmreg.xmmrm.imm (make-instance 'x86-asm-instruction
:name "ROUNDSD"
:operands "xmmreg,xmmrm,imm"
:code-string "[rmi: 66 0f 3a 0b /r ib,u]"
:arch-flags (list "SSE41")))

(setf ROUNDSS-xmmreg.xmmrm.imm (make-instance 'x86-asm-instruction
:name "ROUNDSS"
:operands "xmmreg,xmmrm,imm"
:code-string "[rmi: 66 0f 3a 0a /r ib,u]"
:arch-flags (list "SSE41")))

(setf CRC32-reg32.rm8 (make-instance 'x86-asm-instruction
:name "CRC32"
:operands "reg32,rm8"
:code-string "[rm: f2i 0f 38 f0 /r]"
:arch-flags (list "SSE42")))

(setf CRC32-reg32.rm16 (make-instance 'x86-asm-instruction
:name "CRC32"
:operands "reg32,rm16"
:code-string "[rm: o16 f2i 0f 38 f1 /r]"
:arch-flags (list "SSE42")))

(setf CRC32-reg32.rm32 (make-instance 'x86-asm-instruction
:name "CRC32"
:operands "reg32,rm32"
:code-string "[rm: o32 f2i 0f 38 f1 /r]"
:arch-flags (list "SSE42")))

(setf CRC32-reg64.rm8 (make-instance 'x86-asm-instruction
:name "CRC32"
:operands "reg64,rm8"
:code-string "[rm: o64 f2i 0f 38 f0 /r]"
:arch-flags (list "SSE42" "X64")))

(setf CRC32-reg64.rm64 (make-instance 'x86-asm-instruction
:name "CRC32"
:operands "reg64,rm64"
:code-string "[rm: o64 f2i 0f 38 f1 /r]"
:arch-flags (list "SSE42" "X64")))

(setf PCMPESTRI-xmmreg.xmmrm.imm (make-instance 'x86-asm-instruction
:name "PCMPESTRI"
:operands "xmmreg,xmmrm,imm"
:code-string "[rmi: 66 0f 3a 61 /r ib,u]"
:arch-flags (list "SSE42")))

(setf PCMPESTRM-xmmreg.xmmrm.imm (make-instance 'x86-asm-instruction
:name "PCMPESTRM"
:operands "xmmreg,xmmrm,imm"
:code-string "[rmi: 66 0f 3a 60 /r ib,u]"
:arch-flags (list "SSE42")))

(setf PCMPISTRI-xmmreg.xmmrm.imm (make-instance 'x86-asm-instruction
:name "PCMPISTRI"
:operands "xmmreg,xmmrm,imm"
:code-string "[rmi: 66 0f 3a 63 /r ib,u]"
:arch-flags (list "SSE42")))

(setf PCMPISTRM-xmmreg.xmmrm.imm (make-instance 'x86-asm-instruction
:name "PCMPISTRM"
:operands "xmmreg,xmmrm,imm"
:code-string "[rmi: 66 0f 3a 62 /r ib,u]"
:arch-flags (list "SSE42")))

(setf PCMPGTQ-xmmreg.xmmrm (make-instance 'x86-asm-instruction
:name "PCMPGTQ"
:operands "xmmreg,xmmrm"
:code-string "[rm: 66 0f 38 37 /r]"
:arch-flags (list "SSE42")))

(setf POPCNT-reg16.rm16 (make-instance 'x86-asm-instruction
:name "POPCNT"
:operands "reg16,rm16"
:code-string "[rm: o16 f3i 0f b8 /r]"
:arch-flags (list "NEHALEM" "SW")))

(setf POPCNT-reg32.rm32 (make-instance 'x86-asm-instruction
:name "POPCNT"
:operands "reg32,rm32"
:code-string "[rm: o32 f3i 0f b8 /r]"
:arch-flags (list "NEHALEM" "SD")))

(setf POPCNT-reg64.rm64 (make-instance 'x86-asm-instruction
:name "POPCNT"
:operands "reg64,rm64"
:code-string "[rm: o64 f3i 0f b8 /r]"
:arch-flags (list "NEHALEM" "SQ" "X64")))

(setf GETSEC-void (make-instance 'x86-asm-instruction
:name "GETSEC"
:operands "void"
:code-string "[ 0f 37]"
:arch-flags (list "KATMAI")))

(setf PFRCPV-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PFRCPV"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r 86]"
:arch-flags (list "PENT" "3DNOW" "SQ" "CYRIX")))

(setf PFRSQRTV-mmxreg.mmxrm (make-instance 'x86-asm-instruction
:name "PFRSQRTV"
:operands "mmxreg,mmxrm"
:code-string "[rm: o64nw 0f 0f /r 87]"
:arch-flags (list "PENT" "3DNOW" "SQ" "CYRIX")))

(setf MOVBE-reg16.mem16 (make-instance 'x86-asm-instruction
:name "MOVBE"
:operands "reg16,mem16"
:code-string "[rm: o16 norep 0f 38 f0 /r]"
:arch-flags (list "NEHALEM" "SM")))

(setf MOVBE-reg32.mem32 (make-instance 'x86-asm-instruction
:name "MOVBE"
:operands "reg32,mem32"
:code-string "[rm: o32 norep 0f 38 f0 /r]"
:arch-flags (list "NEHALEM" "SM")))

(setf MOVBE-reg64.mem64 (make-instance 'x86-asm-instruction
:name "MOVBE"
:operands "reg64,mem64"
:code-string "[rm: o64 norep 0f 38 f0 /r]"
:arch-flags (list "NEHALEM" "SM")))

(setf MOVBE-mem16.reg16 (make-instance 'x86-asm-instruction
:name "MOVBE"
:operands "mem16,reg16"
:code-string "[mr: o16 norep 0f 38 f1 /r]"
:arch-flags (list "NEHALEM" "SM")))

(setf MOVBE-mem32.reg32 (make-instance 'x86-asm-instruction
:name "MOVBE"
:operands "mem32,reg32"
:code-string "[mr: o32 norep 0f 38 f1 /r]"
:arch-flags (list "NEHALEM" "SM")))

(setf MOVBE-mem64.reg64 (make-instance 'x86-asm-instruction
:name "MOVBE"
:operands "mem64,reg64"
:code-string "[mr: o64 norep 0f 38 f1 /r]"
:arch-flags (list "NEHALEM" "SM")))

(setf AESENC-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "AESENC"
:operands "xmmreg,xmmrm128"
:code-string "[rm: 66 0f 38 dc /r]"
:arch-flags (list "SSE" "WESTMERE")))

(setf AESENCLAST-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "AESENCLAST"
:operands "xmmreg,xmmrm128"
:code-string "[rm: 66 0f 38 dd /r]"
:arch-flags (list "SSE" "WESTMERE")))

(setf AESDEC-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "AESDEC"
:operands "xmmreg,xmmrm128"
:code-string "[rm: 66 0f 38 de /r]"
:arch-flags (list "SSE" "WESTMERE")))

(setf AESDECLAST-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "AESDECLAST"
:operands "xmmreg,xmmrm128"
:code-string "[rm: 66 0f 38 df /r]"
:arch-flags (list "SSE" "WESTMERE")))

(setf AESIMC-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "AESIMC"
:operands "xmmreg,xmmrm128"
:code-string "[rm: 66 0f 38 db /r]"
:arch-flags (list "SSE" "WESTMERE")))

(setf AESKEYGENASSIST-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "AESKEYGENASSIST"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[rmi: 66 0f 3a df /r ib]"
:arch-flags (list "SSE" "WESTMERE")))

(setf VAESENC-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VAESENC"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 dc /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VAESENCLAST-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VAESENCLAST"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 dd /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VAESDEC-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VAESDEC"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 de /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VAESDECLAST-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VAESDECLAST"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 df /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VAESIMC-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VAESIMC"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.66.0f38 db /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VAESKEYGENASSIST-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VAESKEYGENASSIST"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[rmi: vex.128.66.0f3a df /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VADDPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VADDPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 58 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VADDPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VADDPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 58 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VADDPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VADDPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f 58 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VADDPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VADDPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f 58 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VADDSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VADDSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f 58 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VADDSS-xmmreg.xmmreg*.xmmrm32 (make-instance 'x86-asm-instruction
:name "VADDSS"
:operands "xmmreg,xmmreg*,xmmrm32"
:code-string "[rvm: vex.nds.lig.f3.0f 58 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VADDSUBPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VADDSUBPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f d0 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VADDSUBPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VADDSUBPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f d0 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VADDSUBPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VADDSUBPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.f2.0f d0 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VADDSUBPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VADDSUBPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.f2.0f d0 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VANDPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VANDPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 54 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VANDPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VANDPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 54 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VANDPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VANDPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f 54 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VANDPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VANDPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f 54 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VANDNPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VANDNPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 55 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VANDNPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VANDNPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 55 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VANDNPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VANDNPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f 55 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VANDNPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VANDNPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f 55 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VBLENDPD-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VBLENDPD"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: vex.nds.128.66.0f3a 0d /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VBLENDPD-ymmreg.ymmreg*.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VBLENDPD"
:operands "ymmreg,ymmreg*,ymmrm256,imm8"
:code-string "[rvmi: vex.nds.256.66.0f3a 0d /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VBLENDPS-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VBLENDPS"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: vex.nds.128.66.0f3a 0c /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VBLENDPS-ymmreg.ymmreg*.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VBLENDPS"
:operands "ymmreg,ymmreg*,ymmrm256,imm8"
:code-string "[rvmi: vex.nds.256.66.0f3a 0c /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VBLENDVPD-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VBLENDVPD"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: vex.nds.128.66.0f3a.w0 4b /r /is4]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VBLENDVPD-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VBLENDVPD"
:operands "ymmreg,ymmreg*,ymmrm256,ymmreg"
:code-string "[rvms: vex.nds.256.66.0f3a.w0 4b /r /is4]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VBLENDVPS-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VBLENDVPS"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: vex.nds.128.66.0f3a.w0 4a /r /is4]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VBLENDVPS-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VBLENDVPS"
:operands "ymmreg,ymmreg*,ymmrm256,ymmreg"
:code-string "[rvms: vex.nds.256.66.0f3a.w0 4a /r /is4]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VBROADCASTSS-xmmreg.mem32 (make-instance 'x86-asm-instruction
:name "VBROADCASTSS"
:operands "xmmreg,mem32"
:code-string "[rm: vex.128.66.0f38.w0 18 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VBROADCASTSS-ymmreg.mem32 (make-instance 'x86-asm-instruction
:name "VBROADCASTSS"
:operands "ymmreg,mem32"
:code-string "[rm: vex.256.66.0f38.w0 18 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VBROADCASTSD-ymmreg.mem64 (make-instance 'x86-asm-instruction
:name "VBROADCASTSD"
:operands "ymmreg,mem64"
:code-string "[rm: vex.256.66.0f38.w0 19 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VBROADCASTF128-ymmreg.mem128 (make-instance 'x86-asm-instruction
:name "VBROADCASTF128"
:operands "ymmreg,mem128"
:code-string "[rm: vex.256.66.0f38.w0 1a /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_OSPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPEQ_OSPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 10]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_OSPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPEQ_OSPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 10]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPEQPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 00]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPEQPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 00]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLT_OSPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPLT_OSPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 01]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLT_OSPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPLT_OSPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 01]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLTPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPLTPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 01]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLTPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPLTPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 01]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLE_OSPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPLE_OSPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 02]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLE_OSPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPLE_OSPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 02]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLEPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPLEPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 02]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLEPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPLEPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 02]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPUNORD_QPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPUNORD_QPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 03]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPUNORD_QPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPUNORD_QPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 03]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPUNORDPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPUNORDPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 03]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPUNORDPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPUNORDPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 03]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_UQPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_UQPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 04]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_UQPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_UQPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 04]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNEQPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 04]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNEQPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 04]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLT_USPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNLT_USPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 05]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLT_USPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNLT_USPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 05]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLTPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNLTPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 05]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLTPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNLTPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 05]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLE_USPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNLE_USPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 06]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLE_USPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNLE_USPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 06]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLEPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNLEPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 06]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLEPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNLEPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 06]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPORD_QPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPORD_QPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 07]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPORD_QPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPORD_QPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 07]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPORDPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPORDPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 07]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPORDPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPORDPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 07]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_UQPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPEQ_UQPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 08]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_UQPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPEQ_UQPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 08]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGE_USPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNGE_USPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 09]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGE_USPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNGE_USPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 09]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGEPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNGEPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 09]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGEPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNGEPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 09]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGT_USPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNGT_USPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 0a]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGT_USPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNGT_USPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 0a]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGTPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNGTPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 0a]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGTPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNGTPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 0a]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPFALSE_OQPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPFALSE_OQPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 0b]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPFALSE_OQPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPFALSE_OQPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 0b]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPFALSEPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPFALSEPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 0b]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPFALSEPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPFALSEPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 0b]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_OQPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_OQPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 0c]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_OQPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_OQPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 0c]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGE_OSPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPGE_OSPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 0d]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGE_OSPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPGE_OSPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 0d]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGEPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPGEPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 0d]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGEPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPGEPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 0d]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGT_OSPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPGT_OSPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 0e]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGT_OSPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPGT_OSPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 0e]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGTPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPGTPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 0e]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGTPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPGTPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 0e]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPTRUE_UQPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPTRUE_UQPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 0f]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPTRUE_UQPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPTRUE_UQPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 0f]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPTRUEPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPTRUEPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 0f]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPTRUEPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPTRUEPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 0f]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_OSPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPEQ_OSPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 10]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_OSPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPEQ_OSPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 10]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLT_OQPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPLT_OQPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 11]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLT_OQPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPLT_OQPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 11]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLE_OQPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPLE_OQPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 12]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLE_OQPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPLE_OQPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 12]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPUNORD_SPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPUNORD_SPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 13]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPUNORD_SPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPUNORD_SPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 13]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_USPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_USPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 14]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_USPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_USPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 14]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLT_UQPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNLT_UQPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 15]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLT_UQPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNLT_UQPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 15]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLE_UQPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNLE_UQPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 16]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLE_UQPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNLE_UQPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 16]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPORD_SPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPORD_SPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 17]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPORD_SPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPORD_SPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 17]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_USPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPEQ_USPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 18]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_USPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPEQ_USPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 18]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGE_UQPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNGE_UQPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 19]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGE_UQPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNGE_UQPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 19]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGT_UQPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNGT_UQPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 1a]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGT_UQPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNGT_UQPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 1a]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPFALSE_OSPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPFALSE_OSPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 1b]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPFALSE_OSPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPFALSE_OSPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 1b]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_OSPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_OSPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 1c]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_OSPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_OSPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 1c]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGE_OQPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPGE_OQPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 1d]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGE_OQPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPGE_OQPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 1d]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGT_OQPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPGT_OQPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 1e]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGT_OQPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPGT_OQPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 1e]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPTRUE_USPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPTRUE_USPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f c2 /r 1f]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPTRUE_USPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPTRUE_USPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f c2 /r 1f]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPPD-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VCMPPD"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: vex.nds.128.66.0f c2 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPPD-ymmreg.ymmreg*.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VCMPPD"
:operands "ymmreg,ymmreg*,ymmrm256,imm8"
:code-string "[rvmi: vex.nds.256.66.0f c2 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_OSPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPEQ_OSPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 10]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_OSPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPEQ_OSPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 10]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPEQPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 00]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPEQPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 00]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLT_OSPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPLT_OSPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 01]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLT_OSPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPLT_OSPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 01]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLTPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPLTPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 01]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLTPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPLTPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 01]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLE_OSPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPLE_OSPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 02]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLE_OSPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPLE_OSPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 02]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLEPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPLEPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 02]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLEPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPLEPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 02]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPUNORD_QPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPUNORD_QPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 03]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPUNORD_QPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPUNORD_QPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 03]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPUNORDPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPUNORDPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 03]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPUNORDPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPUNORDPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 03]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_UQPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_UQPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 04]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_UQPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_UQPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 04]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNEQPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 04]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNEQPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 04]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLT_USPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNLT_USPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 05]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLT_USPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNLT_USPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 05]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLTPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNLTPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 05]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLTPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNLTPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 05]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLE_USPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNLE_USPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 06]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLE_USPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNLE_USPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 06]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLEPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNLEPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 06]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLEPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNLEPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 06]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPORD_QPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPORD_QPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 07]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPORD_QPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPORD_QPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 07]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPORDPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPORDPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 07]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPORDPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPORDPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 07]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_UQPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPEQ_UQPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 08]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_UQPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPEQ_UQPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 08]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGE_USPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNGE_USPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 09]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGE_USPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNGE_USPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 09]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGEPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNGEPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 09]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGEPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNGEPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 09]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGT_USPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNGT_USPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 0a]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGT_USPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNGT_USPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 0a]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGTPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNGTPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 0a]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGTPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNGTPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 0a]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPFALSE_OQPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPFALSE_OQPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 0b]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPFALSE_OQPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPFALSE_OQPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 0b]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPFALSEPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPFALSEPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 0b]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPFALSEPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPFALSEPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 0b]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_OQPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_OQPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 0c]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_OQPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_OQPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 0c]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGE_OSPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPGE_OSPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 0d]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGE_OSPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPGE_OSPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 0d]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGEPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPGEPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 0d]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGEPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPGEPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 0d]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGT_OSPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPGT_OSPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 0e]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGT_OSPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPGT_OSPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 0e]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGTPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPGTPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 0e]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGTPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPGTPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 0e]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPTRUE_UQPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPTRUE_UQPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 0f]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPTRUE_UQPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPTRUE_UQPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 0f]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPTRUEPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPTRUEPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 0f]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPTRUEPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPTRUEPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 0f]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_OSPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPEQ_OSPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 10]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_OSPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPEQ_OSPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 10]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLT_OQPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPLT_OQPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 11]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLT_OQPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPLT_OQPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 11]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLE_OQPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPLE_OQPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 12]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLE_OQPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPLE_OQPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 12]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPUNORD_SPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPUNORD_SPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 13]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPUNORD_SPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPUNORD_SPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 13]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_USPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_USPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 14]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_USPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_USPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 14]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLT_UQPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNLT_UQPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 15]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLT_UQPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNLT_UQPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 15]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLE_UQPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNLE_UQPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 16]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLE_UQPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNLE_UQPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 16]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPORD_SPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPORD_SPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 17]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPORD_SPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPORD_SPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 17]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_USPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPEQ_USPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 18]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_USPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPEQ_USPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 18]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGE_UQPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNGE_UQPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 19]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGE_UQPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNGE_UQPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 19]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGT_UQPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNGT_UQPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 1a]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGT_UQPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNGT_UQPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 1a]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPFALSE_OSPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPFALSE_OSPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 1b]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPFALSE_OSPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPFALSE_OSPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 1b]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_OSPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_OSPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 1c]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_OSPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_OSPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 1c]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGE_OQPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPGE_OQPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 1d]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGE_OQPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPGE_OQPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 1d]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGT_OQPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPGT_OQPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 1e]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGT_OQPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPGT_OQPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 1e]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPTRUE_USPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCMPTRUE_USPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f c2 /r 1f]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPTRUE_USPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCMPTRUE_USPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f c2 /r 1f]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPPS-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VCMPPS"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: vex.nds.128.0f c2 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPPS-ymmreg.ymmreg*.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VCMPPS"
:operands "ymmreg,ymmreg*,ymmrm256,imm8"
:code-string "[rvmi: vex.nds.256.0f c2 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_OSSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPEQ_OSSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 10]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPEQSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 00]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLT_OSSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPLT_OSSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 01]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLTSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPLTSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 01]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLE_OSSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPLE_OSSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 02]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLESD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPLESD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 02]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPUNORD_QSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPUNORD_QSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 03]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPUNORDSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPUNORDSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 03]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_UQSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_UQSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 04]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNEQSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 04]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLT_USSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNLT_USSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 05]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLTSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNLTSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 05]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLE_USSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNLE_USSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 06]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLESD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNLESD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 06]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPORD_QSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPORD_QSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 07]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPORDSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPORDSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 07]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_UQSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPEQ_UQSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 08]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGE_USSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNGE_USSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 09]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGESD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNGESD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 09]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGT_USSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNGT_USSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 0a]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGTSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNGTSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 0a]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPFALSE_OQSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPFALSE_OQSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 0b]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPFALSESD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPFALSESD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 0b]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_OQSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_OQSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 0c]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGE_OSSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPGE_OSSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 0d]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGESD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPGESD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 0d]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGT_OSSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPGT_OSSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 0e]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGTSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPGTSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 0e]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPTRUE_UQSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPTRUE_UQSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 0f]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPTRUESD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPTRUESD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 0f]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_OSSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPEQ_OSSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 10]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLT_OQSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPLT_OQSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 11]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLE_OQSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPLE_OQSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 12]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPUNORD_SSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPUNORD_SSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 13]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_USSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_USSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 14]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLT_UQSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNLT_UQSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 15]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLE_UQSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNLE_UQSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 16]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPORD_SSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPORD_SSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 17]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_USSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPEQ_USSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 18]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGE_UQSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNGE_UQSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 19]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGT_UQSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNGT_UQSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 1a]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPFALSE_OSSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPFALSE_OSSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 1b]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_OSSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_OSSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 1c]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGE_OQSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPGE_OQSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 1d]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGT_OQSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPGT_OQSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 1e]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPTRUE_USSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPTRUE_USSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f c2 /r 1f]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPSD-xmmreg.xmmreg*.xmmrm64.imm8 (make-instance 'x86-asm-instruction
:name "VCMPSD"
:operands "xmmreg,xmmreg*,xmmrm64,imm8"
:code-string "[rvmi: vex.nds.lig.f2.0f c2 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_OSSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPEQ_OSSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 10]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPEQSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 00]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLT_OSSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPLT_OSSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 01]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLTSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPLTSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 01]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLE_OSSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPLE_OSSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 02]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLESS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPLESS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 02]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPUNORD_QSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPUNORD_QSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 03]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPUNORDSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPUNORDSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 03]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_UQSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_UQSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 04]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNEQSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 04]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLT_USSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNLT_USSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 05]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLTSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNLTSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 05]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLE_USSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNLE_USSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 06]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLESS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNLESS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 06]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPORD_QSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPORD_QSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 07]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPORDSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPORDSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 07]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_UQSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPEQ_UQSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 08]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGE_USSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNGE_USSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 09]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGESS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNGESS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 09]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGT_USSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNGT_USSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 0a]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGTSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNGTSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 0a]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPFALSE_OQSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPFALSE_OQSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 0b]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPFALSESS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPFALSESS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 0b]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_OQSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_OQSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 0c]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGE_OSSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPGE_OSSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 0d]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGESS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPGESS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 0d]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGT_OSSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPGT_OSSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 0e]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGTSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPGTSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 0e]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPTRUE_UQSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPTRUE_UQSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 0f]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPTRUESS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPTRUESS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 0f]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_OSSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPEQ_OSSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 10]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLT_OQSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPLT_OQSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 11]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPLE_OQSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPLE_OQSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 12]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPUNORD_SSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPUNORD_SSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 13]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_USSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_USSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 14]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLT_UQSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNLT_UQSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 15]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNLE_UQSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNLE_UQSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 16]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPORD_SSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPORD_SSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 17]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPEQ_USSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPEQ_USSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 18]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGE_UQSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNGE_UQSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 19]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNGT_UQSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNGT_UQSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 1a]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPFALSE_OSSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPFALSE_OSSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 1b]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPNEQ_OSSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPNEQ_OSSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 1c]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGE_OQSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPGE_OQSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 1d]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPGT_OQSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPGT_OQSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 1e]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPTRUE_USSS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCMPTRUE_USSS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f3.0f c2 /r 1f]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCMPSS-xmmreg.xmmreg*.xmmrm64.imm8 (make-instance 'x86-asm-instruction
:name "VCMPSS"
:operands "xmmreg,xmmreg*,xmmrm64,imm8"
:code-string "[rvmi: vex.nds.lig.f3.0f c2 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCOMISD-xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCOMISD"
:operands "xmmreg,xmmrm64"
:code-string "[rm: vex.lig.66.0f 2f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCOMISS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VCOMISS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: vex.lig.0f 2f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTDQ2PD-xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCVTDQ2PD"
:operands "xmmreg,xmmrm64"
:code-string "[rm: vex.128.f3.0f e6 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTDQ2PD-ymmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCVTDQ2PD"
:operands "ymmreg,xmmrm128"
:code-string "[rm: vex.256.f3.0f e6 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTDQ2PS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCVTDQ2PS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.0f 5b /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTDQ2PS-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCVTDQ2PS"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.0f 5b /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTPD2DQ-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VCVTPD2DQ"
:operands "xmmreg,xmmreg"
:code-string "[rm: vex.128.f2.0f e6 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTPD2DQ-xmmreg.mem128 (make-instance 'x86-asm-instruction
:name "VCVTPD2DQ"
:operands "xmmreg,mem128"
:code-string "[rm: vex.128.f2.0f e6 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "SO")))

(setf VCVTPD2DQ-xmmreg.ymmreg (make-instance 'x86-asm-instruction
:name "VCVTPD2DQ"
:operands "xmmreg,ymmreg"
:code-string "[rm: vex.256.f2.0f e6 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTPD2DQ-xmmreg.mem256 (make-instance 'x86-asm-instruction
:name "VCVTPD2DQ"
:operands "xmmreg,mem256"
:code-string "[rm: vex.256.f2.0f e6 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "SY")))

(setf VCVTPD2PS-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VCVTPD2PS"
:operands "xmmreg,xmmreg"
:code-string "[rm: vex.128.66.0f 5a /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTPD2PS-xmmreg.mem128 (make-instance 'x86-asm-instruction
:name "VCVTPD2PS"
:operands "xmmreg,mem128"
:code-string "[rm: vex.128.66.0f 5a /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "SO")))

(setf VCVTPD2PS-xmmreg.ymmreg (make-instance 'x86-asm-instruction
:name "VCVTPD2PS"
:operands "xmmreg,ymmreg"
:code-string "[rm: vex.256.66.0f 5a /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTPD2PS-xmmreg.mem256 (make-instance 'x86-asm-instruction
:name "VCVTPD2PS"
:operands "xmmreg,mem256"
:code-string "[rm: vex.256.66.0f 5a /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "SY")))

(setf VCVTPS2DQ-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCVTPS2DQ"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.66.0f 5b /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTPS2DQ-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCVTPS2DQ"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.66.0f 5b /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTPS2PD-xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCVTPS2PD"
:operands "xmmreg,xmmrm64"
:code-string "[rm: vex.128.0f 5a /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTPS2PD-ymmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCVTPS2PD"
:operands "ymmreg,xmmrm128"
:code-string "[rm: vex.256.0f 5a /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTSD2SI-reg32.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCVTSD2SI"
:operands "reg32,xmmrm64"
:code-string "[rm: vex.lig.f2.0f.w0 2d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTSD2SI-reg64.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCVTSD2SI"
:operands "reg64,xmmrm64"
:code-string "[rm: vex.lig.f2.0f.w1 2d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG")))

(setf VCVTSD2SS-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCVTSD2SS"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f 5a /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTSI2SD-xmmreg.xmmreg*.rm32 (make-instance 'x86-asm-instruction
:name "VCVTSI2SD"
:operands "xmmreg,xmmreg*,rm32"
:code-string "[rvm: vex.nds.lig.f2.0f.w0 2a /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "SD")))

(setf VCVTSI2SD-xmmreg.xmmreg*.mem32 (make-instance 'x86-asm-instruction
:name "VCVTSI2SD"
:operands "xmmreg,xmmreg*,mem32"
:code-string "[rvm: vex.nds.lig.f2.0f.w0 2a /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "ND" "SD")))

(setf VCVTSI2SD-xmmreg.xmmreg*.rm64 (make-instance 'x86-asm-instruction
:name "VCVTSI2SD"
:operands "xmmreg,xmmreg*,rm64"
:code-string "[rvm: vex.nds.lig.f2.0f.w1 2a /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG" "SQ")))

(setf VCVTSI2SS-xmmreg.xmmreg*.rm32 (make-instance 'x86-asm-instruction
:name "VCVTSI2SS"
:operands "xmmreg,xmmreg*,rm32"
:code-string "[rvm: vex.nds.lig.f3.0f.w0 2a /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "SD")))

(setf VCVTSI2SS-xmmreg.xmmreg*.mem32 (make-instance 'x86-asm-instruction
:name "VCVTSI2SS"
:operands "xmmreg,xmmreg*,mem32"
:code-string "[rvm: vex.nds.lig.f3.0f.w0 2a /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "ND" "SD")))

(setf VCVTSI2SS-xmmreg.xmmreg*.rm64 (make-instance 'x86-asm-instruction
:name "VCVTSI2SS"
:operands "xmmreg,xmmreg*,rm64"
:code-string "[rvm: vex.nds.lig.f3.0f.w1 2a /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG" "SQ")))

(setf VCVTSS2SD-xmmreg.xmmreg*.xmmrm32 (make-instance 'x86-asm-instruction
:name "VCVTSS2SD"
:operands "xmmreg,xmmreg*,xmmrm32"
:code-string "[rvm: vex.nds.lig.f3.0f 5a /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTSS2SI-reg32.xmmrm32 (make-instance 'x86-asm-instruction
:name "VCVTSS2SI"
:operands "reg32,xmmrm32"
:code-string "[rm: vex.lig.f3.0f.w0 2d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTSS2SI-reg64.xmmrm32 (make-instance 'x86-asm-instruction
:name "VCVTSS2SI"
:operands "reg64,xmmrm32"
:code-string "[rm: vex.lig.f3.0f.w1 2d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG")))

(setf VCVTTPD2DQ-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VCVTTPD2DQ"
:operands "xmmreg,xmmreg"
:code-string "[rm: vex.128.66.0f e6 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTTPD2DQ-xmmreg.mem128 (make-instance 'x86-asm-instruction
:name "VCVTTPD2DQ"
:operands "xmmreg,mem128"
:code-string "[rm: vex.128.66.0f e6 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "SO")))

(setf VCVTTPD2DQ-xmmreg.ymmreg (make-instance 'x86-asm-instruction
:name "VCVTTPD2DQ"
:operands "xmmreg,ymmreg"
:code-string "[rm: vex.256.66.0f e6 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTTPD2DQ-xmmreg.mem256 (make-instance 'x86-asm-instruction
:name "VCVTTPD2DQ"
:operands "xmmreg,mem256"
:code-string "[rm: vex.256.66.0f e6 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "SY")))

(setf VCVTTPS2DQ-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCVTTPS2DQ"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.f3.0f 5b /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTTPS2DQ-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VCVTTPS2DQ"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.f3.0f 5b /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTTSD2SI-reg32.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCVTTSD2SI"
:operands "reg32,xmmrm64"
:code-string "[rm: vex.lig.f2.0f.w0 2c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTTSD2SI-reg64.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCVTTSD2SI"
:operands "reg64,xmmrm64"
:code-string "[rm: vex.lig.f2.0f.w1 2c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG")))

(setf VCVTTSS2SI-reg32.xmmrm32 (make-instance 'x86-asm-instruction
:name "VCVTTSS2SI"
:operands "reg32,xmmrm32"
:code-string "[rm: vex.lig.f3.0f.w0 2c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VCVTTSS2SI-reg64.xmmrm32 (make-instance 'x86-asm-instruction
:name "VCVTTSS2SI"
:operands "reg64,xmmrm32"
:code-string "[rm: vex.lig.f3.0f.w1 2c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG")))

(setf VDIVPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VDIVPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 5e /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VDIVPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VDIVPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 5e /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VDIVPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VDIVPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f 5e /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VDIVPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VDIVPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f 5e /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VDIVSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VDIVSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f 5e /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VDIVSS-xmmreg.xmmreg*.xmmrm32 (make-instance 'x86-asm-instruction
:name "VDIVSS"
:operands "xmmreg,xmmreg*,xmmrm32"
:code-string "[rvm: vex.nds.lig.f3.0f 5e /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VDPPD-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VDPPD"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: vex.nds.128.66.0f3a 41 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VDPPS-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VDPPS"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: vex.nds.128.66.0f3a 40 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VDPPS-ymmreg.ymmreg*.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VDPPS"
:operands "ymmreg,ymmreg*,ymmrm256,imm8"
:code-string "[rvmi: vex.nds.256.66.0f3a 40 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VEXTRACTF128-xmmrm128.ymmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTF128"
:operands "xmmrm128,ymmreg,imm8"
:code-string "[mri: vex.256.66.0f3a.w0 19 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VEXTRACTPS-rm32.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTPS"
:operands "rm32,xmmreg,imm8"
:code-string "[mri: vex.128.66.0f3a 17 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VHADDPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VHADDPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 7c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VHADDPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VHADDPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 7c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VHADDPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VHADDPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.f2.0f 7c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VHADDPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VHADDPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.f2.0f 7c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VHSUBPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VHSUBPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 7d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VHSUBPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VHSUBPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 7d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VHSUBPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VHSUBPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.f2.0f 7d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VHSUBPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VHSUBPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.f2.0f 7d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VINSERTF128-ymmreg.ymmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VINSERTF128"
:operands "ymmreg,ymmreg,xmmrm128,imm8"
:code-string "[rvmi: vex.nds.256.66.0f3a.w0 18 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VINSERTPS-xmmreg.xmmreg*.xmmrm32.imm8 (make-instance 'x86-asm-instruction
:name "VINSERTPS"
:operands "xmmreg,xmmreg*,xmmrm32,imm8"
:code-string "[rvmi: vex.nds.128.66.0f3a 21 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VLDDQU-xmmreg.mem128 (make-instance 'x86-asm-instruction
:name "VLDDQU"
:operands "xmmreg,mem128"
:code-string "[rm: vex.128.f2.0f f0 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VLDQQU-ymmreg.mem256 (make-instance 'x86-asm-instruction
:name "VLDQQU"
:operands "ymmreg,mem256"
:code-string "[rm: vex.256.f2.0f f0 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VLDDQU-ymmreg.mem256 (make-instance 'x86-asm-instruction
:name "VLDDQU"
:operands "ymmreg,mem256"
:code-string "[rm: vex.256.f2.0f f0 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VLDMXCSR-mem32 (make-instance 'x86-asm-instruction
:name "VLDMXCSR"
:operands "mem32"
:code-string "[m: vex.lz.0f ae /2]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMASKMOVDQU-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VMASKMOVDQU"
:operands "xmmreg,xmmreg"
:code-string "[rm: vex.128.66.0f f7 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMASKMOVPS-xmmreg.xmmreg.mem128 (make-instance 'x86-asm-instruction
:name "VMASKMOVPS"
:operands "xmmreg,xmmreg,mem128"
:code-string "[rvm: vex.nds.128.66.0f38.w0 2c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMASKMOVPS-ymmreg.ymmreg.mem256 (make-instance 'x86-asm-instruction
:name "VMASKMOVPS"
:operands "ymmreg,ymmreg,mem256"
:code-string "[rvm: vex.nds.256.66.0f38.w0 2c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMASKMOVPS-mem128.xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VMASKMOVPS"
:operands "mem128,xmmreg,xmmreg"
:code-string "[mvr: vex.nds.128.66.0f38.w0 2e /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "SO")))

(setf VMASKMOVPS-mem256.ymmreg.ymmreg (make-instance 'x86-asm-instruction
:name "VMASKMOVPS"
:operands "mem256,ymmreg,ymmreg"
:code-string "[mvr: vex.nds.256.66.0f38.w0 2e /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "SY")))

(setf VMASKMOVPD-xmmreg.xmmreg.mem128 (make-instance 'x86-asm-instruction
:name "VMASKMOVPD"
:operands "xmmreg,xmmreg,mem128"
:code-string "[rvm: vex.nds.128.66.0f38.w0 2d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMASKMOVPD-ymmreg.ymmreg.mem256 (make-instance 'x86-asm-instruction
:name "VMASKMOVPD"
:operands "ymmreg,ymmreg,mem256"
:code-string "[rvm: vex.nds.256.66.0f38.w0 2d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMASKMOVPD-mem128.xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VMASKMOVPD"
:operands "mem128,xmmreg,xmmreg"
:code-string "[mvr: vex.nds.128.66.0f38.w0 2f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMASKMOVPD-mem256.ymmreg.ymmreg (make-instance 'x86-asm-instruction
:name "VMASKMOVPD"
:operands "mem256,ymmreg,ymmreg"
:code-string "[mvr: vex.nds.256.66.0f38.w0 2f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMAXPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMAXPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 5f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMAXPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMAXPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 5f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMAXPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMAXPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f 5f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMAXPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMAXPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f 5f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMAXSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VMAXSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f 5f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMAXSS-xmmreg.xmmreg*.xmmrm32 (make-instance 'x86-asm-instruction
:name "VMAXSS"
:operands "xmmreg,xmmreg*,xmmrm32"
:code-string "[rvm: vex.nds.lig.f3.0f 5f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMINPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMINPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 5d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMINPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMINPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 5d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMINPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMINPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f 5d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMINPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMINPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f 5d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMINSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VMINSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f 5d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMINSS-xmmreg.xmmreg*.xmmrm32 (make-instance 'x86-asm-instruction
:name "VMINSS"
:operands "xmmreg,xmmreg*,xmmrm32"
:code-string "[rvm: vex.nds.lig.f3.0f 5d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVAPD-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVAPD"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.66.0f 28 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVAPD-xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVAPD"
:operands "xmmrm128,xmmreg"
:code-string "[mr: vex.128.66.0f 29 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVAPD-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVAPD"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.66.0f 28 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVAPD-ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVAPD"
:operands "ymmrm256,ymmreg"
:code-string "[mr: vex.256.66.0f 29 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVAPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVAPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.0f 28 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVAPS-xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVAPS"
:operands "xmmrm128,xmmreg"
:code-string "[mr: vex.128.0f 29 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVAPS-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVAPS"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.0f 28 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVAPS-ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVAPS"
:operands "ymmrm256,ymmreg"
:code-string "[mr: vex.256.0f 29 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVD-xmmreg.rm32 (make-instance 'x86-asm-instruction
:name "VMOVD"
:operands "xmmreg,rm32"
:code-string "[rm: vex.128.66.0f.w0 6e /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVD-rm32.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVD"
:operands "rm32,xmmreg"
:code-string "[mr: vex.128.66.0f.w0 7e /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVQ-xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VMOVQ"
:operands "xmmreg,xmmrm64"
:code-string "[rm: vex.128.f3.0f 7e /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "SQ")))

(setf VMOVQ-xmmrm64.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVQ"
:operands "xmmrm64,xmmreg"
:code-string "[mr: vex.128.66.0f d6 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "SQ")))

(setf VMOVQ-xmmreg.rm64 (make-instance 'x86-asm-instruction
:name "VMOVQ"
:operands "xmmreg,rm64"
:code-string "[rm: vex.128.66.0f.w1 6e /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG" "SQ")))

(setf VMOVQ-rm64.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVQ"
:operands "rm64,xmmreg"
:code-string "[mr: vex.128.66.0f.w1 7e /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG" "SQ")))

(setf VMOVDDUP-xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VMOVDDUP"
:operands "xmmreg,xmmrm64"
:code-string "[rm: vex.128.f2.0f 12 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVDDUP-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVDDUP"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.f2.0f 12 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVDQA-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVDQA"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.66.0f 6f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVDQA-xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQA"
:operands "xmmrm128,xmmreg"
:code-string "[mr: vex.128.66.0f 7f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVQQA-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVQQA"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.66.0f 6f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVQQA-ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVQQA"
:operands "ymmrm256,ymmreg"
:code-string "[mr: vex.256.66.0f 7f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVDQA-ymmreg.ymmrm (make-instance 'x86-asm-instruction
:name "VMOVDQA"
:operands "ymmreg,ymmrm"
:code-string "[rm: vex.256.66.0f 6f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVDQA-ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVDQA"
:operands "ymmrm256,ymmreg"
:code-string "[mr: vex.256.66.0f 7f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVDQU-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVDQU"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.f3.0f 6f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVDQU-xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU"
:operands "xmmrm128,xmmreg"
:code-string "[mr: vex.128.f3.0f 7f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVQQU-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVQQU"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.f3.0f 6f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVQQU-ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVQQU"
:operands "ymmrm256,ymmreg"
:code-string "[mr: vex.256.f3.0f 7f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVDQU-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVDQU"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.f3.0f 6f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVDQU-ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU"
:operands "ymmrm256,ymmreg"
:code-string "[mr: vex.256.f3.0f 7f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVHLPS-xmmreg.xmmreg*.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVHLPS"
:operands "xmmreg,xmmreg*,xmmreg"
:code-string "[rvm: vex.nds.128.0f 12 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVHPD-xmmreg.xmmreg*.mem64 (make-instance 'x86-asm-instruction
:name "VMOVHPD"
:operands "xmmreg,xmmreg*,mem64"
:code-string "[rvm: vex.nds.128.66.0f 16 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVHPD-mem64.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVHPD"
:operands "mem64,xmmreg"
:code-string "[mr: vex.128.66.0f 17 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVHPS-xmmreg.xmmreg*.mem64 (make-instance 'x86-asm-instruction
:name "VMOVHPS"
:operands "xmmreg,xmmreg*,mem64"
:code-string "[rvm: vex.nds.128.0f 16 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVHPS-mem64.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVHPS"
:operands "mem64,xmmreg"
:code-string "[mr: vex.128.0f 17 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVLHPS-xmmreg.xmmreg*.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVLHPS"
:operands "xmmreg,xmmreg*,xmmreg"
:code-string "[rvm: vex.nds.128.0f 16 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVLPD-xmmreg.xmmreg*.mem64 (make-instance 'x86-asm-instruction
:name "VMOVLPD"
:operands "xmmreg,xmmreg*,mem64"
:code-string "[rvm: vex.nds.128.66.0f 12 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVLPD-mem64.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVLPD"
:operands "mem64,xmmreg"
:code-string "[mr: vex.128.66.0f 13 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVLPS-xmmreg.xmmreg*.mem64 (make-instance 'x86-asm-instruction
:name "VMOVLPS"
:operands "xmmreg,xmmreg*,mem64"
:code-string "[rvm: vex.nds.128.0f 12 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVLPS-mem64.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVLPS"
:operands "mem64,xmmreg"
:code-string "[mr: vex.128.0f 13 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVMSKPD-reg64.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVMSKPD"
:operands "reg64,xmmreg"
:code-string "[rm: vex.128.66.0f 50 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG")))

(setf VMOVMSKPD-reg32.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVMSKPD"
:operands "reg32,xmmreg"
:code-string "[rm: vex.128.66.0f 50 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVMSKPD-reg64.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVMSKPD"
:operands "reg64,ymmreg"
:code-string "[rm: vex.256.66.0f 50 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG")))

(setf VMOVMSKPD-reg32.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVMSKPD"
:operands "reg32,ymmreg"
:code-string "[rm: vex.256.66.0f 50 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVMSKPS-reg64.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVMSKPS"
:operands "reg64,xmmreg"
:code-string "[rm: vex.128.0f 50 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG")))

(setf VMOVMSKPS-reg32.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVMSKPS"
:operands "reg32,xmmreg"
:code-string "[rm: vex.128.0f 50 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVMSKPS-reg64.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVMSKPS"
:operands "reg64,ymmreg"
:code-string "[rm: vex.256.0f 50 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG")))

(setf VMOVMSKPS-reg32.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVMSKPS"
:operands "reg32,ymmreg"
:code-string "[rm: vex.256.0f 50 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVNTDQ-mem128.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVNTDQ"
:operands "mem128,xmmreg"
:code-string "[mr: vex.128.66.0f e7 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVNTQQ-mem256.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVNTQQ"
:operands "mem256,ymmreg"
:code-string "[mr: vex.256.66.0f e7 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVNTDQ-mem256.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVNTDQ"
:operands "mem256,ymmreg"
:code-string "[mr: vex.256.66.0f e7 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVNTDQA-xmmreg.mem128 (make-instance 'x86-asm-instruction
:name "VMOVNTDQA"
:operands "xmmreg,mem128"
:code-string "[rm: vex.128.66.0f38 2a /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVNTPD-mem128.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVNTPD"
:operands "mem128,xmmreg"
:code-string "[mr: vex.128.66.0f 2b /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVNTPD-mem256.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVNTPD"
:operands "mem256,ymmreg"
:code-string "[mr: vex.256.66.0f 2b /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVNTPS-mem128.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVNTPS"
:operands "mem128,xmmreg"
:code-string "[mr: vex.128.0f 2b /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVNTPS-mem128.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVNTPS"
:operands "mem128,ymmreg"
:code-string "[mr: vex.256.0f 2b /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVSD-xmmreg.xmmreg*.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVSD"
:operands "xmmreg,xmmreg*,xmmreg"
:code-string "[rvm: vex.nds.lig.f2.0f 10 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVSD-xmmreg.mem64 (make-instance 'x86-asm-instruction
:name "VMOVSD"
:operands "xmmreg,mem64"
:code-string "[rm: vex.lig.f2.0f 10 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVSD-xmmreg.xmmreg*.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVSD"
:operands "xmmreg,xmmreg*,xmmreg"
:code-string "[mvr: vex.nds.lig.f2.0f 11 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVSD-mem64.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVSD"
:operands "mem64,xmmreg"
:code-string "[mr: vex.lig.f2.0f 11 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVSHDUP-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVSHDUP"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.f3.0f 16 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVSHDUP-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVSHDUP"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.f3.0f 16 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVSLDUP-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVSLDUP"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.f3.0f 12 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVSLDUP-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVSLDUP"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.f3.0f 12 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVSS-xmmreg.xmmreg*.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVSS"
:operands "xmmreg,xmmreg*,xmmreg"
:code-string "[rvm: vex.nds.lig.f3.0f 10 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVSS-xmmreg.mem32 (make-instance 'x86-asm-instruction
:name "VMOVSS"
:operands "xmmreg,mem32"
:code-string "[rm: vex.lig.f3.0f 10 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVSS-xmmreg.xmmreg*.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVSS"
:operands "xmmreg,xmmreg*,xmmreg"
:code-string "[mvr: vex.nds.lig.f3.0f 11 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVSS-mem32.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVSS"
:operands "mem32,xmmreg"
:code-string "[mr: vex.lig.f3.0f 11 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVUPD-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVUPD"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.66.0f 10 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVUPD-xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVUPD"
:operands "xmmrm128,xmmreg"
:code-string "[mr: vex.128.66.0f 11 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVUPD-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVUPD"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.66.0f 10 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVUPD-ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVUPD"
:operands "ymmrm256,ymmreg"
:code-string "[mr: vex.256.66.0f 11 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVUPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVUPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.0f 10 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVUPS-xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVUPS"
:operands "xmmrm128,xmmreg"
:code-string "[mr: vex.128.0f 11 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVUPS-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVUPS"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.0f 10 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMOVUPS-ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVUPS"
:operands "ymmrm256,ymmreg"
:code-string "[mr: vex.256.0f 11 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMPSADBW-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VMPSADBW"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: vex.nds.128.66.0f3a 42 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMULPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMULPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 59 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMULPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMULPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 59 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMULPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMULPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f 59 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMULPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMULPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f 59 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMULSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VMULSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f 59 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VMULSS-xmmreg.xmmreg*.xmmrm32 (make-instance 'x86-asm-instruction
:name "VMULSS"
:operands "xmmreg,xmmreg*,xmmrm32"
:code-string "[rvm: vex.nds.lig.f3.0f 59 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VORPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VORPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 56 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VORPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VORPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 56 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VORPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VORPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f 56 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VORPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VORPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f 56 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPABSB-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPABSB"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.66.0f38 1c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPABSW-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPABSW"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.66.0f38 1d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPABSD-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPABSD"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.66.0f38 1e /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPACKSSWB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPACKSSWB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 63 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPACKSSDW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPACKSSDW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 6b /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPACKUSWB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPACKUSWB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 67 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPACKUSDW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPACKUSDW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 2b /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPADDB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPADDB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f fc /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPADDW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPADDW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f fd /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPADDD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPADDD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f fe /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPADDQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPADDQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f d4 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPADDSB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPADDSB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f ec /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPADDSW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPADDSW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f ed /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPADDUSB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPADDUSB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f dc /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPADDUSW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPADDUSW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f dd /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPALIGNR-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPALIGNR"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: vex.nds.128.66.0f3a 0f /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPAND-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPAND"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f db /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPANDN-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPANDN"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f df /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPAVGB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPAVGB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f e0 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPAVGW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPAVGW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f e3 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPBLENDVB-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VPBLENDVB"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: vex.nds.128.66.0f3a.w0 4c /r /is4]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPBLENDW-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPBLENDW"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: vex.nds.128.66.0f3a 0e /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPCMPESTRI-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPESTRI"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[rmi: vex.128.66.0f3a 61 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPCMPESTRM-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPESTRM"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[rmi: vex.128.66.0f3a 60 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPCMPISTRI-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPISTRI"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[rmi: vex.128.66.0f3a 63 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPCMPISTRM-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPISTRM"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[rmi: vex.128.66.0f3a 62 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPCMPEQB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPCMPEQB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 74 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPCMPEQW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPCMPEQW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 75 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPCMPEQD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPCMPEQD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 76 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPCMPEQQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPCMPEQQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 29 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPCMPGTB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPCMPGTB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 64 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPCMPGTW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPCMPGTW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 65 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPCMPGTD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPCMPGTD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 66 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPCMPGTQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPCMPGTQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 37 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPERMILPD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPERMILPD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38.w0 0d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPERMILPD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPERMILPD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38.w0 0d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPERMILPD-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPERMILPD"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[rmi: vex.128.66.0f3a.w0 05 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPERMILPD-ymmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPERMILPD"
:operands "ymmreg,ymmrm256,imm8"
:code-string "[rmi: vex.256.66.0f3a.w0 05 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPERMILPS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPERMILPS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38.w0 0c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPERMILPS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPERMILPS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38.w0 0c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPERMILPS-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPERMILPS"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[rmi: vex.128.66.0f3a.w0 04 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPERMILPS-ymmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPERMILPS"
:operands "ymmreg,ymmrm256,imm8"
:code-string "[rmi: vex.256.66.0f3a.w0 04 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPERM2F128-ymmreg.ymmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPERM2F128"
:operands "ymmreg,ymmreg,ymmrm256,imm8"
:code-string "[rvmi: vex.nds.256.66.0f3a.w0 06 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPEXTRB-reg64.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRB"
:operands "reg64,xmmreg,imm8"
:code-string "[mri: vex.128.66.0f3a.w0 14 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG")))

(setf VPEXTRB-reg32.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRB"
:operands "reg32,xmmreg,imm8"
:code-string "[mri: vex.128.66.0f3a.w0 14 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPEXTRB-mem8.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRB"
:operands "mem8,xmmreg,imm8"
:code-string "[mri: vex.128.66.0f3a.w0 14 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPEXTRW-reg64.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRW"
:operands "reg64,xmmreg,imm8"
:code-string "[rmi: vex.128.66.0f.w0 c5 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG")))

(setf VPEXTRW-reg32.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRW"
:operands "reg32,xmmreg,imm8"
:code-string "[rmi: vex.128.66.0f.w0 c5 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPEXTRW-reg64.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRW"
:operands "reg64,xmmreg,imm8"
:code-string "[mri: vex.128.66.0f3a.w0 15 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG")))

(setf VPEXTRW-reg32.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRW"
:operands "reg32,xmmreg,imm8"
:code-string "[mri: vex.128.66.0f3a.w0 15 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPEXTRW-mem16.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRW"
:operands "mem16,xmmreg,imm8"
:code-string "[mri: vex.128.66.0f3a.w0 15 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPEXTRD-reg64.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRD"
:operands "reg64,xmmreg,imm8"
:code-string "[mri: vex.128.66.0f3a.w0 16 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG")))

(setf VPEXTRD-rm32.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRD"
:operands "rm32,xmmreg,imm8"
:code-string "[mri: vex.128.66.0f3a.w0 16 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPEXTRQ-rm64.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRQ"
:operands "rm64,xmmreg,imm8"
:code-string "[mri: vex.128.66.0f3a.w1 16 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG")))

(setf VPHADDW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPHADDW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 01 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPHADDD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPHADDD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 02 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPHADDSW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPHADDSW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 03 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPHMINPOSUW-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPHMINPOSUW"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.66.0f38 41 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPHSUBW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPHSUBW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 05 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPHSUBD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPHSUBD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 06 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPHSUBSW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPHSUBSW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 07 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPINSRB-xmmreg.xmmreg*.mem8.imm8 (make-instance 'x86-asm-instruction
:name "VPINSRB"
:operands "xmmreg,xmmreg*,mem8,imm8"
:code-string "[rvmi: vex.nds.128.66.0f3a 20 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPINSRB-xmmreg.xmmreg*.rm8.imm8 (make-instance 'x86-asm-instruction
:name "VPINSRB"
:operands "xmmreg,xmmreg*,rm8,imm8"
:code-string "[rvmi: vex.nds.128.66.0f3a 20 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPINSRB-xmmreg.xmmreg*.reg32.imm8 (make-instance 'x86-asm-instruction
:name "VPINSRB"
:operands "xmmreg,xmmreg*,reg32,imm8"
:code-string "[rvmi: vex.nds.128.66.0f3a 20 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPINSRW-xmmreg.xmmreg*.mem16.imm8 (make-instance 'x86-asm-instruction
:name "VPINSRW"
:operands "xmmreg,xmmreg*,mem16,imm8"
:code-string "[rvmi: vex.nds.128.66.0f c4 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPINSRW-xmmreg.xmmreg*.rm16.imm8 (make-instance 'x86-asm-instruction
:name "VPINSRW"
:operands "xmmreg,xmmreg*,rm16,imm8"
:code-string "[rvmi: vex.nds.128.66.0f c4 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPINSRW-xmmreg.xmmreg*.reg32.imm8 (make-instance 'x86-asm-instruction
:name "VPINSRW"
:operands "xmmreg,xmmreg*,reg32,imm8"
:code-string "[rvmi: vex.nds.128.66.0f c4 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPINSRD-xmmreg.xmmreg*.mem32.imm8 (make-instance 'x86-asm-instruction
:name "VPINSRD"
:operands "xmmreg,xmmreg*,mem32,imm8"
:code-string "[rvmi: vex.nds.128.66.0f3a.w0 22 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPINSRD-xmmreg.xmmreg*.rm32.imm8 (make-instance 'x86-asm-instruction
:name "VPINSRD"
:operands "xmmreg,xmmreg*,rm32,imm8"
:code-string "[rvmi: vex.nds.128.66.0f3a.w0 22 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPINSRQ-xmmreg.xmmreg*.mem64.imm8 (make-instance 'x86-asm-instruction
:name "VPINSRQ"
:operands "xmmreg,xmmreg*,mem64,imm8"
:code-string "[rvmi: vex.nds.128.66.0f3a.w1 22 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG")))

(setf VPINSRQ-xmmreg.xmmreg*.rm64.imm8 (make-instance 'x86-asm-instruction
:name "VPINSRQ"
:operands "xmmreg,xmmreg*,rm64,imm8"
:code-string "[rvmi: vex.nds.128.66.0f3a.w1 22 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG")))

(setf VPMADDWD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMADDWD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f f5 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMADDUBSW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMADDUBSW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 04 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMAXSB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMAXSB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 3c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMAXSW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMAXSW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f ee /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMAXSD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMAXSD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 3d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMAXUB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMAXUB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f de /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMAXUW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMAXUW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 3e /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMAXUD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMAXUD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 3f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMINSB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMINSB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 38 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMINSW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMINSW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f ea /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMINSD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMINSD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 39 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMINUB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMINUB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f da /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMINUW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMINUW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 3a /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMINUD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMINUD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 3b /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMOVMSKB-reg64.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVMSKB"
:operands "reg64,xmmreg"
:code-string "[rm: vex.128.66.0f d7 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE" "LONG")))

(setf VPMOVMSKB-reg32.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVMSKB"
:operands "reg32,xmmreg"
:code-string "[rm: vex.128.66.0f d7 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMOVSXBW-xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VPMOVSXBW"
:operands "xmmreg,xmmrm64"
:code-string "[rm: vex.128.66.0f38 20 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMOVSXBD-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VPMOVSXBD"
:operands "xmmreg,xmmrm32"
:code-string "[rm: vex.128.66.0f38 21 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMOVSXBQ-xmmreg.xmmrm16 (make-instance 'x86-asm-instruction
:name "VPMOVSXBQ"
:operands "xmmreg,xmmrm16"
:code-string "[rm: vex.128.66.0f38 22 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMOVSXWD-xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VPMOVSXWD"
:operands "xmmreg,xmmrm64"
:code-string "[rm: vex.128.66.0f38 23 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMOVSXWQ-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VPMOVSXWQ"
:operands "xmmreg,xmmrm32"
:code-string "[rm: vex.128.66.0f38 24 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMOVSXDQ-xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VPMOVSXDQ"
:operands "xmmreg,xmmrm64"
:code-string "[rm: vex.128.66.0f38 25 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMOVZXBW-xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VPMOVZXBW"
:operands "xmmreg,xmmrm64"
:code-string "[rm: vex.128.66.0f38 30 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMOVZXBD-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VPMOVZXBD"
:operands "xmmreg,xmmrm32"
:code-string "[rm: vex.128.66.0f38 31 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMOVZXBQ-xmmreg.xmmrm16 (make-instance 'x86-asm-instruction
:name "VPMOVZXBQ"
:operands "xmmreg,xmmrm16"
:code-string "[rm: vex.128.66.0f38 32 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMOVZXWD-xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VPMOVZXWD"
:operands "xmmreg,xmmrm64"
:code-string "[rm: vex.128.66.0f38 33 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMOVZXWQ-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VPMOVZXWQ"
:operands "xmmreg,xmmrm32"
:code-string "[rm: vex.128.66.0f38 34 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMOVZXDQ-xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VPMOVZXDQ"
:operands "xmmreg,xmmrm64"
:code-string "[rm: vex.128.66.0f38 35 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMULHUW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMULHUW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f e4 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMULHRSW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMULHRSW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 0b /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMULHW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMULHW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f e5 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMULLW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMULLW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f d5 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMULLD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMULLD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 40 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMULUDQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMULUDQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f f4 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPMULDQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMULDQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 28 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPOR-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPOR"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f eb /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSADBW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSADBW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f f6 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSHUFB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSHUFB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 00 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSHUFD-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPSHUFD"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[rmi: vex.128.66.0f 70 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSHUFHW-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPSHUFHW"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[rmi: vex.128.f3.0f 70 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSHUFLW-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPSHUFLW"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[rmi: vex.128.f2.0f 70 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSIGNB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSIGNB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 08 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSIGNW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSIGNW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 09 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSIGND-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSIGND"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38 0a /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSLLDQ-xmmreg.xmmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLDQ"
:operands "xmmreg,xmmreg*,imm8"
:code-string "[vmi: vex.ndd.128.66.0f 73 /7 ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSRLDQ-xmmreg.xmmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLDQ"
:operands "xmmreg,xmmreg*,imm8"
:code-string "[vmi: vex.ndd.128.66.0f 73 /3 ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSLLW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSLLW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f f1 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSLLW-xmmreg.xmmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLW"
:operands "xmmreg,xmmreg*,imm8"
:code-string "[vmi: vex.ndd.128.66.0f 71 /6 ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSLLD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSLLD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f f2 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSLLD-xmmreg.xmmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLD"
:operands "xmmreg,xmmreg*,imm8"
:code-string "[vmi: vex.ndd.128.66.0f 72 /6 ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSLLQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSLLQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f f3 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSLLQ-xmmreg.xmmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLQ"
:operands "xmmreg,xmmreg*,imm8"
:code-string "[vmi: vex.ndd.128.66.0f 73 /6 ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSRAW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRAW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f e1 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSRAW-xmmreg.xmmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSRAW"
:operands "xmmreg,xmmreg*,imm8"
:code-string "[vmi: vex.ndd.128.66.0f 71 /4 ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSRAD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRAD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f e2 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSRAD-xmmreg.xmmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSRAD"
:operands "xmmreg,xmmreg*,imm8"
:code-string "[vmi: vex.ndd.128.66.0f 72 /4 ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSRLW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRLW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f d1 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSRLW-xmmreg.xmmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLW"
:operands "xmmreg,xmmreg*,imm8"
:code-string "[vmi: vex.ndd.128.66.0f 71 /2 ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSRLD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRLD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f d2 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSRLD-xmmreg.xmmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLD"
:operands "xmmreg,xmmreg*,imm8"
:code-string "[vmi: vex.ndd.128.66.0f 72 /2 ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSRLQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRLQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f d3 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSRLQ-xmmreg.xmmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLQ"
:operands "xmmreg,xmmreg*,imm8"
:code-string "[vmi: vex.ndd.128.66.0f 73 /2 ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPTEST-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPTEST"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.66.0f38 17 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPTEST-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPTEST"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.66.0f38 17 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSUBB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSUBB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f f8 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSUBW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSUBW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f f9 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSUBD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSUBD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f fa /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSUBQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSUBQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f fb /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSUBSB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSUBSB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f e8 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSUBSW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSUBSW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f e9 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSUBUSB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSUBUSB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f d8 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPSUBUSW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSUBUSW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f d9 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPUNPCKHBW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPUNPCKHBW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 68 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPUNPCKHWD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPUNPCKHWD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 69 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPUNPCKHDQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPUNPCKHDQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 6a /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPUNPCKHQDQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPUNPCKHQDQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 6d /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPUNPCKLBW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPUNPCKLBW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 60 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPUNPCKLWD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPUNPCKLWD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 61 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPUNPCKLDQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPUNPCKLDQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 62 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPUNPCKLQDQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPUNPCKLQDQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 6c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPXOR-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPXOR"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f ef /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VRCPPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VRCPPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.0f 53 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VRCPPS-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VRCPPS"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.0f 53 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VRCPSS-xmmreg.xmmreg*.xmmrm32 (make-instance 'x86-asm-instruction
:name "VRCPSS"
:operands "xmmreg,xmmreg*,xmmrm32"
:code-string "[rvm: vex.nds.lig.f3.0f 53 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VRSQRTPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VRSQRTPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.0f 52 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VRSQRTPS-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VRSQRTPS"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.0f 52 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VRSQRTSS-xmmreg.xmmreg*.xmmrm32 (make-instance 'x86-asm-instruction
:name "VRSQRTSS"
:operands "xmmreg,xmmreg*,xmmrm32"
:code-string "[rvm: vex.nds.lig.f3.0f 52 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VROUNDPD-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VROUNDPD"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[rmi: vex.128.66.0f3a 09 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VROUNDPD-ymmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VROUNDPD"
:operands "ymmreg,ymmrm256,imm8"
:code-string "[rmi: vex.256.66.0f3a 09 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VROUNDPS-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VROUNDPS"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[rmi: vex.128.66.0f3a 08 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VROUNDPS-ymmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VROUNDPS"
:operands "ymmreg,ymmrm256,imm8"
:code-string "[rmi: vex.256.66.0f3a 08 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VROUNDSD-xmmreg.xmmreg*.xmmrm64.imm8 (make-instance 'x86-asm-instruction
:name "VROUNDSD"
:operands "xmmreg,xmmreg*,xmmrm64,imm8"
:code-string "[rvmi: vex.nds.128.66.0f3a 0b /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VROUNDSS-xmmreg.xmmreg*.xmmrm32.imm8 (make-instance 'x86-asm-instruction
:name "VROUNDSS"
:operands "xmmreg,xmmreg*,xmmrm32,imm8"
:code-string "[rvmi: vex.nds.128.66.0f3a 0a /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VSHUFPD-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VSHUFPD"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: vex.nds.128.66.0f c6 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VSHUFPD-ymmreg.ymmreg*.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VSHUFPD"
:operands "ymmreg,ymmreg*,ymmrm256,imm8"
:code-string "[rvmi: vex.nds.256.66.0f c6 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VSHUFPS-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VSHUFPS"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: vex.nds.128.0f c6 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VSHUFPS-ymmreg.ymmreg*.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VSHUFPS"
:operands "ymmreg,ymmreg*,ymmrm256,imm8"
:code-string "[rvmi: vex.nds.256.0f c6 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VSQRTPD-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VSQRTPD"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.66.0f 51 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VSQRTPD-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VSQRTPD"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.66.0f 51 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VSQRTPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VSQRTPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.0f 51 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VSQRTPS-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VSQRTPS"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.0f 51 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VSQRTSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VSQRTSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f 51 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VSQRTSS-xmmreg.xmmreg*.xmmrm32 (make-instance 'x86-asm-instruction
:name "VSQRTSS"
:operands "xmmreg,xmmreg*,xmmrm32"
:code-string "[rvm: vex.nds.lig.f3.0f 51 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VSTMXCSR-mem32 (make-instance 'x86-asm-instruction
:name "VSTMXCSR"
:operands "mem32"
:code-string "[m: vex.128.0f ae /3]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VSUBPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VSUBPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 5c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VSUBPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VSUBPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 5c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VSUBPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VSUBPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f 5c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VSUBPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VSUBPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f 5c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VSUBSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x86-asm-instruction
:name "VSUBSD"
:operands "xmmreg,xmmreg*,xmmrm64"
:code-string "[rvm: vex.nds.lig.f2.0f 5c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VSUBSS-xmmreg.xmmreg*.xmmrm32 (make-instance 'x86-asm-instruction
:name "VSUBSS"
:operands "xmmreg,xmmreg*,xmmrm32"
:code-string "[rvm: vex.nds.lig.f3.0f 5c /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VTESTPS-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VTESTPS"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.66.0f38.w0 0e /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VTESTPS-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VTESTPS"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.66.0f38.w0 0e /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VTESTPD-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VTESTPD"
:operands "xmmreg,xmmrm128"
:code-string "[rm: vex.128.66.0f38.w0 0f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VTESTPD-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VTESTPD"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.66.0f38.w0 0f /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VUCOMISD-xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VUCOMISD"
:operands "xmmreg,xmmrm64"
:code-string "[rm: vex.lig.66.0f 2e /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VUCOMISS-xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VUCOMISS"
:operands "xmmreg,xmmrm32"
:code-string "[rm: vex.lig.0f 2e /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VUNPCKHPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VUNPCKHPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 15 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VUNPCKHPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VUNPCKHPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 15 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VUNPCKHPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VUNPCKHPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f 15 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VUNPCKHPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VUNPCKHPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f 15 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VUNPCKLPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VUNPCKLPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 14 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VUNPCKLPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VUNPCKLPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 14 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VUNPCKLPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VUNPCKLPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f 14 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VUNPCKLPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VUNPCKLPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f 14 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VXORPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VXORPD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f 57 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VXORPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VXORPD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 57 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VXORPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VXORPS"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.0f 57 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VXORPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VXORPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.0f 57 /r]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VZEROALL-void (make-instance 'x86-asm-instruction
:name "VZEROALL"
:operands "void"
:code-string "[ vex.256.0f.w0 77]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VZEROUPPER-void (make-instance 'x86-asm-instruction
:name "VZEROUPPER"
:operands "void"
:code-string "[ vex.128.0f.w0 77]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf PCLMULLQLQDQ-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "PCLMULLQLQDQ"
:operands "xmmreg,xmmrm128"
:code-string "[rm: 66 0f 3a 44 /r 00]"
:arch-flags (list "SSE" "WESTMERE")))

(setf PCLMULHQLQDQ-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "PCLMULHQLQDQ"
:operands "xmmreg,xmmrm128"
:code-string "[rm: 66 0f 3a 44 /r 01]"
:arch-flags (list "SSE" "WESTMERE")))

(setf PCLMULLQHQDQ-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "PCLMULLQHQDQ"
:operands "xmmreg,xmmrm128"
:code-string "[rm: 66 0f 3a 44 /r 10]"
:arch-flags (list "SSE" "WESTMERE")))

(setf PCLMULHQHQDQ-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "PCLMULHQHQDQ"
:operands "xmmreg,xmmrm128"
:code-string "[rm: 66 0f 3a 44 /r 11]"
:arch-flags (list "SSE" "WESTMERE")))

(setf PCLMULQDQ-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "PCLMULQDQ"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[rmi: 66 0f 3a 44 /r ib]"
:arch-flags (list "SSE" "WESTMERE")))

(setf VPCLMULLQLQDQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPCLMULLQLQDQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f3a 44 /r 00]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPCLMULHQLQDQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPCLMULHQLQDQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f3a 44 /r 01]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPCLMULLQHQDQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPCLMULLQHQDQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f3a 44 /r 10]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPCLMULHQHQDQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPCLMULHQHQDQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f3a 44 /r 11]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VPCLMULQDQ-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPCLMULQDQ"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: vex.nds.128.66.0f3a 44 /r ib]"
:arch-flags (list "AVX" "SANDYBRIDGE")))

(setf VFMADD132PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADD132PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 98 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD132PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADD132PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 98 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD132PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADD132PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 98 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD132PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADD132PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 98 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD312PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADD312PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 98 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD312PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADD312PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 98 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD312PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADD312PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 98 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD312PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADD312PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 98 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD213PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADD213PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 a8 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD213PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADD213PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 a8 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD213PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADD213PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 a8 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD213PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADD213PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 a8 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD123PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADD123PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 a8 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD123PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADD123PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 a8 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD123PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADD123PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 a8 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD123PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADD123PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 a8 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD231PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADD231PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 b8 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD231PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADD231PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 b8 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD231PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADD231PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 b8 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD231PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADD231PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 b8 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD321PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADD321PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 b8 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD321PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADD321PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 b8 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD321PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADD321PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 b8 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD321PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADD321PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 b8 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB132PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADDSUB132PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 96 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB132PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADDSUB132PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 96 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB132PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADDSUB132PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 96 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB132PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADDSUB132PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 96 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB312PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADDSUB312PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 96 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB312PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADDSUB312PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 96 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB312PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADDSUB312PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 96 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB312PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADDSUB312PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 96 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB213PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADDSUB213PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 a6 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB213PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADDSUB213PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 a6 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB213PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADDSUB213PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 a6 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB213PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADDSUB213PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 a6 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB123PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADDSUB123PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 a6 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB123PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADDSUB123PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 a6 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB123PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADDSUB123PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 a6 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB123PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADDSUB123PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 a6 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB231PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADDSUB231PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 b6 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB231PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADDSUB231PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 b6 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB231PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADDSUB231PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 b6 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB231PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADDSUB231PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 b6 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB321PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADDSUB321PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 b6 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB321PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADDSUB321PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 b6 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB321PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADDSUB321PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 b6 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADDSUB321PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADDSUB321PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 b6 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB132PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUB132PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 9a /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB132PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUB132PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 9a /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB132PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUB132PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 9a /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB132PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUB132PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 9a /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB312PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUB312PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 9a /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB312PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUB312PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 9a /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB312PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUB312PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 9a /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB312PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUB312PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 9a /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB213PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUB213PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 aa /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB213PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUB213PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 aa /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB213PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUB213PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 aa /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB213PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUB213PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 aa /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB123PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUB123PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 aa /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB123PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUB123PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 aa /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB123PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUB123PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 aa /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB123PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUB123PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 aa /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB231PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUB231PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 ba /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB231PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUB231PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 ba /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB231PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUB231PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 ba /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB231PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUB231PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 ba /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB321PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUB321PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 ba /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB321PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUB321PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 ba /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB321PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUB321PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 ba /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB321PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUB321PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 ba /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD132PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUBADD132PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 97 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD132PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUBADD132PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 97 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD132PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUBADD132PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 97 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD132PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUBADD132PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 97 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD312PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUBADD312PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 97 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD312PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUBADD312PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 97 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD312PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUBADD312PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 97 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD312PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUBADD312PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 97 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD213PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUBADD213PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 a7 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD213PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUBADD213PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 a7 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD213PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUBADD213PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 a7 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD213PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUBADD213PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 a7 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD123PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUBADD123PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 a7 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD123PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUBADD123PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 a7 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD123PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUBADD123PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 a7 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD123PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUBADD123PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 a7 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD231PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUBADD231PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 b7 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD231PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUBADD231PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 b7 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD231PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUBADD231PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 b7 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD231PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUBADD231PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 b7 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD321PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUBADD321PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 b7 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD321PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUBADD321PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 b7 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD321PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUBADD321PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 b7 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUBADD321PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUBADD321PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 b7 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD132PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMADD132PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 9c /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD132PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMADD132PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 9c /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD132PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMADD132PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 9c /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD132PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMADD132PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 9c /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD312PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMADD312PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 9c /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD312PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMADD312PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 9c /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD312PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMADD312PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 9c /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD312PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMADD312PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 9c /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD213PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMADD213PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 ac /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD213PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMADD213PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 ac /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD213PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMADD213PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 ac /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD213PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMADD213PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 ac /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD123PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMADD123PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 ac /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD123PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMADD123PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 ac /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD123PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMADD123PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 ac /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD123PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMADD123PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 ac /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD231PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMADD231PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 bc /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD231PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMADD231PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 bc /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD231PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMADD231PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 bc /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD231PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMADD231PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 bc /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD321PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMADD321PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 bc /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD321PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMADD321PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 bc /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD321PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMADD321PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 bc /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD321PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMADD321PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 bc /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB132PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMSUB132PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 9e /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB132PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMSUB132PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 9e /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB132PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMSUB132PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 9e /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB132PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMSUB132PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 9e /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB312PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMSUB312PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 9e /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB312PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMSUB312PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 9e /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB312PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMSUB312PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 9e /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB312PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMSUB312PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 9e /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB213PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMSUB213PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 ae /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB213PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMSUB213PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 ae /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB213PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMSUB213PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 ae /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB213PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMSUB213PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 ae /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB123PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMSUB123PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 ae /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB123PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMSUB123PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 ae /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB123PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMSUB123PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 ae /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB123PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMSUB123PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 ae /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB231PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMSUB231PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 be /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB231PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMSUB231PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 be /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB231PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMSUB231PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 be /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB231PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMSUB231PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 be /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB321PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMSUB321PS"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w0 be /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB321PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMSUB321PS"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w0 be /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB321PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMSUB321PD"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm: vex.dds.128.66.0f38.w1 be /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB321PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMSUB321PD"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm: vex.dds.256.66.0f38.w1 be /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD132SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFMADD132SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 99 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD132SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFMADD132SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 99 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD312SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFMADD312SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 99 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD312SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFMADD312SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 99 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD213SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFMADD213SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 a9 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD213SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFMADD213SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 a9 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD123SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFMADD123SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 a9 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD123SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFMADD123SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 a9 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD231SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFMADD231SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 b9 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD231SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFMADD231SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 b9 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD321SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFMADD321SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 b9 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMADD321SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFMADD321SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 b9 /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB132SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFMSUB132SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 9b /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB132SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFMSUB132SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 9b /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB312SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFMSUB312SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 9b /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB312SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFMSUB312SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 9b /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB213SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFMSUB213SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 ab /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB213SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFMSUB213SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 ab /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB123SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFMSUB123SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 ab /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB123SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFMSUB123SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 ab /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB231SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFMSUB231SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 bb /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB231SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFMSUB231SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 bb /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB321SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFMSUB321SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 bb /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFMSUB321SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFMSUB321SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 bb /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD132SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFNMADD132SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 9d /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD132SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFNMADD132SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 9d /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD312SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFNMADD312SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 9d /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD312SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFNMADD312SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 9d /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD213SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFNMADD213SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 ad /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD213SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFNMADD213SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 ad /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD123SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFNMADD123SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 ad /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD123SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFNMADD123SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 ad /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD231SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFNMADD231SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 bd /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD231SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFNMADD231SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 bd /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD321SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFNMADD321SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 bd /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMADD321SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFNMADD321SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 bd /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB132SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFNMSUB132SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 9f /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB132SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFNMSUB132SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 9f /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB312SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFNMSUB312SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 9f /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB312SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFNMSUB312SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 9f /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB213SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFNMSUB213SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 af /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB213SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFNMSUB213SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 af /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB123SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFNMSUB123SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 af /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB123SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFNMSUB123SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 af /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB231SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFNMSUB231SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 bf /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB231SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFNMSUB231SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 bf /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB321SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFNMSUB321SS"
:operands "xmmreg,xmmreg,xmmrm32"
:code-string "[rvm: vex.dds.128.66.0f38.w0 bf /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf VFNMSUB321SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFNMSUB321SD"
:operands "xmmreg,xmmreg,xmmrm64"
:code-string "[rvm: vex.dds.128.66.0f38.w1 bf /r]"
:arch-flags (list "FMA" "FUTURE")))

(setf RDFSBASE-reg32 (make-instance 'x86-asm-instruction
:name "RDFSBASE"
:operands "reg32"
:code-string "[m: norexw f3 0f ae /0]"
:arch-flags (list "LONG" "FUTURE")))

(setf RDFSBASE-reg64 (make-instance 'x86-asm-instruction
:name "RDFSBASE"
:operands "reg64"
:code-string "[m: o64 f3 0f ae /0]"
:arch-flags (list "LONG" "FUTURE")))

(setf RDGSBASE-reg32 (make-instance 'x86-asm-instruction
:name "RDGSBASE"
:operands "reg32"
:code-string "[m: norexw f3 0f ae /1]"
:arch-flags (list "LONG" "FUTURE")))

(setf RDGSBASE-reg64 (make-instance 'x86-asm-instruction
:name "RDGSBASE"
:operands "reg64"
:code-string "[m: o64 f3 0f ae /1]"
:arch-flags (list "LONG" "FUTURE")))

(setf RDRAND-reg16 (make-instance 'x86-asm-instruction
:name "RDRAND"
:operands "reg16"
:code-string "[m: o16 0f c7 /6]"
:arch-flags (list "FUTURE")))

(setf RDRAND-reg32 (make-instance 'x86-asm-instruction
:name "RDRAND"
:operands "reg32"
:code-string "[m: o32 0f c7 /6]"
:arch-flags (list "FUTURE")))

(setf RDRAND-reg64 (make-instance 'x86-asm-instruction
:name "RDRAND"
:operands "reg64"
:code-string "[m: o64 0f c7 /6]"
:arch-flags (list "LONG" "FUTURE")))

(setf WRFSBASE-reg32 (make-instance 'x86-asm-instruction
:name "WRFSBASE"
:operands "reg32"
:code-string "[m: norexw f3 0f ae /2]"
:arch-flags (list "LONG" "FUTURE")))

(setf WRFSBASE-reg64 (make-instance 'x86-asm-instruction
:name "WRFSBASE"
:operands "reg64"
:code-string "[m: o64 f3 0f ae /2]"
:arch-flags (list "LONG" "FUTURE")))

(setf WRGSBASE-reg32 (make-instance 'x86-asm-instruction
:name "WRGSBASE"
:operands "reg32"
:code-string "[m: norexw f3 0f ae /3]"
:arch-flags (list "LONG" "FUTURE")))

(setf WRGSBASE-reg64 (make-instance 'x86-asm-instruction
:name "WRGSBASE"
:operands "reg64"
:code-string "[m: o64 f3 0f ae /3]"
:arch-flags (list "LONG" "FUTURE")))

(setf VCVTPH2PS-ymmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCVTPH2PS"
:operands "ymmreg,xmmrm128"
:code-string "[rm: vex.256.66.0f38.w0 13 /r]"
:arch-flags (list "AVX" "FUTURE")))

(setf VCVTPH2PS-xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCVTPH2PS"
:operands "xmmreg,xmmrm64"
:code-string "[rm: vex.128.66.0f38.w0 13 /r]"
:arch-flags (list "AVX" "FUTURE")))

(setf VCVTPS2PH-xmmrm128.ymmreg.imm8 (make-instance 'x86-asm-instruction
:name "VCVTPS2PH"
:operands "xmmrm128,ymmreg,imm8"
:code-string "[mri: vex.256.66.0f3a.w0 1d /r ib]"
:arch-flags (list "AVX" "FUTURE")))

(setf VCVTPS2PH-xmmrm64.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VCVTPS2PH"
:operands "xmmrm64,xmmreg,imm8"
:code-string "[mri: vex.128.66.0f3a.w0 1d /r ib]"
:arch-flags (list "AVX" "FUTURE")))

(setf ADCX-reg32.rm32 (make-instance 'x86-asm-instruction
:name "ADCX"
:operands "reg32,rm32"
:code-string "[rm: norexw 66 0f 38 f6 /r]"
:arch-flags (list "FUTURE")))

(setf ADCX-reg64.rm64 (make-instance 'x86-asm-instruction
:name "ADCX"
:operands "reg64,rm64"
:code-string "[rm: o64 66 0f 38 f6 /r]"
:arch-flags (list "LONG" "FUTURE")))

(setf ADOX-reg32.rm32 (make-instance 'x86-asm-instruction
:name "ADOX"
:operands "reg32,rm32"
:code-string "[rm: norexw f3 0f 38 f6 /r]"
:arch-flags (list "FUTURE")))

(setf ADOX-reg64.rm64 (make-instance 'x86-asm-instruction
:name "ADOX"
:operands "reg64,rm64"
:code-string "[rm: o64 f3 0f 38 f6 /r]"
:arch-flags (list "LONG" "FUTURE")))

(setf RDSEED-reg16 (make-instance 'x86-asm-instruction
:name "RDSEED"
:operands "reg16"
:code-string "[m: o16 0f c7 /7]"
:arch-flags (list "FUTURE")))

(setf RDSEED-reg32 (make-instance 'x86-asm-instruction
:name "RDSEED"
:operands "reg32"
:code-string "[m: o32 0f c7 /7]"
:arch-flags (list "FUTURE")))

(setf RDSEED-reg64 (make-instance 'x86-asm-instruction
:name "RDSEED"
:operands "reg64"
:code-string "[m: o64 0f c7 /7]"
:arch-flags (list "LONG" "FUTURE")))

(setf CLAC-void (make-instance 'x86-asm-instruction
:name "CLAC"
:operands "void"
:code-string "[ 0f 01 ca]"
:arch-flags (list "PRIV" "FUTURE")))

(setf STAC-void (make-instance 'x86-asm-instruction
:name "STAC"
:operands "void"
:code-string "[ 0f 01 cb]"
:arch-flags (list "PRIV" "FUTURE")))

(setf XSTORE-void (make-instance 'x86-asm-instruction
:name "XSTORE"
:operands "void"
:code-string "[ 0f a7 c0]"
:arch-flags (list "PENT" "CYRIX")))

(setf XCRYPTECB-void (make-instance 'x86-asm-instruction
:name "XCRYPTECB"
:operands "void"
:code-string "[ mustrep 0f a7 c8]"
:arch-flags (list "PENT" "CYRIX")))

(setf XCRYPTCBC-void (make-instance 'x86-asm-instruction
:name "XCRYPTCBC"
:operands "void"
:code-string "[ mustrep 0f a7 d0]"
:arch-flags (list "PENT" "CYRIX")))

(setf XCRYPTCTR-void (make-instance 'x86-asm-instruction
:name "XCRYPTCTR"
:operands "void"
:code-string "[ mustrep 0f a7 d8]"
:arch-flags (list "PENT" "CYRIX")))

(setf XCRYPTCFB-void (make-instance 'x86-asm-instruction
:name "XCRYPTCFB"
:operands "void"
:code-string "[ mustrep 0f a7 e0]"
:arch-flags (list "PENT" "CYRIX")))

(setf XCRYPTOFB-void (make-instance 'x86-asm-instruction
:name "XCRYPTOFB"
:operands "void"
:code-string "[ mustrep 0f a7 e8]"
:arch-flags (list "PENT" "CYRIX")))

(setf MONTMUL-void (make-instance 'x86-asm-instruction
:name "MONTMUL"
:operands "void"
:code-string "[ mustrep 0f a6 c0]"
:arch-flags (list "PENT" "CYRIX")))

(setf XSHA1-void (make-instance 'x86-asm-instruction
:name "XSHA1"
:operands "void"
:code-string "[ mustrep 0f a6 c8]"
:arch-flags (list "PENT" "CYRIX")))

(setf XSHA256-void (make-instance 'x86-asm-instruction
:name "XSHA256"
:operands "void"
:code-string "[ mustrep 0f a6 d0]"
:arch-flags (list "PENT" "CYRIX")))

(setf LLWPCB-reg32 (make-instance 'x86-asm-instruction
:name "LLWPCB"
:operands "reg32"
:code-string "[m: xop.m9.w0.l0.p0 12 /0]"
:arch-flags (list "AMD" "386")))

(setf LLWPCB-reg64 (make-instance 'x86-asm-instruction
:name "LLWPCB"
:operands "reg64"
:code-string "[m: xop.m9.w1.l0.p0 12 /0]"
:arch-flags (list "AMD" "X64")))

(setf SLWPCB-reg32 (make-instance 'x86-asm-instruction
:name "SLWPCB"
:operands "reg32"
:code-string "[m: xop.m9.w0.l0.p0 12 /1]"
:arch-flags (list "AMD" "386")))

(setf SLWPCB-reg64 (make-instance 'x86-asm-instruction
:name "SLWPCB"
:operands "reg64"
:code-string "[m: xop.m9.w1.l0.p0 12 /1]"
:arch-flags (list "AMD" "X64")))

(setf LWPVAL-reg32.rm32.imm32 (make-instance 'x86-asm-instruction
:name "LWPVAL"
:operands "reg32,rm32,imm32"
:code-string "[vmi: xop.m10.w0.ndd.l0.p0 12 /1 id]"
:arch-flags (list "AMD" "386")))

(setf LWPVAL-reg64.rm32.imm32 (make-instance 'x86-asm-instruction
:name "LWPVAL"
:operands "reg64,rm32,imm32"
:code-string "[vmi: xop.m10.w1.ndd.l0.p0 12 /1 id]"
:arch-flags (list "AMD" "X64")))

(setf LWPINS-reg32.rm32.imm32 (make-instance 'x86-asm-instruction
:name "LWPINS"
:operands "reg32,rm32,imm32"
:code-string "[vmi: xop.m10.w0.ndd.l0.p0 12 /0 id]"
:arch-flags (list "AMD" "386")))

(setf LWPINS-reg64.rm32.imm32 (make-instance 'x86-asm-instruction
:name "LWPINS"
:operands "reg64,rm32,imm32"
:code-string "[vmi: xop.m10.w1.ndd.l0.p0 12 /0 id]"
:arch-flags (list "AMD" "X64")))

(setf VFMADDPD-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VFMADDPD"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 69 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMADDPD-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VFMADDPD"
:operands "ymmreg,ymmreg*,ymmrm256,ymmreg"
:code-string "[rvms: vex.m3.w0.nds.l1.p1 69 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMADDPD-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADDPD"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm128"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 69 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMADDPD-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADDPD"
:operands "ymmreg,ymmreg*,ymmreg,ymmrm256"
:code-string "[rvsm: vex.m3.w1.nds.l1.p1 69 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMADDPS-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VFMADDPS"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 68 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMADDPS-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VFMADDPS"
:operands "ymmreg,ymmreg*,ymmrm256,ymmreg"
:code-string "[rvms: vex.m3.w0.nds.l1.p1 68 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMADDPS-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADDPS"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm128"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 68 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMADDPS-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADDPS"
:operands "ymmreg,ymmreg*,ymmreg,ymmrm256"
:code-string "[rvsm: vex.m3.w1.nds.l1.p1 68 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMADDSD-xmmreg.xmmreg*.xmmrm64.xmmreg (make-instance 'x86-asm-instruction
:name "VFMADDSD"
:operands "xmmreg,xmmreg*,xmmrm64,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 6b /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMADDSD-xmmreg.xmmreg*.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFMADDSD"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm64"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 6b /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMADDSS-xmmreg.xmmreg*.xmmrm32.xmmreg (make-instance 'x86-asm-instruction
:name "VFMADDSS"
:operands "xmmreg,xmmreg*,xmmrm32,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 6a /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMADDSS-xmmreg.xmmreg*.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFMADDSS"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm32"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 6a /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMADDSUBPD-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VFMADDSUBPD"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 5d /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMADDSUBPD-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VFMADDSUBPD"
:operands "ymmreg,ymmreg*,ymmrm256,ymmreg"
:code-string "[rvms: vex.m3.w0.nds.l1.p1 5d /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMADDSUBPD-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADDSUBPD"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm128"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 5d /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMADDSUBPD-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADDSUBPD"
:operands "ymmreg,ymmreg*,ymmreg,ymmrm256"
:code-string "[rvsm: vex.m3.w1.nds.l1.p1 5d /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMADDSUBPS-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VFMADDSUBPS"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 5c /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMADDSUBPS-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VFMADDSUBPS"
:operands "ymmreg,ymmreg*,ymmrm256,ymmreg"
:code-string "[rvms: vex.m3.w0.nds.l1.p1 5c /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMADDSUBPS-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMADDSUBPS"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm128"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 5c /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMADDSUBPS-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMADDSUBPS"
:operands "ymmreg,ymmreg*,ymmreg,ymmrm256"
:code-string "[rvsm: vex.m3.w1.nds.l1.p1 5c /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBADDPD-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VFMSUBADDPD"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 5f /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBADDPD-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VFMSUBADDPD"
:operands "ymmreg,ymmreg*,ymmrm256,ymmreg"
:code-string "[rvms: vex.m3.w0.nds.l1.p1 5f /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBADDPD-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUBADDPD"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm128"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 5f /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBADDPD-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUBADDPD"
:operands "ymmreg,ymmreg*,ymmreg,ymmrm256"
:code-string "[rvsm: vex.m3.w1.nds.l1.p1 5f /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBADDPS-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VFMSUBADDPS"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 5e /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBADDPS-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VFMSUBADDPS"
:operands "ymmreg,ymmreg*,ymmrm256,ymmreg"
:code-string "[rvms: vex.m3.w0.nds.l1.p1 5e /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBADDPS-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUBADDPS"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm128"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 5e /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBADDPS-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUBADDPS"
:operands "ymmreg,ymmreg*,ymmreg,ymmrm256"
:code-string "[rvsm: vex.m3.w1.nds.l1.p1 5e /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBPD-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VFMSUBPD"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 6d /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBPD-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VFMSUBPD"
:operands "ymmreg,ymmreg*,ymmrm256,ymmreg"
:code-string "[rvms: vex.m3.w0.nds.l1.p1 6d /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBPD-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUBPD"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm128"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 6d /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBPD-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUBPD"
:operands "ymmreg,ymmreg*,ymmreg,ymmrm256"
:code-string "[rvsm: vex.m3.w1.nds.l1.p1 6d /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBPS-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VFMSUBPS"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 6c /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBPS-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VFMSUBPS"
:operands "ymmreg,ymmreg*,ymmrm256,ymmreg"
:code-string "[rvms: vex.m3.w0.nds.l1.p1 6c /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBPS-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFMSUBPS"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm128"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 6c /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBPS-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFMSUBPS"
:operands "ymmreg,ymmreg*,ymmreg,ymmrm256"
:code-string "[rvsm: vex.m3.w1.nds.l1.p1 6c /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBSD-xmmreg.xmmreg*.xmmrm64.xmmreg (make-instance 'x86-asm-instruction
:name "VFMSUBSD"
:operands "xmmreg,xmmreg*,xmmrm64,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 6f /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBSD-xmmreg.xmmreg*.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFMSUBSD"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm64"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 6f /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBSS-xmmreg.xmmreg*.xmmrm32.xmmreg (make-instance 'x86-asm-instruction
:name "VFMSUBSS"
:operands "xmmreg,xmmreg*,xmmrm32,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 6e /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFMSUBSS-xmmreg.xmmreg*.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFMSUBSS"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm32"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 6e /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMADDPD-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VFNMADDPD"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 79 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMADDPD-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VFNMADDPD"
:operands "ymmreg,ymmreg*,ymmrm256,ymmreg"
:code-string "[rvms: vex.m3.w0.nds.l1.p1 79 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMADDPD-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMADDPD"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm128"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 79 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMADDPD-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMADDPD"
:operands "ymmreg,ymmreg*,ymmreg,ymmrm256"
:code-string "[rvsm: vex.m3.w1.nds.l1.p1 79 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMADDPS-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VFNMADDPS"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 78 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMADDPS-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VFNMADDPS"
:operands "ymmreg,ymmreg*,ymmrm256,ymmreg"
:code-string "[rvms: vex.m3.w0.nds.l1.p1 78 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMADDPS-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMADDPS"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm128"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 78 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMADDPS-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMADDPS"
:operands "ymmreg,ymmreg*,ymmreg,ymmrm256"
:code-string "[rvsm: vex.m3.w1.nds.l1.p1 78 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMADDSD-xmmreg.xmmreg*.xmmrm64.xmmreg (make-instance 'x86-asm-instruction
:name "VFNMADDSD"
:operands "xmmreg,xmmreg*,xmmrm64,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 7b /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMADDSD-xmmreg.xmmreg*.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFNMADDSD"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm64"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 7b /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMADDSS-xmmreg.xmmreg*.xmmrm32.xmmreg (make-instance 'x86-asm-instruction
:name "VFNMADDSS"
:operands "xmmreg,xmmreg*,xmmrm32,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 7a /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMADDSS-xmmreg.xmmreg*.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFNMADDSS"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm32"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 7a /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMSUBPD-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VFNMSUBPD"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 7d /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMSUBPD-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VFNMSUBPD"
:operands "ymmreg,ymmreg*,ymmrm256,ymmreg"
:code-string "[rvms: vex.m3.w0.nds.l1.p1 7d /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMSUBPD-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMSUBPD"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm128"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 7d /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMSUBPD-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMSUBPD"
:operands "ymmreg,ymmreg*,ymmreg,ymmrm256"
:code-string "[rvsm: vex.m3.w1.nds.l1.p1 7d /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMSUBPS-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VFNMSUBPS"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 7c /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMSUBPS-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VFNMSUBPS"
:operands "ymmreg,ymmreg*,ymmrm256,ymmreg"
:code-string "[rvms: vex.m3.w0.nds.l1.p1 7c /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMSUBPS-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VFNMSUBPS"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm128"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 7c /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMSUBPS-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VFNMSUBPS"
:operands "ymmreg,ymmreg*,ymmreg,ymmrm256"
:code-string "[rvsm: vex.m3.w1.nds.l1.p1 7c /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMSUBSD-xmmreg.xmmreg*.xmmrm64.xmmreg (make-instance 'x86-asm-instruction
:name "VFNMSUBSD"
:operands "xmmreg,xmmreg*,xmmrm64,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 7f /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMSUBSD-xmmreg.xmmreg*.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VFNMSUBSD"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm64"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 7f /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMSUBSS-xmmreg.xmmreg*.xmmrm32.xmmreg (make-instance 'x86-asm-instruction
:name "VFNMSUBSS"
:operands "xmmreg,xmmreg*,xmmrm32,xmmreg"
:code-string "[rvms: vex.m3.w0.nds.l0.p1 7e /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFNMSUBSS-xmmreg.xmmreg*.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VFNMSUBSS"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm32"
:code-string "[rvsm: vex.m3.w1.nds.l0.p1 7e /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VFRCZPD-xmmreg.xmmrm128* (make-instance 'x86-asm-instruction
:name "VFRCZPD"
:operands "xmmreg,xmmrm128*"
:code-string "[rm: xop.m9.w0.l0.p0 81 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VFRCZPD-ymmreg.ymmrm256* (make-instance 'x86-asm-instruction
:name "VFRCZPD"
:operands "ymmreg,ymmrm256*"
:code-string "[rm: xop.m9.w0.l1.p0 81 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VFRCZPS-xmmreg.xmmrm128* (make-instance 'x86-asm-instruction
:name "VFRCZPS"
:operands "xmmreg,xmmrm128*"
:code-string "[rm: xop.m9.w0.l0.p0 80 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VFRCZPS-ymmreg.ymmrm256* (make-instance 'x86-asm-instruction
:name "VFRCZPS"
:operands "ymmreg,ymmrm256*"
:code-string "[rm: xop.m9.w0.l1.p0 80 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VFRCZSD-xmmreg.xmmrm64* (make-instance 'x86-asm-instruction
:name "VFRCZSD"
:operands "xmmreg,xmmrm64*"
:code-string "[rm: xop.m9.w0.l0.p0 83 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VFRCZSS-xmmreg.xmmrm32* (make-instance 'x86-asm-instruction
:name "VFRCZSS"
:operands "xmmreg,xmmrm32*"
:code-string "[rm: xop.m9.w0.l0.p0 82 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPCMOV-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VPCMOV"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: xop.m8.w0.nds.l0.p0 a2 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VPCMOV-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VPCMOV"
:operands "ymmreg,ymmreg*,ymmrm256,ymmreg"
:code-string "[rvms: xop.m8.w0.nds.l1.p0 a2 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VPCMOV-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPCMOV"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm128"
:code-string "[rvsm: xop.m8.w1.nds.l0.p0 a2 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VPCMOV-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPCMOV"
:operands "ymmreg,ymmreg*,ymmreg,ymmrm256"
:code-string "[rvsm: xop.m8.w1.nds.l1.p0 a2 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VPCOMB-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPCOMB"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: xop.m8.w0.nds.l0.p0 cc /r ib]"
:arch-flags (list "AMD" "SSE5")))

(setf VPCOMD-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPCOMD"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: xop.m8.w0.nds.l0.p0 ce /r ib]"
:arch-flags (list "AMD" "SSE5")))

(setf VPCOMQ-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPCOMQ"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: xop.m8.w0.nds.l0.p0 cf /r ib]"
:arch-flags (list "AMD" "SSE5")))

(setf VPCOMUB-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPCOMUB"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: xop.m8.w0.nds.l0.p0 ec /r ib]"
:arch-flags (list "AMD" "SSE5")))

(setf VPCOMUD-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPCOMUD"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: xop.m8.w0.nds.l0.p0 ee /r ib]"
:arch-flags (list "AMD" "SSE5")))

(setf VPCOMUQ-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPCOMUQ"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: xop.m8.w0.nds.l0.p0 ef /r ib]"
:arch-flags (list "AMD" "SSE5")))

(setf VPCOMUW-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPCOMUW"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: xop.m8.w0.nds.l0.p0 ed /r ib]"
:arch-flags (list "AMD" "SSE5")))

(setf VPCOMW-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPCOMW"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: xop.m8.w0.nds.l0.p0 cd /r ib]"
:arch-flags (list "AMD" "SSE5")))

(setf VPHADDBD-xmmreg.xmmrm128* (make-instance 'x86-asm-instruction
:name "VPHADDBD"
:operands "xmmreg,xmmrm128*"
:code-string "[rm: xop.m9.w0.l0.p0 c2 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPHADDBQ-xmmreg.xmmrm128* (make-instance 'x86-asm-instruction
:name "VPHADDBQ"
:operands "xmmreg,xmmrm128*"
:code-string "[rm: xop.m9.w0.l0.p0 c3 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPHADDBW-xmmreg.xmmrm128* (make-instance 'x86-asm-instruction
:name "VPHADDBW"
:operands "xmmreg,xmmrm128*"
:code-string "[rm: xop.m9.w0.l0.p0 c1 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPHADDDQ-xmmreg.xmmrm128* (make-instance 'x86-asm-instruction
:name "VPHADDDQ"
:operands "xmmreg,xmmrm128*"
:code-string "[rm: xop.m9.w0.l0.p0 cb /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPHADDUBD-xmmreg.xmmrm128* (make-instance 'x86-asm-instruction
:name "VPHADDUBD"
:operands "xmmreg,xmmrm128*"
:code-string "[rm: xop.m9.w0.l0.p0 d2 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPHADDUBQ-xmmreg.xmmrm128* (make-instance 'x86-asm-instruction
:name "VPHADDUBQ"
:operands "xmmreg,xmmrm128*"
:code-string "[rm: xop.m9.w0.l0.p0 d3 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPHADDUBW-xmmreg.xmmrm128* (make-instance 'x86-asm-instruction
:name "VPHADDUBW"
:operands "xmmreg,xmmrm128*"
:code-string "[rm: xop.m9.w0.l0.p0 d1 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPHADDUDQ-xmmreg.xmmrm128* (make-instance 'x86-asm-instruction
:name "VPHADDUDQ"
:operands "xmmreg,xmmrm128*"
:code-string "[rm: xop.m9.w0.l0.p0 db /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPHADDUWD-xmmreg.xmmrm128* (make-instance 'x86-asm-instruction
:name "VPHADDUWD"
:operands "xmmreg,xmmrm128*"
:code-string "[rm: xop.m9.w0.l0.p0 d6 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPHADDUWQ-xmmreg.xmmrm128* (make-instance 'x86-asm-instruction
:name "VPHADDUWQ"
:operands "xmmreg,xmmrm128*"
:code-string "[rm: xop.m9.w0.l0.p0 d7 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPHADDWD-xmmreg.xmmrm128* (make-instance 'x86-asm-instruction
:name "VPHADDWD"
:operands "xmmreg,xmmrm128*"
:code-string "[rm: xop.m9.w0.l0.p0 c6 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPHADDWQ-xmmreg.xmmrm128* (make-instance 'x86-asm-instruction
:name "VPHADDWQ"
:operands "xmmreg,xmmrm128*"
:code-string "[rm: xop.m9.w0.l0.p0 c7 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPHSUBBW-xmmreg.xmmrm128* (make-instance 'x86-asm-instruction
:name "VPHSUBBW"
:operands "xmmreg,xmmrm128*"
:code-string "[rm: xop.m9.w0.l0.p0 e1 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPHSUBDQ-xmmreg.xmmrm128* (make-instance 'x86-asm-instruction
:name "VPHSUBDQ"
:operands "xmmreg,xmmrm128*"
:code-string "[rm: xop.m9.w0.l0.p0 e3 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPHSUBWD-xmmreg.xmmrm128* (make-instance 'x86-asm-instruction
:name "VPHSUBWD"
:operands "xmmreg,xmmrm128*"
:code-string "[rm: xop.m9.w0.l0.p0 e2 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPMACSDD-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VPMACSDD"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: xop.m8.w0.nds.l0.p0 9e /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VPMACSDQH-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VPMACSDQH"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: xop.m8.w0.nds.l0.p0 9f /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VPMACSDQL-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VPMACSDQL"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: xop.m8.w0.nds.l0.p0 97 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VPMACSSDD-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VPMACSSDD"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: xop.m8.w0.nds.l0.p0 8e /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VPMACSSDQH-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VPMACSSDQH"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: xop.m8.w0.nds.l0.p0 8f /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VPMACSSDQL-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VPMACSSDQL"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: xop.m8.w0.nds.l0.p0 87 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VPMACSSWD-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VPMACSSWD"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: xop.m8.w0.nds.l0.p0 86 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VPMACSSWW-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VPMACSSWW"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: xop.m8.w0.nds.l0.p0 85 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VPMACSWD-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VPMACSWD"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: xop.m8.w0.nds.l0.p0 96 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VPMACSWW-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VPMACSWW"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: xop.m8.w0.nds.l0.p0 95 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VPMADCSSWD-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VPMADCSSWD"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: xop.m8.w0.nds.l0.p0 a6 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VPMADCSWD-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VPMADCSWD"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: xop.m8.w0.nds.l0.p0 b6 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VPPERM-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPPERM"
:operands "xmmreg,xmmreg*,xmmreg,xmmrm128"
:code-string "[rvsm: xop.m8.w1.nds.l0.p0 a3 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VPPERM-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x86-asm-instruction
:name "VPPERM"
:operands "xmmreg,xmmreg*,xmmrm128,xmmreg"
:code-string "[rvms: xop.m8.w0.nds.l0.p0 a3 /r /is4]"
:arch-flags (list "AMD" "SSE5")))

(setf VPROTB-xmmreg.xmmrm128*.xmmreg (make-instance 'x86-asm-instruction
:name "VPROTB"
:operands "xmmreg,xmmrm128*,xmmreg"
:code-string "[rmv: xop.m9.w0.nds.l0.p0 90 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPROTB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPROTB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: xop.m9.w1.nds.l0.p0 90 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPROTB-xmmreg.xmmrm128*.imm8 (make-instance 'x86-asm-instruction
:name "VPROTB"
:operands "xmmreg,xmmrm128*,imm8"
:code-string "[rmi: xop.m8.w0.l0.p0 c0 /r ib]"
:arch-flags (list "AMD" "SSE5")))

(setf VPROTD-xmmreg.xmmrm128*.xmmreg (make-instance 'x86-asm-instruction
:name "VPROTD"
:operands "xmmreg,xmmrm128*,xmmreg"
:code-string "[rmv: xop.m9.w0.nds.l0.p0 92 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPROTD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPROTD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: xop.m9.w1.nds.l0.p0 92 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPROTD-xmmreg.xmmrm128*.imm8 (make-instance 'x86-asm-instruction
:name "VPROTD"
:operands "xmmreg,xmmrm128*,imm8"
:code-string "[rmi: xop.m8.w0.l0.p0 c2 /r ib]"
:arch-flags (list "AMD" "SSE5")))

(setf VPROTQ-xmmreg.xmmrm128*.xmmreg (make-instance 'x86-asm-instruction
:name "VPROTQ"
:operands "xmmreg,xmmrm128*,xmmreg"
:code-string "[rmv: xop.m9.w0.nds.l0.p0 93 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPROTQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPROTQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: xop.m9.w1.nds.l0.p0 93 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPROTQ-xmmreg.xmmrm128*.imm8 (make-instance 'x86-asm-instruction
:name "VPROTQ"
:operands "xmmreg,xmmrm128*,imm8"
:code-string "[rmi: xop.m8.w0.l0.p0 c3 /r ib]"
:arch-flags (list "AMD" "SSE5")))

(setf VPROTW-xmmreg.xmmrm128*.xmmreg (make-instance 'x86-asm-instruction
:name "VPROTW"
:operands "xmmreg,xmmrm128*,xmmreg"
:code-string "[rmv: xop.m9.w0.nds.l0.p0 91 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPROTW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPROTW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: xop.m9.w1.nds.l0.p0 91 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPROTW-xmmreg.xmmrm128*.imm8 (make-instance 'x86-asm-instruction
:name "VPROTW"
:operands "xmmreg,xmmrm128*,imm8"
:code-string "[rmi: xop.m8.w0.l0.p0 c1 /r ib]"
:arch-flags (list "AMD" "SSE5")))

(setf VPSHAB-xmmreg.xmmrm128*.xmmreg (make-instance 'x86-asm-instruction
:name "VPSHAB"
:operands "xmmreg,xmmrm128*,xmmreg"
:code-string "[rmv: xop.m9.w0.nds.l0.p0 98 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPSHAB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSHAB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: xop.m9.w1.nds.l0.p0 98 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPSHAD-xmmreg.xmmrm128*.xmmreg (make-instance 'x86-asm-instruction
:name "VPSHAD"
:operands "xmmreg,xmmrm128*,xmmreg"
:code-string "[rmv: xop.m9.w0.nds.l0.p0 9a /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPSHAD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSHAD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: xop.m9.w1.nds.l0.p0 9a /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPSHAQ-xmmreg.xmmrm128*.xmmreg (make-instance 'x86-asm-instruction
:name "VPSHAQ"
:operands "xmmreg,xmmrm128*,xmmreg"
:code-string "[rmv: xop.m9.w0.nds.l0.p0 9b /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPSHAQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSHAQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: xop.m9.w1.nds.l0.p0 9b /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPSHAW-xmmreg.xmmrm128*.xmmreg (make-instance 'x86-asm-instruction
:name "VPSHAW"
:operands "xmmreg,xmmrm128*,xmmreg"
:code-string "[rmv: xop.m9.w0.nds.l0.p0 99 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPSHAW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSHAW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: xop.m9.w1.nds.l0.p0 99 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPSHLB-xmmreg.xmmrm128*.xmmreg (make-instance 'x86-asm-instruction
:name "VPSHLB"
:operands "xmmreg,xmmrm128*,xmmreg"
:code-string "[rmv: xop.m9.w0.nds.l0.p0 94 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPSHLB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSHLB"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: xop.m9.w1.nds.l0.p0 94 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPSHLD-xmmreg.xmmrm128*.xmmreg (make-instance 'x86-asm-instruction
:name "VPSHLD"
:operands "xmmreg,xmmrm128*,xmmreg"
:code-string "[rmv: xop.m9.w0.nds.l0.p0 96 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPSHLD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSHLD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: xop.m9.w1.nds.l0.p0 96 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPSHLQ-xmmreg.xmmrm128*.xmmreg (make-instance 'x86-asm-instruction
:name "VPSHLQ"
:operands "xmmreg,xmmrm128*,xmmreg"
:code-string "[rmv: xop.m9.w0.nds.l0.p0 97 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPSHLQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSHLQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: xop.m9.w1.nds.l0.p0 97 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPSHLW-xmmreg.xmmrm128*.xmmreg (make-instance 'x86-asm-instruction
:name "VPSHLW"
:operands "xmmreg,xmmrm128*,xmmreg"
:code-string "[rmv: xop.m9.w0.nds.l0.p0 95 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VPSHLW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSHLW"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: xop.m9.w1.nds.l0.p0 95 /r]"
:arch-flags (list "AMD" "SSE5")))

(setf VMPSADBW-ymmreg.ymmreg*.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VMPSADBW"
:operands "ymmreg,ymmreg*,ymmrm256,imm8"
:code-string "[rvmi: vex.nds.256.66.0f3a 42 /r ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPABSB-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPABSB"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.66.0f38 1c /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPABSW-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPABSW"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.66.0f38 1d /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPABSD-ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPABSD"
:operands "ymmreg,ymmrm256"
:code-string "[rm: vex.256.66.0f38 1e /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPACKSSWB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPACKSSWB"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 63 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPACKSSDW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPACKSSDW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 6b /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPACKUSDW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPACKUSDW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 2b /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPACKUSWB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPACKUSWB"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 67 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPADDB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPADDB"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f fc /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPADDW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPADDW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f fd /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPADDD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPADDD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f fe /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPADDQ-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPADDQ"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f d4 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPADDSB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPADDSB"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f ec /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPADDSW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPADDSW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f ed /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPADDUSB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPADDUSB"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f dc /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPADDUSW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPADDUSW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f dd /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPALIGNR-ymmreg.ymmreg*.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPALIGNR"
:operands "ymmreg,ymmreg*,ymmrm256,imm8"
:code-string "[rvmi: vex.nds.256.66.0f3a 0f /r ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPAND-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPAND"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f db /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPANDN-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPANDN"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f df /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPAVGB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPAVGB"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f e0 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPAVGW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPAVGW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f e3 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBLENDVB-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x86-asm-instruction
:name "VPBLENDVB"
:operands "ymmreg,ymmreg*,ymmrm256,ymmreg"
:code-string "[rvms: vex.nds.256.66.0f3a 4c /r /is4]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBLENDW-ymmreg.ymmreg*.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPBLENDW"
:operands "ymmreg,ymmreg*,ymmrm256,imm8"
:code-string "[rvmi: vex.nds.256.66.0f3a 0e /r ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPCMPEQB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPCMPEQB"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 74 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPCMPEQW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPCMPEQW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 75 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPCMPEQD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPCMPEQD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 76 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPCMPEQQ-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPCMPEQQ"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 29 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPCMPGTB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPCMPGTB"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 64 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPCMPGTW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPCMPGTW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 65 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPCMPGTD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPCMPGTD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 66 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPCMPGTQ-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPCMPGTQ"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 37 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPHADDW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPHADDW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 01 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPHADDD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPHADDD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 02 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPHADDSW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPHADDSW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 03 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPHSUBW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPHSUBW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 05 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPHSUBD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPHSUBD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 06 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPHSUBSW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPHSUBSW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 07 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMADDUBSW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMADDUBSW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 04 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMADDWD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMADDWD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f f5 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMAXSB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMAXSB"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 3c /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMAXSW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMAXSW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f ee /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMAXSD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMAXSD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 3d /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMAXUB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMAXUB"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f de /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMAXUW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMAXUW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 3e /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMAXUD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMAXUD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 3f /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMINSB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMINSB"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 38 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMINSW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMINSW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f ea /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMINSD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMINSD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 39 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMINUB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMINUB"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f da /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMINUW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMINUW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 3a /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMINUD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMINUD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 3b /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVMSKB-reg32.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVMSKB"
:operands "reg32,ymmreg"
:code-string "[rm: vex.256.66.0f d7 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVMSKB-reg64.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVMSKB"
:operands "reg64,ymmreg"
:code-string "[rm: vex.256.66.0f d7 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVSXBW-ymmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMOVSXBW"
:operands "ymmreg,xmmrm128"
:code-string "[rm: vex.256.66.0f38 20 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVSXBD-ymmreg.mem64 (make-instance 'x86-asm-instruction
:name "VPMOVSXBD"
:operands "ymmreg,mem64"
:code-string "[rm: vex.256.66.0f38 21 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVSXBD-ymmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSXBD"
:operands "ymmreg,xmmreg"
:code-string "[rm: vex.256.66.0f38 21 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVSXBQ-ymmreg.mem32 (make-instance 'x86-asm-instruction
:name "VPMOVSXBQ"
:operands "ymmreg,mem32"
:code-string "[rm: vex.256.66.0f38 22 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVSXBQ-ymmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSXBQ"
:operands "ymmreg,xmmreg"
:code-string "[rm: vex.256.66.0f38 22 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVSXWD-ymmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMOVSXWD"
:operands "ymmreg,xmmrm128"
:code-string "[rm: vex.256.66.0f38 23 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVSXWQ-ymmreg.mem64 (make-instance 'x86-asm-instruction
:name "VPMOVSXWQ"
:operands "ymmreg,mem64"
:code-string "[rm: vex.256.66.0f38 24 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVSXWQ-ymmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSXWQ"
:operands "ymmreg,xmmreg"
:code-string "[rm: vex.256.66.0f38 24 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVSXDQ-ymmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMOVSXDQ"
:operands "ymmreg,xmmrm128"
:code-string "[rm: vex.256.66.0f38 25 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVZXBW-ymmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMOVZXBW"
:operands "ymmreg,xmmrm128"
:code-string "[rm: vex.256.66.0f38 30 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVZXBD-ymmreg.mem64 (make-instance 'x86-asm-instruction
:name "VPMOVZXBD"
:operands "ymmreg,mem64"
:code-string "[rm: vex.256.66.0f38 31 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVZXBD-ymmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVZXBD"
:operands "ymmreg,xmmreg"
:code-string "[rm: vex.256.66.0f38 31 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVZXBQ-ymmreg.mem32 (make-instance 'x86-asm-instruction
:name "VPMOVZXBQ"
:operands "ymmreg,mem32"
:code-string "[rm: vex.256.66.0f38 32 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVZXBQ-ymmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVZXBQ"
:operands "ymmreg,xmmreg"
:code-string "[rm: vex.256.66.0f38 32 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVZXWD-ymmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMOVZXWD"
:operands "ymmreg,xmmrm128"
:code-string "[rm: vex.256.66.0f38 33 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVZXWQ-ymmreg.mem64 (make-instance 'x86-asm-instruction
:name "VPMOVZXWQ"
:operands "ymmreg,mem64"
:code-string "[rm: vex.256.66.0f38 34 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVZXWQ-ymmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVZXWQ"
:operands "ymmreg,xmmreg"
:code-string "[rm: vex.256.66.0f38 34 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMOVZXDQ-ymmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMOVZXDQ"
:operands "ymmreg,xmmrm128"
:code-string "[rm: vex.256.66.0f38 35 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMULDQ-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMULDQ"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 28 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMULHRSW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMULHRSW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 0b /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMULHUW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMULHUW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f e4 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMULHW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMULHW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f e5 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMULLW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMULLW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f d5 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMULLD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMULLD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 40 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMULUDQ-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMULUDQ"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f f4 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPOR-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPOR"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f eb /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSADBW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSADBW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f f6 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSHUFB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSHUFB"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 00 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSHUFD-ymmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPSHUFD"
:operands "ymmreg,ymmrm256,imm8"
:code-string "[rmi: vex.256.66.0f 70 /r ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSHUFHW-ymmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPSHUFHW"
:operands "ymmreg,ymmrm256,imm8"
:code-string "[rmi: vex.256.f3.0f 70 /r ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSHUFLW-ymmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPSHUFLW"
:operands "ymmreg,ymmrm256,imm8"
:code-string "[rmi: vex.256.f2.0f 70 /r ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSIGNB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSIGNB"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 08 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSIGNW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSIGNW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 09 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSIGND-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSIGND"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38 0a /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSLLDQ-ymmreg.ymmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLDQ"
:operands "ymmreg,ymmreg*,imm8"
:code-string "[vmi: vex.ndd.256.66.0f 73 /7 ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSLLW-ymmreg.ymmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSLLW"
:operands "ymmreg,ymmreg*,xmmrm128"
:code-string "[rvm: vex.nds.256.66.0f f1 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSLLW-ymmreg.ymmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLW"
:operands "ymmreg,ymmreg*,imm8"
:code-string "[vmi: vex.ndd.256.66.0f 71 /6 ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSLLD-ymmreg.ymmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSLLD"
:operands "ymmreg,ymmreg*,xmmrm128"
:code-string "[rvm: vex.nds.256.66.0f f2 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSLLD-ymmreg.ymmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLD"
:operands "ymmreg,ymmreg*,imm8"
:code-string "[vmi: vex.ndd.256.66.0f 72 /6 ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSLLQ-ymmreg.ymmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSLLQ"
:operands "ymmreg,ymmreg*,xmmrm128"
:code-string "[rvm: vex.nds.256.66.0f f3 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSLLQ-ymmreg.ymmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLQ"
:operands "ymmreg,ymmreg*,imm8"
:code-string "[vmi: vex.ndd.256.66.0f 73 /6 ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSRAW-ymmreg.ymmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRAW"
:operands "ymmreg,ymmreg*,xmmrm128"
:code-string "[rvm: vex.nds.256.66.0f e1 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSRAW-ymmreg.ymmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSRAW"
:operands "ymmreg,ymmreg*,imm8"
:code-string "[vmi: vex.ndd.256.66.0f 71 /4 ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSRAD-ymmreg.ymmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRAD"
:operands "ymmreg,ymmreg*,xmmrm128"
:code-string "[rvm: vex.nds.256.66.0f e2 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSRAD-ymmreg.ymmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSRAD"
:operands "ymmreg,ymmreg*,imm8"
:code-string "[vmi: vex.ndd.256.66.0f 72 /4 ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSRLDQ-ymmreg.ymmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLDQ"
:operands "ymmreg,ymmreg*,imm8"
:code-string "[vmi: vex.ndd.256.66.0f 73 /3 ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSRLW-ymmreg.ymmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRLW"
:operands "ymmreg,ymmreg*,xmmrm128"
:code-string "[rvm: vex.nds.256.66.0f d1 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSRLW-ymmreg.ymmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLW"
:operands "ymmreg,ymmreg*,imm8"
:code-string "[vmi: vex.ndd.256.66.0f 71 /2 ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSRLD-ymmreg.ymmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRLD"
:operands "ymmreg,ymmreg*,xmmrm128"
:code-string "[rvm: vex.nds.256.66.0f d2 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSRLD-ymmreg.ymmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLD"
:operands "ymmreg,ymmreg*,imm8"
:code-string "[vmi: vex.ndd.256.66.0f 72 /2 ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSRLQ-ymmreg.ymmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRLQ"
:operands "ymmreg,ymmreg*,xmmrm128"
:code-string "[rvm: vex.nds.256.66.0f d3 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSRLQ-ymmreg.ymmreg*.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLQ"
:operands "ymmreg,ymmreg*,imm8"
:code-string "[vmi: vex.ndd.256.66.0f.wig 73 /2 ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSUBB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSUBB"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f f8 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSUBW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSUBW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f f9 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSUBD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSUBD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f fa /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSUBQ-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSUBQ"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f fb /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSUBSB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSUBSB"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f e8 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSUBSW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSUBSW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f e9 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSUBUSB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSUBUSB"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f d8 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSUBUSW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSUBUSW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f d9 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPUNPCKHBW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPUNPCKHBW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 68 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPUNPCKHWD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPUNPCKHWD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 69 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPUNPCKHDQ-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPUNPCKHDQ"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 6a /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPUNPCKHQDQ-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPUNPCKHQDQ"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 6d /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPUNPCKLBW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPUNPCKLBW"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 60 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPUNPCKLWD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPUNPCKLWD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 61 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPUNPCKLDQ-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPUNPCKLDQ"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 62 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPUNPCKLQDQ-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPUNPCKLQDQ"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f 6c /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPXOR-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPXOR"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f ef /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VMOVNTDQA-ymmreg.mem256 (make-instance 'x86-asm-instruction
:name "VMOVNTDQA"
:operands "ymmreg,mem256"
:code-string "[rm: vex.256.66.0f38 2a /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VBROADCASTSS-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VBROADCASTSS"
:operands "xmmreg,xmmreg"
:code-string "[rm: vex.128.66.0f38.w0 18 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VBROADCASTSS-ymmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VBROADCASTSS"
:operands "ymmreg,xmmreg"
:code-string "[rm: vex.256.66.0f38.w0 18 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VBROADCASTSD-ymmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VBROADCASTSD"
:operands "ymmreg,xmmreg"
:code-string "[rm: vex.256.66.0f38.w0 19 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VBROADCASTI128-ymmreg.mem128 (make-instance 'x86-asm-instruction
:name "VBROADCASTI128"
:operands "ymmreg,mem128"
:code-string "[rm: vex.256.66.0f38.w0 5a /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBLENDD-xmmreg.xmmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPBLENDD"
:operands "xmmreg,xmmreg*,xmmrm128,imm8"
:code-string "[rvmi: vex.nds.128.66.0f3a.w0 02 /r ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBLENDD-ymmreg.ymmreg*.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPBLENDD"
:operands "ymmreg,ymmreg*,ymmrm256,imm8"
:code-string "[rvmi: vex.nds.256.66.0f3a.w0 02 /r ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBROADCASTB-xmmreg.mem8 (make-instance 'x86-asm-instruction
:name "VPBROADCASTB"
:operands "xmmreg,mem8"
:code-string "[rm: vex.128.66.0f38.w0 78 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBROADCASTB-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTB"
:operands "xmmreg,xmmreg"
:code-string "[rm: vex.128.66.0f38.w0 78 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBROADCASTB-ymmreg.mem8 (make-instance 'x86-asm-instruction
:name "VPBROADCASTB"
:operands "ymmreg,mem8"
:code-string "[rm: vex.256.66.0f38.w0 78 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBROADCASTB-ymmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTB"
:operands "ymmreg,xmmreg"
:code-string "[rm: vex.256.66.0f38.w0 78 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBROADCASTW-xmmreg.mem16 (make-instance 'x86-asm-instruction
:name "VPBROADCASTW"
:operands "xmmreg,mem16"
:code-string "[rm: vex.128.66.0f38.w0 79 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBROADCASTW-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTW"
:operands "xmmreg,xmmreg"
:code-string "[rm: vex.128.66.0f38.w0 79 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBROADCASTW-ymmreg.mem16 (make-instance 'x86-asm-instruction
:name "VPBROADCASTW"
:operands "ymmreg,mem16"
:code-string "[rm: vex.256.66.0f38.w0 79 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBROADCASTW-ymmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTW"
:operands "ymmreg,xmmreg"
:code-string "[rm: vex.256.66.0f38.w0 79 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBROADCASTD-xmmreg.mem32 (make-instance 'x86-asm-instruction
:name "VPBROADCASTD"
:operands "xmmreg,mem32"
:code-string "[rm: vex.128.66.0f38.w0 58 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBROADCASTD-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTD"
:operands "xmmreg,xmmreg"
:code-string "[rm: vex.128.66.0f38.w0 58 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBROADCASTD-ymmreg.mem32 (make-instance 'x86-asm-instruction
:name "VPBROADCASTD"
:operands "ymmreg,mem32"
:code-string "[rm: vex.256.66.0f38.w0 58 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBROADCASTD-ymmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTD"
:operands "ymmreg,xmmreg"
:code-string "[rm: vex.256.66.0f38.w0 58 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBROADCASTQ-xmmreg.mem64 (make-instance 'x86-asm-instruction
:name "VPBROADCASTQ"
:operands "xmmreg,mem64"
:code-string "[rm: vex.128.66.0f38.w0 59 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBROADCASTQ-xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTQ"
:operands "xmmreg,xmmreg"
:code-string "[rm: vex.128.66.0f38.w0 59 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBROADCASTQ-ymmreg.mem64 (make-instance 'x86-asm-instruction
:name "VPBROADCASTQ"
:operands "ymmreg,mem64"
:code-string "[rm: vex.256.66.0f38.w0 59 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPBROADCASTQ-ymmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTQ"
:operands "ymmreg,xmmreg"
:code-string "[rm: vex.256.66.0f38.w0 59 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPERMD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPERMD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38.w0 36 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPERMPD-ymmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPERMPD"
:operands "ymmreg,ymmrm256,imm8"
:code-string "[rmi: vex.256.66.0f3a.w1 01 /r ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPERMPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPERMPS"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38.w0 16 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPERMQ-ymmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPERMQ"
:operands "ymmreg,ymmrm256,imm8"
:code-string "[rmi: vex.256.66.0f3a.w1 00 /r ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPERM2I128-ymmreg.ymmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPERM2I128"
:operands "ymmreg,ymmreg,ymmrm256,imm8"
:code-string "[rvmi: vex.nds.256.66.0f3a.w0 46 /r ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VEXTRACTI128-xmmrm128.ymmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTI128"
:operands "xmmrm128,ymmreg,imm8"
:code-string "[mri: vex.256.66.0f3a.w0 39 /r ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VINSERTI128-ymmreg.ymmreg*.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VINSERTI128"
:operands "ymmreg,ymmreg*,xmmrm128,imm8"
:code-string "[rvmi: vex.nds.256.66.0f3a.w0 38 /r ib]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMASKMOVD-xmmreg.xmmreg*.mem128 (make-instance 'x86-asm-instruction
:name "VPMASKMOVD"
:operands "xmmreg,xmmreg*,mem128"
:code-string "[rvm: vex.nds.128.66.0f38.w0 8c /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMASKMOVD-ymmreg.ymmreg*.mem256 (make-instance 'x86-asm-instruction
:name "VPMASKMOVD"
:operands "ymmreg,ymmreg*,mem256"
:code-string "[rvm: vex.nds.256.66.0f38.w0 8c /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMASKMOVQ-xmmreg.xmmreg*.mem128 (make-instance 'x86-asm-instruction
:name "VPMASKMOVQ"
:operands "xmmreg,xmmreg*,mem128"
:code-string "[rvm: vex.nds.128.66.0f38.w1 8c /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMASKMOVQ-ymmreg.ymmreg*.mem256 (make-instance 'x86-asm-instruction
:name "VPMASKMOVQ"
:operands "ymmreg,ymmreg*,mem256"
:code-string "[rvm: vex.nds.256.66.0f38.w1 8c /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMASKMOVD-mem128.xmmreg*.xmmreg (make-instance 'x86-asm-instruction
:name "VPMASKMOVD"
:operands "mem128,xmmreg*,xmmreg"
:code-string "[mvr: vex.nds.128.66.0f38.w0 8e /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMASKMOVD-mem256.ymmreg*.ymmreg (make-instance 'x86-asm-instruction
:name "VPMASKMOVD"
:operands "mem256,ymmreg*,ymmreg"
:code-string "[mvr: vex.nds.256.66.0f38.w0 8e /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMASKMOVQ-mem128.xmmreg*.xmmreg (make-instance 'x86-asm-instruction
:name "VPMASKMOVQ"
:operands "mem128,xmmreg*,xmmreg"
:code-string "[mvr: vex.nds.128.66.0f38.w1 8e /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPMASKMOVQ-mem256.ymmreg*.ymmreg (make-instance 'x86-asm-instruction
:name "VPMASKMOVQ"
:operands "mem256,ymmreg*,ymmreg"
:code-string "[mvr: vex.nds.256.66.0f38.w1 8e /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSLLVD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSLLVD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38.w0 47 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSLLVQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSLLVQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38.w1 47 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSLLVD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSLLVD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38.w0 47 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSLLVQ-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSLLVQ"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38.w1 47 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSRAVD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRAVD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38.w0 46 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSRAVD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSRAVD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38.w0 46 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSRLVD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRLVD"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38.w0 45 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSRLVQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRLVQ"
:operands "xmmreg,xmmreg*,xmmrm128"
:code-string "[rvm: vex.nds.128.66.0f38.w1 45 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSRLVD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSRLVD"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38.w0 45 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPSRLVQ-ymmreg.ymmreg*.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSRLVQ"
:operands "ymmreg,ymmreg*,ymmrm256"
:code-string "[rvm: vex.nds.256.66.0f38.w1 45 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VGATHERDPD-xmmreg.xmem64.xmmreg (make-instance 'x86-asm-instruction
:name "VGATHERDPD"
:operands "xmmreg,xmem64,xmmreg"
:code-string "[rmv: vm32x vex.dds.128.66.0f38.w1 92 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VGATHERQPD-xmmreg.xmem64.xmmreg (make-instance 'x86-asm-instruction
:name "VGATHERQPD"
:operands "xmmreg,xmem64,xmmreg"
:code-string "[rmv: vm64x vex.dds.128.66.0f38.w1 93 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VGATHERDPD-ymmreg.xmem64.ymmreg (make-instance 'x86-asm-instruction
:name "VGATHERDPD"
:operands "ymmreg,xmem64,ymmreg"
:code-string "[rmv: vm32x vex.dds.256.66.0f38.w1 92 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VGATHERQPD-ymmreg.ymem64.ymmreg (make-instance 'x86-asm-instruction
:name "VGATHERQPD"
:operands "ymmreg,ymem64,ymmreg"
:code-string "[rmv: vm64y vex.dds.256.66.0f38.w1 93 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VGATHERDPS-xmmreg.xmem32.xmmreg (make-instance 'x86-asm-instruction
:name "VGATHERDPS"
:operands "xmmreg,xmem32,xmmreg"
:code-string "[rmv: vm32x vex.dds.128.66.0f38.w0 92 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VGATHERQPS-xmmreg.xmem32.xmmreg (make-instance 'x86-asm-instruction
:name "VGATHERQPS"
:operands "xmmreg,xmem32,xmmreg"
:code-string "[rmv: vm64x vex.dds.128.66.0f38.w0 93 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VGATHERDPS-ymmreg.ymem32.ymmreg (make-instance 'x86-asm-instruction
:name "VGATHERDPS"
:operands "ymmreg,ymem32,ymmreg"
:code-string "[rmv: vm32y vex.dds.256.66.0f38.w0 92 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VGATHERQPS-xmmreg.ymem32.xmmreg (make-instance 'x86-asm-instruction
:name "VGATHERQPS"
:operands "xmmreg,ymem32,xmmreg"
:code-string "[rmv: vm64y vex.dds.256.66.0f38.w0 93 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPGATHERDD-xmmreg.xmem32.xmmreg (make-instance 'x86-asm-instruction
:name "VPGATHERDD"
:operands "xmmreg,xmem32,xmmreg"
:code-string "[rmv: vm32x vex.dds.128.66.0f38.w0 90 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPGATHERQD-xmmreg.xmem32.xmmreg (make-instance 'x86-asm-instruction
:name "VPGATHERQD"
:operands "xmmreg,xmem32,xmmreg"
:code-string "[rmv: vm64x vex.dds.128.66.0f38.w0 91 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPGATHERDD-ymmreg.ymem32.ymmreg (make-instance 'x86-asm-instruction
:name "VPGATHERDD"
:operands "ymmreg,ymem32,ymmreg"
:code-string "[rmv: vm32y vex.dds.256.66.0f38.w0 90 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPGATHERQD-xmmreg.ymem32.xmmreg (make-instance 'x86-asm-instruction
:name "VPGATHERQD"
:operands "xmmreg,ymem32,xmmreg"
:code-string "[rmv: vm64y vex.dds.256.66.0f38.w0 91 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPGATHERDQ-xmmreg.xmem64.xmmreg (make-instance 'x86-asm-instruction
:name "VPGATHERDQ"
:operands "xmmreg,xmem64,xmmreg"
:code-string "[rmv: vm32x vex.dds.128.66.0f38.w1 90 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPGATHERQQ-xmmreg.xmem64.xmmreg (make-instance 'x86-asm-instruction
:name "VPGATHERQQ"
:operands "xmmreg,xmem64,xmmreg"
:code-string "[rmv: vm64x vex.dds.128.66.0f38.w1 91 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPGATHERDQ-ymmreg.xmem64.ymmreg (make-instance 'x86-asm-instruction
:name "VPGATHERDQ"
:operands "ymmreg,xmem64,ymmreg"
:code-string "[rmv: vm32x vex.dds.256.66.0f38.w1 90 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf VPGATHERQQ-ymmreg.ymem64.ymmreg (make-instance 'x86-asm-instruction
:name "VPGATHERQQ"
:operands "ymmreg,ymem64,ymmreg"
:code-string "[rmv: vm64y vex.dds.256.66.0f38.w1 91 /r]"
:arch-flags (list "FUTURE" "AVX2")))

(setf XABORT-imm (make-instance 'x86-asm-instruction
:name "XABORT"
:operands "imm"
:code-string "[i: c6 f8 ib]"
:arch-flags (list "FUTURE" "RTM")))

(setf XABORT-imm8 (make-instance 'x86-asm-instruction
:name "XABORT"
:operands "imm8"
:code-string "[i: c6 f8 ib]"
:arch-flags (list "FUTURE" "RTM")))

(setf XBEGIN-imm (make-instance 'x86-asm-instruction
:name "XBEGIN"
:operands "imm"
:code-string "[i: odf c7 f8 rel]"
:arch-flags (list "FUTURE" "RTM")))

(setf XBEGIN-imm-near (make-instance 'x86-asm-instruction
:name "XBEGIN"
:operands "imm|near"
:code-string "[i: odf c7 f8 rel]"
:arch-flags (list "FUTURE" "RTM" "ND")))

(setf XBEGIN-imm64 (make-instance 'x86-asm-instruction
:name "XBEGIN"
:operands "imm64"
:code-string "[i: o64nw c7 f8 rel]"
:arch-flags (list "FUTURE" "RTM" "LONG")))

(setf XBEGIN-imm64-near (make-instance 'x86-asm-instruction
:name "XBEGIN"
:operands "imm64|near"
:code-string "[i: o64nw c7 f8 rel]"
:arch-flags (list "FUTURE" "RTM" "LONG" "ND")))

(setf XEND-void (make-instance 'x86-asm-instruction
:name "XEND"
:operands "void"
:code-string "[ 0f 01 d5]"
:arch-flags (list "FUTURE" "RTM")))

(setf XTEST-void (make-instance 'x86-asm-instruction
:name "XTEST"
:operands "void"
:code-string "[ 0f 01 d6]"
:arch-flags (list "FUTURE" "HLE" "RTM")))

(setf ANDN-reg32.reg32.rm32 (make-instance 'x86-asm-instruction
:name "ANDN"
:operands "reg32,reg32,rm32"
:code-string "[rvm: vex.nds.lz.0f38.w0 f2 /r]"
:arch-flags (list "FUTURE" "BMI1")))

(setf ANDN-reg64.reg64.rm64 (make-instance 'x86-asm-instruction
:name "ANDN"
:operands "reg64,reg64,rm64"
:code-string "[rvm: vex.nds.lz.0f38.w1 f2 /r]"
:arch-flags (list "LONG" "FUTURE" "BMI1")))

(setf BEXTR-reg32.rm32.reg32 (make-instance 'x86-asm-instruction
:name "BEXTR"
:operands "reg32,rm32,reg32"
:code-string "[rmv: vex.nds.lz.0f38.w0 f7 /r]"
:arch-flags (list "FUTURE" "BMI1")))

(setf BEXTR-reg64.rm64.reg64 (make-instance 'x86-asm-instruction
:name "BEXTR"
:operands "reg64,rm64,reg64"
:code-string "[rmv: vex.nds.lz.0f38.w1 f7 /r]"
:arch-flags (list "LONG" "FUTURE" "BMI1")))

(setf BEXTR-reg32.rm32.imm32 (make-instance 'x86-asm-instruction
:name "BEXTR"
:operands "reg32,rm32,imm32"
:code-string "[rmi: xop.m10.lz.w0 10 /r id]"
:arch-flags (list "FUTURE" "TBM")))

(setf BEXTR-reg64.rm64.imm32 (make-instance 'x86-asm-instruction
:name "BEXTR"
:operands "reg64,rm64,imm32"
:code-string "[rmi: xop.m10.lz.w1 10 /r id]"
:arch-flags (list "LONG" "FUTURE" "TBM")))

(setf BLCI-reg32.rm32 (make-instance 'x86-asm-instruction
:name "BLCI"
:operands "reg32,rm32"
:code-string "[vm: xop.ndd.lz.m9.w0 02 /6]"
:arch-flags (list "FUTURE" "TBM")))

(setf BLCI-reg64.rm64 (make-instance 'x86-asm-instruction
:name "BLCI"
:operands "reg64,rm64"
:code-string "[vm: xop.ndd.lz.m9.w1 02 /6]"
:arch-flags (list "LONG" "FUTURE" "TBM")))

(setf BLCIC-reg32.rm32 (make-instance 'x86-asm-instruction
:name "BLCIC"
:operands "reg32,rm32"
:code-string "[vm: xop.ndd.lz.m9.w0 01 /5]"
:arch-flags (list "FUTURE" "TBM")))

(setf BLCIC-reg64.rm64 (make-instance 'x86-asm-instruction
:name "BLCIC"
:operands "reg64,rm64"
:code-string "[vm: xop.ndd.lz.m9.w1 01 /5]"
:arch-flags (list "LONG" "FUTURE" "TBM")))

(setf BLSI-reg32.rm32 (make-instance 'x86-asm-instruction
:name "BLSI"
:operands "reg32,rm32"
:code-string "[vm: vex.ndd.lz.0f38.w0 f3 /3]"
:arch-flags (list "FUTURE" "BMI1")))

(setf BLSI-reg64.rm64 (make-instance 'x86-asm-instruction
:name "BLSI"
:operands "reg64,rm64"
:code-string "[vm: vex.ndd.lz.0f38.w1 f3 /3]"
:arch-flags (list "LONG" "FUTURE" "BMI1")))

(setf BLSIC-reg32.rm32 (make-instance 'x86-asm-instruction
:name "BLSIC"
:operands "reg32,rm32"
:code-string "[vm: xop.ndd.lz.m9.w0 01 /6]"
:arch-flags (list "FUTURE" "TBM")))

(setf BLSIC-reg64.rm64 (make-instance 'x86-asm-instruction
:name "BLSIC"
:operands "reg64,rm64"
:code-string "[vm: xop.ndd.lz.m9.w1 01 /6]"
:arch-flags (list "LONG" "FUTURE" "TBM")))

(setf BLCFILL-reg32.rm32 (make-instance 'x86-asm-instruction
:name "BLCFILL"
:operands "reg32,rm32"
:code-string "[vm: xop.ndd.lz.m9.w0 01 /1]"
:arch-flags (list "FUTURE" "TBM")))

(setf BLCFILL-reg64.rm64 (make-instance 'x86-asm-instruction
:name "BLCFILL"
:operands "reg64,rm64"
:code-string "[vm: xop.ndd.lz.m9.w1 01 /1]"
:arch-flags (list "LONG" "FUTURE" "TBM")))

(setf BLSFILL-reg32.rm32 (make-instance 'x86-asm-instruction
:name "BLSFILL"
:operands "reg32,rm32"
:code-string "[vm: xop.ndd.lz.m9.w0 01 /2]"
:arch-flags (list "FUTURE" "TBM")))

(setf BLSFILL-reg64.rm64 (make-instance 'x86-asm-instruction
:name "BLSFILL"
:operands "reg64,rm64"
:code-string "[vm: xop.ndd.lz.m9.w1 01 /2]"
:arch-flags (list "LONG" "FUTURE" "TBM")))

(setf BLCMSK-reg32.rm32 (make-instance 'x86-asm-instruction
:name "BLCMSK"
:operands "reg32,rm32"
:code-string "[vm: xop.ndd.lz.m9.w0 02 /1]"
:arch-flags (list "FUTURE" "TBM")))

(setf BLCMSK-reg64.rm64 (make-instance 'x86-asm-instruction
:name "BLCMSK"
:operands "reg64,rm64"
:code-string "[vm: xop.ndd.lz.m9.w1 02 /1]"
:arch-flags (list "LONG" "FUTURE" "TBM")))

(setf BLSMSK-reg32.rm32 (make-instance 'x86-asm-instruction
:name "BLSMSK"
:operands "reg32,rm32"
:code-string "[vm: vex.ndd.lz.0f38.w0 f3 /2]"
:arch-flags (list "FUTURE" "BMI1")))

(setf BLSMSK-reg64.rm64 (make-instance 'x86-asm-instruction
:name "BLSMSK"
:operands "reg64,rm64"
:code-string "[vm: vex.ndd.lz.0f38.w1 f3 /2]"
:arch-flags (list "LONG" "FUTURE" "BMI1")))

(setf BLSR-reg32.rm32 (make-instance 'x86-asm-instruction
:name "BLSR"
:operands "reg32,rm32"
:code-string "[vm: vex.ndd.lz.0f38.w0 f3 /1]"
:arch-flags (list "FUTURE" "BMI1")))

(setf BLSR-reg64.rm64 (make-instance 'x86-asm-instruction
:name "BLSR"
:operands "reg64,rm64"
:code-string "[vm: vex.ndd.lz.0f38.w1 f3 /1]"
:arch-flags (list "LONG" "FUTURE" "BMI1")))

(setf BLCS-reg32.rm32 (make-instance 'x86-asm-instruction
:name "BLCS"
:operands "reg32,rm32"
:code-string "[vm: xop.ndd.lz.m9.w0 01 /3]"
:arch-flags (list "FUTURE" "TBM")))

(setf BLCS-reg64.rm64 (make-instance 'x86-asm-instruction
:name "BLCS"
:operands "reg64,rm64"
:code-string "[vm: xop.ndd.lz.m9.w1 01 /3]"
:arch-flags (list "LONG" "FUTURE" "TBM")))

(setf BZHI-reg32.rm32.reg32 (make-instance 'x86-asm-instruction
:name "BZHI"
:operands "reg32,rm32,reg32"
:code-string "[rmv: vex.nds.lz.0f38.w0 f5 /r]"
:arch-flags (list "FUTURE" "BMI2")))

(setf BZHI-reg64.rm64.reg64 (make-instance 'x86-asm-instruction
:name "BZHI"
:operands "reg64,rm64,reg64"
:code-string "[rmv: vex.nds.lz.0f38.w1 f5 /r]"
:arch-flags (list "LONG" "FUTURE" "BMI2")))

(setf MULX-reg32.reg32.rm32 (make-instance 'x86-asm-instruction
:name "MULX"
:operands "reg32,reg32,rm32"
:code-string "[rvm: vex.ndd.lz.f2.0f38.w0 f6 /r]"
:arch-flags (list "FUTURE" "BMI2")))

(setf MULX-reg64.reg64.rm64 (make-instance 'x86-asm-instruction
:name "MULX"
:operands "reg64,reg64,rm64"
:code-string "[rvm: vex.ndd.lz.f2.0f38.w1 f6 /r]"
:arch-flags (list "LONG" "FUTURE" "BMI2")))

(setf PDEP-reg32.reg32.rm32 (make-instance 'x86-asm-instruction
:name "PDEP"
:operands "reg32,reg32,rm32"
:code-string "[rvm: vex.nds.lz.f2.0f38.w0 f5 /r]"
:arch-flags (list "FUTURE" "BMI2")))

(setf PDEP-reg64.reg64.rm64 (make-instance 'x86-asm-instruction
:name "PDEP"
:operands "reg64,reg64,rm64"
:code-string "[rvm: vex.nds.lz.f2.0f38.w1 f5 /r]"
:arch-flags (list "LONG" "FUTURE" "BMI2")))

(setf PEXT-reg32.reg32.rm32 (make-instance 'x86-asm-instruction
:name "PEXT"
:operands "reg32,reg32,rm32"
:code-string "[rvm: vex.nds.lz.f3.0f38.w0 f5 /r]"
:arch-flags (list "FUTURE" "BMI2")))

(setf PEXT-reg64.reg64.rm64 (make-instance 'x86-asm-instruction
:name "PEXT"
:operands "reg64,reg64,rm64"
:code-string "[rvm: vex.nds.lz.f3.0f38.w1 f5 /r]"
:arch-flags (list "LONG" "FUTURE" "BMI2")))

(setf RORX-reg32.rm32.imm8 (make-instance 'x86-asm-instruction
:name "RORX"
:operands "reg32,rm32,imm8"
:code-string "[rmi: vex.lz.f2.0f3a.w0 f0 /r ib]"
:arch-flags (list "FUTURE" "BMI2")))

(setf RORX-reg64.rm64.imm8 (make-instance 'x86-asm-instruction
:name "RORX"
:operands "reg64,rm64,imm8"
:code-string "[rmi: vex.lz.f2.0f3a.w1 f0 /r ib]"
:arch-flags (list "LONG" "FUTURE" "BMI2")))

(setf SARX-reg32.rm32.reg32 (make-instance 'x86-asm-instruction
:name "SARX"
:operands "reg32,rm32,reg32"
:code-string "[rmv: vex.nds.lz.f3.0f38.w0 f7 /r]"
:arch-flags (list "FUTURE" "BMI2")))

(setf SARX-reg64.rm64.reg64 (make-instance 'x86-asm-instruction
:name "SARX"
:operands "reg64,rm64,reg64"
:code-string "[rmv: vex.nds.lz.f3.0f38.w1 f7 /r]"
:arch-flags (list "LONG" "FUTURE" "BMI2")))

(setf SHLX-reg32.rm32.reg32 (make-instance 'x86-asm-instruction
:name "SHLX"
:operands "reg32,rm32,reg32"
:code-string "[rmv: vex.nds.lz.66.0f38.w0 f7 /r]"
:arch-flags (list "FUTURE" "BMI2")))

(setf SHLX-reg64.rm64.reg64 (make-instance 'x86-asm-instruction
:name "SHLX"
:operands "reg64,rm64,reg64"
:code-string "[rmv: vex.nds.lz.66.0f38.w1 f7 /r]"
:arch-flags (list "LONG" "FUTURE" "BMI2")))

(setf SHRX-reg32.rm32.reg32 (make-instance 'x86-asm-instruction
:name "SHRX"
:operands "reg32,rm32,reg32"
:code-string "[rmv: vex.nds.lz.f2.0f38.w0 f7 /r]"
:arch-flags (list "FUTURE" "BMI2")))

(setf SHRX-reg64.rm64.reg64 (make-instance 'x86-asm-instruction
:name "SHRX"
:operands "reg64,rm64,reg64"
:code-string "[rmv: vex.nds.lz.f2.0f38.w1 f7 /r]"
:arch-flags (list "LONG" "FUTURE" "BMI2")))

(setf TZCNT-reg16.rm16 (make-instance 'x86-asm-instruction
:name "TZCNT"
:operands "reg16,rm16"
:code-string "[rm: o16 f3i 0f bc /r]"
:arch-flags (list "FUTURE" "BMI1")))

(setf TZCNT-reg32.rm32 (make-instance 'x86-asm-instruction
:name "TZCNT"
:operands "reg32,rm32"
:code-string "[rm: o32 f3i 0f bc /r]"
:arch-flags (list "FUTURE" "BMI1")))

(setf TZCNT-reg64.rm64 (make-instance 'x86-asm-instruction
:name "TZCNT"
:operands "reg64,rm64"
:code-string "[rm: o64 f3i 0f bc /r]"
:arch-flags (list "LONG" "FUTURE" "BMI1")))

(setf TZMSK-reg32.rm32 (make-instance 'x86-asm-instruction
:name "TZMSK"
:operands "reg32,rm32"
:code-string "[vm: xop.ndd.lz.m9.w0 01 /4]"
:arch-flags (list "FUTURE" "TBM")))

(setf TZMSK-reg64.rm64 (make-instance 'x86-asm-instruction
:name "TZMSK"
:operands "reg64,rm64"
:code-string "[vm: xop.ndd.lz.m9.w1 01 /4]"
:arch-flags (list "LONG" "FUTURE" "TBM")))

(setf T1MSKC-reg32.rm32 (make-instance 'x86-asm-instruction
:name "T1MSKC"
:operands "reg32,rm32"
:code-string "[vm: xop.ndd.lz.m9.w0 01 /7]"
:arch-flags (list "FUTURE" "TBM")))

(setf T1MSKC-reg64.rm64 (make-instance 'x86-asm-instruction
:name "T1MSKC"
:operands "reg64,rm64"
:code-string "[vm: xop.ndd.lz.m9.w1 01 /7]"
:arch-flags (list "LONG" "FUTURE" "TBM")))

(setf PREFETCHWT1-mem8 (make-instance 'x86-asm-instruction
:name "PREFETCHWT1"
:operands "mem8"
:code-string "[m: 0f 0d /2 ]"
:arch-flags (list "PREFETCHWT1" "FUTURE")))

(setf BNDMK-bndreg.mem (make-instance 'x86-asm-instruction
:name "BNDMK"
:operands "bndreg,mem"
:code-string "[rm: f3 0f 1b /r ]"
:arch-flags (list "MPX" "MIB" "FUTURE")))

(setf BNDCL-bndreg.mem (make-instance 'x86-asm-instruction
:name "BNDCL"
:operands "bndreg,mem"
:code-string "[rm: f3 0f 1a /r ]"
:arch-flags (list "MPX" "FUTURE")))

(setf BNDCL-bndreg.reg64 (make-instance 'x86-asm-instruction
:name "BNDCL"
:operands "bndreg,reg64"
:code-string "[rm: o64nw f3 0f 1a /r ]"
:arch-flags (list "MPX" "LONG" "FUTURE")))

(setf BNDCU-bndreg.mem (make-instance 'x86-asm-instruction
:name "BNDCU"
:operands "bndreg,mem"
:code-string "[rm: f2 0f 1a /r ]"
:arch-flags (list "MPX" "FUTURE")))

(setf BNDCU-bndreg.reg64 (make-instance 'x86-asm-instruction
:name "BNDCU"
:operands "bndreg,reg64"
:code-string "[rm: o64nw f2 0f 1a /r ]"
:arch-flags (list "MPX" "LONG" "FUTURE")))

(setf BNDCN-bndreg.mem (make-instance 'x86-asm-instruction
:name "BNDCN"
:operands "bndreg,mem"
:code-string "[rm: f2 0f 1b /r ]"
:arch-flags (list "MPX" "FUTURE")))

(setf BNDCN-bndreg.reg64 (make-instance 'x86-asm-instruction
:name "BNDCN"
:operands "bndreg,reg64"
:code-string "[rm: o64nw f2 0f 1b /r ]"
:arch-flags (list "MPX" "LONG" "FUTURE")))

(setf BNDMOV-bndreg.bndreg (make-instance 'x86-asm-instruction
:name "BNDMOV"
:operands "bndreg,bndreg"
:code-string "[rm: 66 0f 1a /r ]"
:arch-flags (list "MPX" "FUTURE")))

(setf BNDMOV-bndreg.mem (make-instance 'x86-asm-instruction
:name "BNDMOV"
:operands "bndreg,mem"
:code-string "[rm: 66 0f 1a /r ]"
:arch-flags (list "MPX" "FUTURE")))

(setf BNDMOV-bndreg.bndreg (make-instance 'x86-asm-instruction
:name "BNDMOV"
:operands "bndreg,bndreg"
:code-string "[mr: 66 0f 1b /r ]"
:arch-flags (list "MPX" "FUTURE")))

(setf BNDMOV-mem.bndreg (make-instance 'x86-asm-instruction
:name "BNDMOV"
:operands "mem,bndreg"
:code-string "[mr: 66 0f 1b /r ]"
:arch-flags (list "MPX" "FUTURE")))

(setf BNDLDX-bndreg.mem (make-instance 'x86-asm-instruction
:name "BNDLDX"
:operands "bndreg,mem"
:code-string "[rm: 0f 1a /r ]"
:arch-flags (list "MPX" "MIB" "FUTURE")))

(setf BNDLDX-bndreg.mem.reg64 (make-instance 'x86-asm-instruction
:name "BNDLDX"
:operands "bndreg,mem,reg64"
:code-string "[rmx: 0f 1a /r ]"
:arch-flags (list "MPX" "MIB" "LONG" "FUTURE")))

(setf BNDSTX-mem.bndreg (make-instance 'x86-asm-instruction
:name "BNDSTX"
:operands "mem,bndreg"
:code-string "[mr: 0f 1b /r ]"
:arch-flags (list "MPX" "MIB" "FUTURE")))

(setf BNDSTX-mem.reg64.bndreg (make-instance 'x86-asm-instruction
:name "BNDSTX"
:operands "mem,reg64,bndreg"
:code-string "[mxr: 0f 1b /r ]"
:arch-flags (list "MPX" "MIB" "LONG" "FUTURE")))

(setf BNDSTX-mem.bndreg.reg64 (make-instance 'x86-asm-instruction
:name "BNDSTX"
:operands "mem,bndreg,reg64"
:code-string "[mrx: 0f 1b /r ]"
:arch-flags (list "MPX" "MIB" "LONG" "FUTURE")))

(setf KADDB-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KADDB"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.66.0f.w0 4a /r ]"
:arch-flags (list "FUTURE")))

(setf KADDD-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KADDD"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.66.0f.w1 4a /r ]"
:arch-flags (list "FUTURE")))

(setf KADDQ-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KADDQ"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.0f.w1 4a /r ]"
:arch-flags (list "FUTURE")))

(setf KADDW-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KADDW"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.0f.w0 4a /r ]"
:arch-flags (list "FUTURE")))

(setf KANDB-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KANDB"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.66.0f.w0 41 /r ]"
:arch-flags (list "FUTURE")))

(setf KANDD-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KANDD"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.66.0f.w1 41 /r ]"
:arch-flags (list "FUTURE")))

(setf KANDNB-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KANDNB"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.66.0f.w0 42 /r ]"
:arch-flags (list "FUTURE")))

(setf KANDND-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KANDND"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.66.0f.w1 42 /r ]"
:arch-flags (list "FUTURE")))

(setf KANDNQ-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KANDNQ"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.0f.w1 42 /r ]"
:arch-flags (list "FUTURE")))

(setf KANDNW-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KANDNW"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.0f.w0 42 /r ]"
:arch-flags (list "FUTURE")))

(setf KANDQ-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KANDQ"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.0f.w1 41 /r ]"
:arch-flags (list "FUTURE")))

(setf KANDW-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KANDW"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.0f.w0 41 /r ]"
:arch-flags (list "FUTURE")))

(setf KMOVB-kreg.krm8 (make-instance 'x86-asm-instruction
:name "KMOVB"
:operands "kreg,krm8"
:code-string "[rm: vex.l0.66.0f.w0 90 /r ]"
:arch-flags (list "FUTURE")))

(setf KMOVB-mem8.kreg (make-instance 'x86-asm-instruction
:name "KMOVB"
:operands "mem8,kreg"
:code-string "[mr: vex.l0.66.0f.w0 91 /r ]"
:arch-flags (list "FUTURE")))

(setf KMOVB-kreg.reg32 (make-instance 'x86-asm-instruction
:name "KMOVB"
:operands "kreg,reg32"
:code-string "[rm: vex.l0.66.0f.w0 92 /r ]"
:arch-flags (list "FUTURE")))

(setf KMOVB-reg32.kreg (make-instance 'x86-asm-instruction
:name "KMOVB"
:operands "reg32,kreg"
:code-string "[rm: vex.l0.66.0f.w0 93 /r ]"
:arch-flags (list "FUTURE")))

(setf KMOVD-kreg.krm32 (make-instance 'x86-asm-instruction
:name "KMOVD"
:operands "kreg,krm32"
:code-string "[rm: vex.l0.66.0f.w1 90 /r ]"
:arch-flags (list "FUTURE")))

(setf KMOVD-mem32.kreg (make-instance 'x86-asm-instruction
:name "KMOVD"
:operands "mem32,kreg"
:code-string "[mr: vex.l0.66.0f.w1 91 /r ]"
:arch-flags (list "FUTURE")))

(setf KMOVD-kreg.reg32 (make-instance 'x86-asm-instruction
:name "KMOVD"
:operands "kreg,reg32"
:code-string "[rm: vex.l0.f2.0f.w0 92 /r ]"
:arch-flags (list "FUTURE")))

(setf KMOVD-reg32.kreg (make-instance 'x86-asm-instruction
:name "KMOVD"
:operands "reg32,kreg"
:code-string "[rm: vex.l0.f2.0f.w0 93 /r ]"
:arch-flags (list "FUTURE")))

(setf KMOVQ-kreg.krm64 (make-instance 'x86-asm-instruction
:name "KMOVQ"
:operands "kreg,krm64"
:code-string "[rm: vex.l0.0f.w1 90 /r ]"
:arch-flags (list "FUTURE")))

(setf KMOVQ-mem64.kreg (make-instance 'x86-asm-instruction
:name "KMOVQ"
:operands "mem64,kreg"
:code-string "[mr: vex.l0.0f.w1 91 /r ]"
:arch-flags (list "FUTURE")))

(setf KMOVQ-kreg.reg64 (make-instance 'x86-asm-instruction
:name "KMOVQ"
:operands "kreg,reg64"
:code-string "[rm: vex.l0.f2.0f.w1 92 /r ]"
:arch-flags (list "FUTURE")))

(setf KMOVQ-reg64.kreg (make-instance 'x86-asm-instruction
:name "KMOVQ"
:operands "reg64,kreg"
:code-string "[rm: vex.l0.f2.0f.w1 93 /r ]"
:arch-flags (list "FUTURE")))

(setf KMOVW-kreg.krm16 (make-instance 'x86-asm-instruction
:name "KMOVW"
:operands "kreg,krm16"
:code-string "[rm: vex.l0.0f.w0 90 /r ]"
:arch-flags (list "FUTURE")))

(setf KMOVW-mem16.kreg (make-instance 'x86-asm-instruction
:name "KMOVW"
:operands "mem16,kreg"
:code-string "[mr: vex.l0.0f.w0 91 /r ]"
:arch-flags (list "FUTURE")))

(setf KMOVW-kreg.reg32 (make-instance 'x86-asm-instruction
:name "KMOVW"
:operands "kreg,reg32"
:code-string "[rm: vex.l0.0f.w0 92 /r ]"
:arch-flags (list "FUTURE")))

(setf KMOVW-reg32.kreg (make-instance 'x86-asm-instruction
:name "KMOVW"
:operands "reg32,kreg"
:code-string "[rm: vex.l0.0f.w0 93 /r ]"
:arch-flags (list "FUTURE")))

(setf KNOTB-kreg.kreg (make-instance 'x86-asm-instruction
:name "KNOTB"
:operands "kreg,kreg"
:code-string "[rm: vex.l0.66.0f.w0 44 /r ]"
:arch-flags (list "FUTURE")))

(setf KNOTD-kreg.kreg (make-instance 'x86-asm-instruction
:name "KNOTD"
:operands "kreg,kreg"
:code-string "[rm: vex.l0.66.0f.w1 44 /r ]"
:arch-flags (list "FUTURE")))

(setf KNOTQ-kreg.kreg (make-instance 'x86-asm-instruction
:name "KNOTQ"
:operands "kreg,kreg"
:code-string "[rm: vex.l0.0f.w1 44 /r ]"
:arch-flags (list "FUTURE")))

(setf KNOTW-kreg.kreg (make-instance 'x86-asm-instruction
:name "KNOTW"
:operands "kreg,kreg"
:code-string "[rm: vex.l0.0f.w0 44 /r ]"
:arch-flags (list "FUTURE")))

(setf KORB-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KORB"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.66.0f.w0 45 /r ]"
:arch-flags (list "FUTURE")))

(setf KORD-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KORD"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.66.0f.w1 45 /r ]"
:arch-flags (list "FUTURE")))

(setf KORQ-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KORQ"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.0f.w1 45 /r ]"
:arch-flags (list "FUTURE")))

(setf KORTESTB-kreg.kreg (make-instance 'x86-asm-instruction
:name "KORTESTB"
:operands "kreg,kreg"
:code-string "[rm: vex.l0.66.0f.w0 98 /r ]"
:arch-flags (list "FUTURE")))

(setf KORTESTD-kreg.kreg (make-instance 'x86-asm-instruction
:name "KORTESTD"
:operands "kreg,kreg"
:code-string "[rm: vex.l0.66.0f.w1 98 /r ]"
:arch-flags (list "FUTURE")))

(setf KORTESTQ-kreg.kreg (make-instance 'x86-asm-instruction
:name "KORTESTQ"
:operands "kreg,kreg"
:code-string "[rm: vex.l0.0f.w1 98 /r ]"
:arch-flags (list "FUTURE")))

(setf KORTESTW-kreg.kreg (make-instance 'x86-asm-instruction
:name "KORTESTW"
:operands "kreg,kreg"
:code-string "[rm: vex.l0.0f.w0 98 /r ]"
:arch-flags (list "FUTURE")))

(setf KORW-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KORW"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.0f.w0 45 /r ]"
:arch-flags (list "FUTURE")))

(setf KSHIFTLB-kreg.kreg.imm8 (make-instance 'x86-asm-instruction
:name "KSHIFTLB"
:operands "kreg,kreg,imm8"
:code-string "[rmi: vex.l0.66.0f3a.w0 32 /r ib ]"
:arch-flags (list "FUTURE")))

(setf KSHIFTLD-kreg.kreg.imm8 (make-instance 'x86-asm-instruction
:name "KSHIFTLD"
:operands "kreg,kreg,imm8"
:code-string "[rmi: vex.l0.66.0f3a.w0 33 /r ib ]"
:arch-flags (list "FUTURE")))

(setf KSHIFTLQ-kreg.kreg.imm8 (make-instance 'x86-asm-instruction
:name "KSHIFTLQ"
:operands "kreg,kreg,imm8"
:code-string "[rmi: vex.l0.66.0f3a.w1 33 /r ib ]"
:arch-flags (list "FUTURE")))

(setf KSHIFTLW-kreg.kreg.imm8 (make-instance 'x86-asm-instruction
:name "KSHIFTLW"
:operands "kreg,kreg,imm8"
:code-string "[rmi: vex.l0.66.0f3a.w1 32 /r ib ]"
:arch-flags (list "FUTURE")))

(setf KSHIFTRB-kreg.kreg.imm8 (make-instance 'x86-asm-instruction
:name "KSHIFTRB"
:operands "kreg,kreg,imm8"
:code-string "[rmi: vex.l0.66.0f3a.w0 30 /r ib ]"
:arch-flags (list "FUTURE")))

(setf KSHIFTRD-kreg.kreg.imm8 (make-instance 'x86-asm-instruction
:name "KSHIFTRD"
:operands "kreg,kreg,imm8"
:code-string "[rmi: vex.l0.66.0f3a.w0 31 /r ib ]"
:arch-flags (list "FUTURE")))

(setf KSHIFTRQ-kreg.kreg.imm8 (make-instance 'x86-asm-instruction
:name "KSHIFTRQ"
:operands "kreg,kreg,imm8"
:code-string "[rmi: vex.l0.66.0f3a.w1 31 /r ib ]"
:arch-flags (list "FUTURE")))

(setf KSHIFTRW-kreg.kreg.imm8 (make-instance 'x86-asm-instruction
:name "KSHIFTRW"
:operands "kreg,kreg,imm8"
:code-string "[rmi: vex.l0.66.0f3a.w1 30 /r ib ]"
:arch-flags (list "FUTURE")))

(setf KTESTB-kreg.kreg (make-instance 'x86-asm-instruction
:name "KTESTB"
:operands "kreg,kreg"
:code-string "[rm: vex.l0.66.0f.w0 99 /r ]"
:arch-flags (list "FUTURE")))

(setf KTESTD-kreg.kreg (make-instance 'x86-asm-instruction
:name "KTESTD"
:operands "kreg,kreg"
:code-string "[rm: vex.l0.66.0f.w1 99 /r ]"
:arch-flags (list "FUTURE")))

(setf KTESTQ-kreg.kreg (make-instance 'x86-asm-instruction
:name "KTESTQ"
:operands "kreg,kreg"
:code-string "[rm: vex.l0.0f.w1 99 /r ]"
:arch-flags (list "FUTURE")))

(setf KTESTW-kreg.kreg (make-instance 'x86-asm-instruction
:name "KTESTW"
:operands "kreg,kreg"
:code-string "[rm: vex.l0.0f.w0 99 /r ]"
:arch-flags (list "FUTURE")))

(setf KUNPCKBW-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KUNPCKBW"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.66.0f.w0 4b /r ]"
:arch-flags (list "FUTURE")))

(setf KUNPCKDQ-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KUNPCKDQ"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.0f.w1 4b /r ]"
:arch-flags (list "FUTURE")))

(setf KUNPCKWD-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KUNPCKWD"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.0f.w0 4b /r ]"
:arch-flags (list "FUTURE")))

(setf KXNORB-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KXNORB"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.66.0f.w0 46 /r ]"
:arch-flags (list "FUTURE")))

(setf KXNORD-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KXNORD"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.66.0f.w1 46 /r ]"
:arch-flags (list "FUTURE")))

(setf KXNORQ-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KXNORQ"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.0f.w1 46 /r ]"
:arch-flags (list "FUTURE")))

(setf KXNORW-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KXNORW"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.0f.w0 46 /r ]"
:arch-flags (list "FUTURE")))

(setf KXORB-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KXORB"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.66.0f.w0 47 /r ]"
:arch-flags (list "FUTURE")))

(setf KXORD-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KXORD"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.66.0f.w1 47 /r ]"
:arch-flags (list "FUTURE")))

(setf KXORQ-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KXORQ"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.0f.w1 47 /r ]"
:arch-flags (list "FUTURE")))

(setf KXORW-kreg.kreg.kreg (make-instance 'x86-asm-instruction
:name "KXORW"
:operands "kreg,kreg,kreg"
:code-string "[rvm: vex.nds.l1.0f.w0 47 /r ]"
:arch-flags (list "FUTURE")))

(setf SHA1MSG1-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "SHA1MSG1"
:operands "xmmreg,xmmrm128"
:code-string "[rm: 0f 38 c9 /r ]"
:arch-flags (list "SHA" "FUTURE")))

(setf SHA1MSG2-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "SHA1MSG2"
:operands "xmmreg,xmmrm128"
:code-string "[rm: 0f 38 ca /r ]"
:arch-flags (list "SHA" "FUTURE")))

(setf SHA1NEXTE-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "SHA1NEXTE"
:operands "xmmreg,xmmrm128"
:code-string "[rm: 0f 38 c8 /r ]"
:arch-flags (list "SHA" "FUTURE")))

(setf SHA1RNDS4-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "SHA1RNDS4"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[rmi: 0f 3a cc /r ib ]"
:arch-flags (list "SHA" "FUTURE")))

(setf SHA256MSG1-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "SHA256MSG1"
:operands "xmmreg,xmmrm128"
:code-string "[rm: 0f 38 cc /r ]"
:arch-flags (list "SHA" "FUTURE")))

(setf SHA256MSG2-xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "SHA256MSG2"
:operands "xmmreg,xmmrm128"
:code-string "[rm: 0f 38 cd /r ]"
:arch-flags (list "SHA" "FUTURE")))

(setf SHA256RNDS2-xmmreg.xmmrm128.xmm0 (make-instance 'x86-asm-instruction
:name "SHA256RNDS2"
:operands "xmmreg,xmmrm128,xmm0"
:code-string "[rm-: 0f 38 cb /r ]"
:arch-flags (list "SHA" "FUTURE")))

(setf VADDPD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VADDPD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 58 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VADDPD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VADDPD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 58 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VADDPD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VADDPD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 58 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VADDPS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VADDPS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.0f.w0 58 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VADDPS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VADDPS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.0f.w0 58 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VADDPS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VADDPS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.0f.w0 58 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VADDSD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VADDSD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|er"
:code-string "[rvm:t1s: evex.nds.128.f2.0f.w1 58 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VADDSS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VADDSS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|er"
:code-string "[rvm:t1s: evex.nds.128.f3.0f.w0 58 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VALIGND-xmmreg-mask-z.xmmreg.xmmrm128-b32.imm8 (make-instance 'x86-asm-instruction
:name "VALIGND"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32,imm8"
:code-string "[rvmi:fv: evex.nds.128.66.0f3a.w0 03 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VALIGND-ymmreg-mask-z.ymmreg.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VALIGND"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32,imm8"
:code-string "[rvmi:fv: evex.nds.256.66.0f3a.w0 03 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VALIGND-zmmreg-mask-z.zmmreg.zmmrm512-b32.imm8 (make-instance 'x86-asm-instruction
:name "VALIGND"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32,imm8"
:code-string "[rvmi:fv: evex.nds.512.66.0f3a.w0 03 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VALIGNQ-xmmreg-mask-z.xmmreg.xmmrm128-b64.imm8 (make-instance 'x86-asm-instruction
:name "VALIGNQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64,imm8"
:code-string "[rvmi:fv: evex.nds.128.66.0f3a.w1 03 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VALIGNQ-ymmreg-mask-z.ymmreg.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VALIGNQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64,imm8"
:code-string "[rvmi:fv: evex.nds.256.66.0f3a.w1 03 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VALIGNQ-zmmreg-mask-z.zmmreg.zmmrm512-b64.imm8 (make-instance 'x86-asm-instruction
:name "VALIGNQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64,imm8"
:code-string "[rvmi:fv: evex.nds.512.66.0f3a.w1 03 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VANDNPD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VANDNPD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 55 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VANDNPD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VANDNPD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 55 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VANDNPD-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VANDNPD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 55 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VANDNPS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VANDNPS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.0f.w0 55 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VANDNPS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VANDNPS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.0f.w0 55 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VANDNPS-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VANDNPS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.0f.w0 55 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VANDPD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VANDPD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 54 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VANDPD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VANDPD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 54 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VANDPD-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VANDPD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 54 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VANDPS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VANDPS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.0f.w0 54 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VANDPS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VANDPS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.0f.w0 54 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VANDPS-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VANDPS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.0f.w0 54 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VBLENDMPD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VBLENDMPD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 65 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VBLENDMPD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VBLENDMPD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 65 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VBLENDMPD-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VBLENDMPD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 65 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VBLENDMPS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VBLENDMPS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 65 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VBLENDMPS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VBLENDMPS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 65 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VBLENDMPS-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VBLENDMPS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 65 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VBROADCASTF32X2-ymmreg-mask-z.xmmrm64 (make-instance 'x86-asm-instruction
:name "VBROADCASTF32X2"
:operands "ymmreg|mask|z,xmmrm64"
:code-string "[rm:t2: evex.256.66.0f38.w0 19 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VBROADCASTF32X2-zmmreg-mask-z.xmmrm64 (make-instance 'x86-asm-instruction
:name "VBROADCASTF32X2"
:operands "zmmreg|mask|z,xmmrm64"
:code-string "[rm:t2: evex.512.66.0f38.w0 19 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VBROADCASTF32X4-ymmreg-mask-z.mem128 (make-instance 'x86-asm-instruction
:name "VBROADCASTF32X4"
:operands "ymmreg|mask|z,mem128"
:code-string "[rm:t4: evex.256.66.0f38.w0 1a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VBROADCASTF32X4-zmmreg-mask-z.mem128 (make-instance 'x86-asm-instruction
:name "VBROADCASTF32X4"
:operands "zmmreg|mask|z,mem128"
:code-string "[rm:t4: evex.512.66.0f38.w0 1a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VBROADCASTF32X8-zmmreg-mask-z.mem256 (make-instance 'x86-asm-instruction
:name "VBROADCASTF32X8"
:operands "zmmreg|mask|z,mem256"
:code-string "[rm:t8: evex.512.66.0f38.w0 1b /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VBROADCASTF64X2-ymmreg-mask-z.mem128 (make-instance 'x86-asm-instruction
:name "VBROADCASTF64X2"
:operands "ymmreg|mask|z,mem128"
:code-string "[rm:t2: evex.256.66.0f38.w1 1a /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VBROADCASTF64X2-zmmreg-mask-z.mem128 (make-instance 'x86-asm-instruction
:name "VBROADCASTF64X2"
:operands "zmmreg|mask|z,mem128"
:code-string "[rm:t2: evex.512.66.0f38.w1 1a /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VBROADCASTF64X4-zmmreg-mask-z.mem256 (make-instance 'x86-asm-instruction
:name "VBROADCASTF64X4"
:operands "zmmreg|mask|z,mem256"
:code-string "[rm:t4: evex.512.66.0f38.w1 1b /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VBROADCASTI32X2-xmmreg-mask-z.xmmrm64 (make-instance 'x86-asm-instruction
:name "VBROADCASTI32X2"
:operands "xmmreg|mask|z,xmmrm64"
:code-string "[rm:t2: evex.128.66.0f38.w0 59 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VBROADCASTI32X2-ymmreg-mask-z.xmmrm64 (make-instance 'x86-asm-instruction
:name "VBROADCASTI32X2"
:operands "ymmreg|mask|z,xmmrm64"
:code-string "[rm:t2: evex.256.66.0f38.w0 59 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VBROADCASTI32X2-zmmreg-mask-z.xmmrm64 (make-instance 'x86-asm-instruction
:name "VBROADCASTI32X2"
:operands "zmmreg|mask|z,xmmrm64"
:code-string "[rm:t2: evex.512.66.0f38.w0 59 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VBROADCASTI32X4-ymmreg-mask-z.mem128 (make-instance 'x86-asm-instruction
:name "VBROADCASTI32X4"
:operands "ymmreg|mask|z,mem128"
:code-string "[rm:t4: evex.256.66.0f38.w0 5a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VBROADCASTI32X4-zmmreg-mask-z.mem128 (make-instance 'x86-asm-instruction
:name "VBROADCASTI32X4"
:operands "zmmreg|mask|z,mem128"
:code-string "[rm:t4: evex.512.66.0f38.w0 5a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VBROADCASTI32X8-zmmreg-mask-z.mem256 (make-instance 'x86-asm-instruction
:name "VBROADCASTI32X8"
:operands "zmmreg|mask|z,mem256"
:code-string "[rm:t8: evex.512.66.0f38.w0 5b /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VBROADCASTI64X2-ymmreg-mask-z.mem128 (make-instance 'x86-asm-instruction
:name "VBROADCASTI64X2"
:operands "ymmreg|mask|z,mem128"
:code-string "[rm:t2: evex.256.66.0f38.w1 5a /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VBROADCASTI64X2-zmmreg-mask-z.mem128 (make-instance 'x86-asm-instruction
:name "VBROADCASTI64X2"
:operands "zmmreg|mask|z,mem128"
:code-string "[rm:t2: evex.512.66.0f38.w1 5a /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VBROADCASTI64X4-zmmreg-mask-z.mem256 (make-instance 'x86-asm-instruction
:name "VBROADCASTI64X4"
:operands "zmmreg|mask|z,mem256"
:code-string "[rm:t4: evex.512.66.0f38.w1 5b /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VBROADCASTSD-ymmreg-mask-z.mem64 (make-instance 'x86-asm-instruction
:name "VBROADCASTSD"
:operands "ymmreg|mask|z,mem64"
:code-string "[rm:t1s: evex.256.66.0f38.w1 19 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VBROADCASTSD-zmmreg-mask-z.mem64 (make-instance 'x86-asm-instruction
:name "VBROADCASTSD"
:operands "zmmreg|mask|z,mem64"
:code-string "[rm:t1s: evex.512.66.0f38.w1 19 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VBROADCASTSD-ymmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VBROADCASTSD"
:operands "ymmreg|mask|z,xmmreg"
:code-string "[rm: evex.256.66.0f38.w1 19 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VBROADCASTSD-zmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VBROADCASTSD"
:operands "zmmreg|mask|z,xmmreg"
:code-string "[rm: evex.512.66.0f38.w1 19 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VBROADCASTSS-xmmreg-mask-z.mem32 (make-instance 'x86-asm-instruction
:name "VBROADCASTSS"
:operands "xmmreg|mask|z,mem32"
:code-string "[rm:t1s: evex.128.66.0f38.w0 18 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VBROADCASTSS-ymmreg-mask-z.mem32 (make-instance 'x86-asm-instruction
:name "VBROADCASTSS"
:operands "ymmreg|mask|z,mem32"
:code-string "[rm:t1s: evex.256.66.0f38.w0 18 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VBROADCASTSS-zmmreg-mask-z.mem32 (make-instance 'x86-asm-instruction
:name "VBROADCASTSS"
:operands "zmmreg|mask|z,mem32"
:code-string "[rm:t1s: evex.512.66.0f38.w0 18 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VBROADCASTSS-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VBROADCASTSS"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[rm: evex.128.66.0f38.w0 18 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VBROADCASTSS-ymmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VBROADCASTSS"
:operands "ymmreg|mask|z,xmmreg"
:code-string "[rm: evex.256.66.0f38.w0 18 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VBROADCASTSS-zmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VBROADCASTSS"
:operands "zmmreg|mask|z,xmmreg"
:code-string "[rm: evex.512.66.0f38.w0 18 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCMPPD-kreg-mask.xmmreg.xmmrm128-b64.imm8 (make-instance 'x86-asm-instruction
:name "VCMPPD"
:operands "kreg|mask,xmmreg,xmmrm128|b64,imm8"
:code-string "[rvmi:fv: evex.nds.128.66.0f.w1 c2 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCMPPD-kreg-mask.ymmreg.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VCMPPD"
:operands "kreg|mask,ymmreg,ymmrm256|b64,imm8"
:code-string "[rvmi:fv: evex.nds.256.66.0f.w1 c2 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCMPPD-kreg-mask.zmmreg.zmmrm512-b64-sae.imm8 (make-instance 'x86-asm-instruction
:name "VCMPPD"
:operands "kreg|mask,zmmreg,zmmrm512|b64|sae,imm8"
:code-string "[rvmi:fv: evex.nds.512.66.0f.w1 c2 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCMPPS-kreg-mask.xmmreg.xmmrm128-b32.imm8 (make-instance 'x86-asm-instruction
:name "VCMPPS"
:operands "kreg|mask,xmmreg,xmmrm128|b32,imm8"
:code-string "[rvmi:fv: evex.nds.128.0f.w0 c2 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCMPPS-kreg-mask.ymmreg.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VCMPPS"
:operands "kreg|mask,ymmreg,ymmrm256|b32,imm8"
:code-string "[rvmi:fv: evex.nds.256.0f.w0 c2 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCMPPS-kreg-mask.zmmreg.zmmrm512-b32-sae.imm8 (make-instance 'x86-asm-instruction
:name "VCMPPS"
:operands "kreg|mask,zmmreg,zmmrm512|b32|sae,imm8"
:code-string "[rvmi:fv: evex.nds.512.0f.w0 c2 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCMPSD-kreg-mask.xmmreg.xmmrm64-sae.imm8 (make-instance 'x86-asm-instruction
:name "VCMPSD"
:operands "kreg|mask,xmmreg,xmmrm64|sae,imm8"
:code-string "[rvmi:t1s: evex.nds.128.f2.0f.w1 c2 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCMPSS-kreg-mask.xmmreg.xmmrm32-sae.imm8 (make-instance 'x86-asm-instruction
:name "VCMPSS"
:operands "kreg|mask,xmmreg,xmmrm32|sae,imm8"
:code-string "[rvmi:t1s: evex.nds.128.f3.0f.w0 c2 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCOMISD-xmmreg.xmmrm64-sae (make-instance 'x86-asm-instruction
:name "VCOMISD"
:operands "xmmreg,xmmrm64|sae"
:code-string "[rm:t1s: evex.128.66.0f.w1 2f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCOMISS-xmmreg.xmmrm32-sae (make-instance 'x86-asm-instruction
:name "VCOMISS"
:operands "xmmreg,xmmrm32|sae"
:code-string "[rm:t1s: evex.128.0f.w0 2f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCOMPRESSPD-mem128-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VCOMPRESSPD"
:operands "mem128|mask,xmmreg"
:code-string "[mr:t1s: evex.128.66.0f38.w1 8a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCOMPRESSPD-mem256-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VCOMPRESSPD"
:operands "mem256|mask,ymmreg"
:code-string "[mr:t1s: evex.256.66.0f38.w1 8a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCOMPRESSPD-mem512-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VCOMPRESSPD"
:operands "mem512|mask,zmmreg"
:code-string "[mr:t1s: evex.512.66.0f38.w1 8a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCOMPRESSPD-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VCOMPRESSPD"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.66.0f38.w1 8a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCOMPRESSPD-ymmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VCOMPRESSPD"
:operands "ymmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.66.0f38.w1 8a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCOMPRESSPD-zmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VCOMPRESSPD"
:operands "zmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.66.0f38.w1 8a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCOMPRESSPS-mem128-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VCOMPRESSPS"
:operands "mem128|mask,xmmreg"
:code-string "[mr:t1s: evex.128.66.0f38.w0 8a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCOMPRESSPS-mem256-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VCOMPRESSPS"
:operands "mem256|mask,ymmreg"
:code-string "[mr:t1s: evex.256.66.0f38.w0 8a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCOMPRESSPS-mem512-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VCOMPRESSPS"
:operands "mem512|mask,zmmreg"
:code-string "[mr:t1s: evex.512.66.0f38.w0 8a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCOMPRESSPS-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VCOMPRESSPS"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.66.0f38.w0 8a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCOMPRESSPS-ymmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VCOMPRESSPS"
:operands "ymmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.66.0f38.w0 8a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCOMPRESSPS-zmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VCOMPRESSPS"
:operands "zmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.66.0f38.w0 8a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTDQ2PD-xmmreg-mask-z.xmmrm64-b32 (make-instance 'x86-asm-instruction
:name "VCVTDQ2PD"
:operands "xmmreg|mask|z,xmmrm64|b32"
:code-string "[rm:hv: evex.128.f3.0f.w0 e6 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTDQ2PD-ymmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VCVTDQ2PD"
:operands "ymmreg|mask|z,xmmrm128|b32"
:code-string "[rm:hv: evex.256.f3.0f.w0 e6 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTDQ2PD-zmmreg-mask-z.ymmrm256-b32-er (make-instance 'x86-asm-instruction
:name "VCVTDQ2PD"
:operands "zmmreg|mask|z,ymmrm256|b32|er"
:code-string "[rm:hv: evex.512.f3.0f.w0 e6 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTDQ2PS-xmmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VCVTDQ2PS"
:operands "xmmreg|mask|z,xmmrm128|b32"
:code-string "[rm:fv: evex.128.0f.w0 5b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTDQ2PS-ymmreg-mask-z.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VCVTDQ2PS"
:operands "ymmreg|mask|z,ymmrm256|b32"
:code-string "[rm:fv: evex.256.0f.w0 5b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTDQ2PS-zmmreg-mask-z.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VCVTDQ2PS"
:operands "zmmreg|mask|z,zmmrm512|b32|er"
:code-string "[rm:fv: evex.512.0f.w0 5b /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTPD2DQ-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VCVTPD2DQ"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.f2.0f.w1 e6 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTPD2DQ-xmmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VCVTPD2DQ"
:operands "xmmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.f2.0f.w1 e6 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTPD2DQ-ymmreg-mask-z.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VCVTPD2DQ"
:operands "ymmreg|mask|z,zmmrm512|b64|er"
:code-string "[rm:fv: evex.512.f2.0f.w1 e6 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTPD2PS-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VCVTPD2PS"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.66.0f.w1 5a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTPD2PS-xmmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VCVTPD2PS"
:operands "xmmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.66.0f.w1 5a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTPD2PS-ymmreg-mask-z.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VCVTPD2PS"
:operands "ymmreg|mask|z,zmmrm512|b64|er"
:code-string "[rm:fv: evex.512.66.0f.w1 5a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTPD2QQ-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VCVTPD2QQ"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.66.0f.w1 7b /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTPD2QQ-ymmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VCVTPD2QQ"
:operands "ymmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.66.0f.w1 7b /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTPD2QQ-zmmreg-mask-z.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VCVTPD2QQ"
:operands "zmmreg|mask|z,zmmrm512|b64|er"
:code-string "[rm:fv: evex.512.66.0f.w1 7b /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VCVTPD2UDQ-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VCVTPD2UDQ"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.0f.w1 79 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTPD2UDQ-xmmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VCVTPD2UDQ"
:operands "xmmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.0f.w1 79 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTPD2UDQ-ymmreg-mask-z.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VCVTPD2UDQ"
:operands "ymmreg|mask|z,zmmrm512|b64|er"
:code-string "[rm:fv: evex.512.0f.w1 79 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTPD2UQQ-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VCVTPD2UQQ"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.66.0f.w1 79 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTPD2UQQ-ymmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VCVTPD2UQQ"
:operands "ymmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.66.0f.w1 79 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTPD2UQQ-zmmreg-mask-z.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VCVTPD2UQQ"
:operands "zmmreg|mask|z,zmmrm512|b64|er"
:code-string "[rm:fv: evex.512.66.0f.w1 79 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VCVTPH2PS-xmmreg-mask-z.xmmrm64 (make-instance 'x86-asm-instruction
:name "VCVTPH2PS"
:operands "xmmreg|mask|z,xmmrm64"
:code-string "[rm:hvm: evex.128.66.0f38.w0 13 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTPH2PS-ymmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VCVTPH2PS"
:operands "ymmreg|mask|z,xmmrm128"
:code-string "[rm:hvm: evex.256.66.0f38.w0 13 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTPH2PS-zmmreg-mask-z.ymmrm256-sae (make-instance 'x86-asm-instruction
:name "VCVTPH2PS"
:operands "zmmreg|mask|z,ymmrm256|sae"
:code-string "[rm:hvm: evex.512.66.0f38.w0 13 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTPS2DQ-xmmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VCVTPS2DQ"
:operands "xmmreg|mask|z,xmmrm128|b32"
:code-string "[rm:fv: evex.128.66.0f.w0 5b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTPS2DQ-ymmreg-mask-z.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VCVTPS2DQ"
:operands "ymmreg|mask|z,ymmrm256|b32"
:code-string "[rm:fv: evex.256.66.0f.w0 5b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTPS2DQ-zmmreg-mask-z.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VCVTPS2DQ"
:operands "zmmreg|mask|z,zmmrm512|b32|er"
:code-string "[rm:fv: evex.512.66.0f.w0 5b /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTPS2PD-xmmreg-mask-z.xmmrm64-b32 (make-instance 'x86-asm-instruction
:name "VCVTPS2PD"
:operands "xmmreg|mask|z,xmmrm64|b32"
:code-string "[rm:hv: evex.128.0f.w0 5a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTPS2PD-ymmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VCVTPS2PD"
:operands "ymmreg|mask|z,xmmrm128|b32"
:code-string "[rm:hv: evex.256.0f.w0 5a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTPS2PD-zmmreg-mask-z.ymmrm256-b32-sae (make-instance 'x86-asm-instruction
:name "VCVTPS2PD"
:operands "zmmreg|mask|z,ymmrm256|b32|sae"
:code-string "[rm:hv: evex.512.0f.w0 5a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTPS2PH-xmmreg-mask-z.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VCVTPS2PH"
:operands "xmmreg|mask|z,xmmreg,imm8"
:code-string "[mri:hvm: evex.128.66.0f3a.w0 1d /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTPS2PH-xmmreg-mask-z.ymmreg.imm8 (make-instance 'x86-asm-instruction
:name "VCVTPS2PH"
:operands "xmmreg|mask|z,ymmreg,imm8"
:code-string "[mri:hvm: evex.256.66.0f3a.w0 1d /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTPS2PH-ymmreg-mask-z.zmmreg-sae.imm8 (make-instance 'x86-asm-instruction
:name "VCVTPS2PH"
:operands "ymmreg|mask|z,zmmreg|sae,imm8"
:code-string "[mri:hvm: evex.512.66.0f3a.w0 1d /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTPS2PH-mem64-mask.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VCVTPS2PH"
:operands "mem64|mask,xmmreg,imm8"
:code-string "[mri:hvm: evex.128.66.0f3a.w0 1d /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTPS2PH-mem128-mask.ymmreg.imm8 (make-instance 'x86-asm-instruction
:name "VCVTPS2PH"
:operands "mem128|mask,ymmreg,imm8"
:code-string "[mri:hvm: evex.256.66.0f3a.w0 1d /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTPS2PH-mem256-mask.zmmreg-sae.imm8 (make-instance 'x86-asm-instruction
:name "VCVTPS2PH"
:operands "mem256|mask,zmmreg|sae,imm8"
:code-string "[mri:hvm: evex.512.66.0f3a.w0 1d /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTPS2QQ-xmmreg-mask-z.xmmrm64-b32 (make-instance 'x86-asm-instruction
:name "VCVTPS2QQ"
:operands "xmmreg|mask|z,xmmrm64|b32"
:code-string "[rm:hv: evex.128.66.0f.w0 7b /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTPS2QQ-ymmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VCVTPS2QQ"
:operands "ymmreg|mask|z,xmmrm128|b32"
:code-string "[rm:hv: evex.256.66.0f.w0 7b /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTPS2QQ-zmmreg-mask-z.ymmrm256-b32-er (make-instance 'x86-asm-instruction
:name "VCVTPS2QQ"
:operands "zmmreg|mask|z,ymmrm256|b32|er"
:code-string "[rm:hv: evex.512.66.0f.w0 7b /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VCVTPS2UDQ-xmmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VCVTPS2UDQ"
:operands "xmmreg|mask|z,xmmrm128|b32"
:code-string "[rm:fv: evex.128.0f.w0 79 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTPS2UDQ-ymmreg-mask-z.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VCVTPS2UDQ"
:operands "ymmreg|mask|z,ymmrm256|b32"
:code-string "[rm:fv: evex.256.0f.w0 79 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTPS2UDQ-zmmreg-mask-z.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VCVTPS2UDQ"
:operands "zmmreg|mask|z,zmmrm512|b32|er"
:code-string "[rm:fv: evex.512.0f.w0 79 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTPS2UQQ-xmmreg-mask-z.xmmrm64-b32 (make-instance 'x86-asm-instruction
:name "VCVTPS2UQQ"
:operands "xmmreg|mask|z,xmmrm64|b32"
:code-string "[rm:hv: evex.128.66.0f.w0 79 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTPS2UQQ-ymmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VCVTPS2UQQ"
:operands "ymmreg|mask|z,xmmrm128|b32"
:code-string "[rm:hv: evex.256.66.0f.w0 79 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTPS2UQQ-zmmreg-mask-z.ymmrm256-b32-er (make-instance 'x86-asm-instruction
:name "VCVTPS2UQQ"
:operands "zmmreg|mask|z,ymmrm256|b32|er"
:code-string "[rm:hv: evex.512.66.0f.w0 79 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VCVTQQ2PD-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VCVTQQ2PD"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.f3.0f.w1 e6 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTQQ2PD-ymmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VCVTQQ2PD"
:operands "ymmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.f3.0f.w1 e6 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTQQ2PD-zmmreg-mask-z.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VCVTQQ2PD"
:operands "zmmreg|mask|z,zmmrm512|b64|er"
:code-string "[rm:fv: evex.512.f3.0f.w1 e6 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VCVTQQ2PS-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VCVTQQ2PS"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.0f.w1 5b /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTQQ2PS-xmmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VCVTQQ2PS"
:operands "xmmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.0f.w1 5b /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTQQ2PS-ymmreg-mask-z.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VCVTQQ2PS"
:operands "ymmreg|mask|z,zmmrm512|b64|er"
:code-string "[rm:fv: evex.512.0f.w1 5b /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VCVTSD2SI-reg32.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VCVTSD2SI"
:operands "reg32,xmmrm64|er"
:code-string "[rm:t1f64: evex.128.f2.0f.w0 2d /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTSD2SI-reg64.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VCVTSD2SI"
:operands "reg64,xmmrm64|er"
:code-string "[rm:t1f64: evex.128.f2.0f.w1 2d /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTSD2SS-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VCVTSD2SS"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|er"
:code-string "[rvm:t1s: evex.nds.128.f2.0f.w1 5a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTSD2USI-reg32.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VCVTSD2USI"
:operands "reg32,xmmrm64|er"
:code-string "[rm:t1f64: evex.128.f2.0f.w0 79 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTSD2USI-reg64.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VCVTSD2USI"
:operands "reg64,xmmrm64|er"
:code-string "[rm:t1f64: evex.128.f2.0f.w1 79 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTSI2SD-xmmreg.xmmreg-er.rm32 (make-instance 'x86-asm-instruction
:name "VCVTSI2SD"
:operands "xmmreg,xmmreg|er,rm32"
:code-string "[rvm:t1s: evex.nds.128.f2.0f.w0 2a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTSI2SD-xmmreg.xmmreg-er.rm64 (make-instance 'x86-asm-instruction
:name "VCVTSI2SD"
:operands "xmmreg,xmmreg|er,rm64"
:code-string "[rvm:t1s: evex.nds.128.f2.0f.w1 2a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTSI2SS-xmmreg.xmmreg-er.rm32 (make-instance 'x86-asm-instruction
:name "VCVTSI2SS"
:operands "xmmreg,xmmreg|er,rm32"
:code-string "[rvm:t1s: evex.nds.128.f3.0f.w0 2a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTSI2SS-xmmreg.xmmreg-er.rm64 (make-instance 'x86-asm-instruction
:name "VCVTSI2SS"
:operands "xmmreg,xmmreg|er,rm64"
:code-string "[rvm:t1s: evex.nds.128.f3.0f.w1 2a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTSS2SD-xmmreg-mask-z.xmmreg.xmmrm32-sae (make-instance 'x86-asm-instruction
:name "VCVTSS2SD"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|sae"
:code-string "[rvm:t1s: evex.nds.128.f3.0f.w0 5a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTSS2SI-reg32.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VCVTSS2SI"
:operands "reg32,xmmrm32|er"
:code-string "[rm:t1f32: evex.128.f3.0f.w0 2d /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTSS2SI-reg64.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VCVTSS2SI"
:operands "reg64,xmmrm32|er"
:code-string "[rm:t1f32: evex.128.f3.0f.w1 2d /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTSS2USI-reg32.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VCVTSS2USI"
:operands "reg32,xmmrm32|er"
:code-string "[rm:t1f32: evex.128.f3.0f.w0 79 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTSS2USI-reg64.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VCVTSS2USI"
:operands "reg64,xmmrm32|er"
:code-string "[rm:t1f32: evex.128.f3.0f.w1 79 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTTPD2DQ-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VCVTTPD2DQ"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.66.0f.w1 e6 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTTPD2DQ-xmmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VCVTTPD2DQ"
:operands "xmmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.66.0f.w1 e6 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTTPD2DQ-ymmreg-mask-z.zmmrm512-b64-sae (make-instance 'x86-asm-instruction
:name "VCVTTPD2DQ"
:operands "ymmreg|mask|z,zmmrm512|b64|sae"
:code-string "[rm:fv: evex.512.66.0f.w1 e6 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTTPD2QQ-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VCVTTPD2QQ"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.66.0f.w1 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTTPD2QQ-ymmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VCVTTPD2QQ"
:operands "ymmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.66.0f.w1 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTTPD2QQ-zmmreg-mask-z.zmmrm512-b64-sae (make-instance 'x86-asm-instruction
:name "VCVTTPD2QQ"
:operands "zmmreg|mask|z,zmmrm512|b64|sae"
:code-string "[rm:fv: evex.512.66.0f.w1 7a /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VCVTTPD2UDQ-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VCVTTPD2UDQ"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.0f.w1 78 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTTPD2UDQ-xmmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VCVTTPD2UDQ"
:operands "xmmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.0f.w1 78 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTTPD2UDQ-ymmreg-mask-z.zmmrm512-b64-sae (make-instance 'x86-asm-instruction
:name "VCVTTPD2UDQ"
:operands "ymmreg|mask|z,zmmrm512|b64|sae"
:code-string "[rm:fv: evex.512.0f.w1 78 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTTPD2UQQ-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VCVTTPD2UQQ"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.66.0f.w1 78 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTTPD2UQQ-ymmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VCVTTPD2UQQ"
:operands "ymmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.66.0f.w1 78 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTTPD2UQQ-zmmreg-mask-z.zmmrm512-b64-sae (make-instance 'x86-asm-instruction
:name "VCVTTPD2UQQ"
:operands "zmmreg|mask|z,zmmrm512|b64|sae"
:code-string "[rm:fv: evex.512.66.0f.w1 78 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VCVTTPS2DQ-xmmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VCVTTPS2DQ"
:operands "xmmreg|mask|z,xmmrm128|b32"
:code-string "[rm:fv: evex.128.f3.0f.w0 5b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTTPS2DQ-ymmreg-mask-z.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VCVTTPS2DQ"
:operands "ymmreg|mask|z,ymmrm256|b32"
:code-string "[rm:fv: evex.256.f3.0f.w0 5b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTTPS2DQ-zmmreg-mask-z.zmmrm512-b32-sae (make-instance 'x86-asm-instruction
:name "VCVTTPS2DQ"
:operands "zmmreg|mask|z,zmmrm512|b32|sae"
:code-string "[rm:fv: evex.512.f3.0f.w0 5b /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTTPS2QQ-xmmreg-mask-z.xmmrm64-b32 (make-instance 'x86-asm-instruction
:name "VCVTTPS2QQ"
:operands "xmmreg|mask|z,xmmrm64|b32"
:code-string "[rm:hv: evex.128.66.0f.w0 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTTPS2QQ-ymmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VCVTTPS2QQ"
:operands "ymmreg|mask|z,xmmrm128|b32"
:code-string "[rm:hv: evex.256.66.0f.w0 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTTPS2QQ-zmmreg-mask-z.ymmrm256-b32-sae (make-instance 'x86-asm-instruction
:name "VCVTTPS2QQ"
:operands "zmmreg|mask|z,ymmrm256|b32|sae"
:code-string "[rm:hv: evex.512.66.0f.w0 7a /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VCVTTPS2UDQ-xmmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VCVTTPS2UDQ"
:operands "xmmreg|mask|z,xmmrm128|b32"
:code-string "[rm:fv: evex.128.0f.w0 78 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTTPS2UDQ-ymmreg-mask-z.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VCVTTPS2UDQ"
:operands "ymmreg|mask|z,ymmrm256|b32"
:code-string "[rm:fv: evex.256.0f.w0 78 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTTPS2UDQ-zmmreg-mask-z.zmmrm512-b32-sae (make-instance 'x86-asm-instruction
:name "VCVTTPS2UDQ"
:operands "zmmreg|mask|z,zmmrm512|b32|sae"
:code-string "[rm:fv: evex.512.0f.w0 78 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTTPS2UQQ-xmmreg-mask-z.xmmrm64-b32 (make-instance 'x86-asm-instruction
:name "VCVTTPS2UQQ"
:operands "xmmreg|mask|z,xmmrm64|b32"
:code-string "[rm:hv: evex.128.66.0f.w0 78 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTTPS2UQQ-ymmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VCVTTPS2UQQ"
:operands "ymmreg|mask|z,xmmrm128|b32"
:code-string "[rm:hv: evex.256.66.0f.w0 78 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTTPS2UQQ-zmmreg-mask-z.ymmrm256-b32-sae (make-instance 'x86-asm-instruction
:name "VCVTTPS2UQQ"
:operands "zmmreg|mask|z,ymmrm256|b32|sae"
:code-string "[rm:hv: evex.512.66.0f.w0 78 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VCVTTSD2SI-reg32.xmmrm64-sae (make-instance 'x86-asm-instruction
:name "VCVTTSD2SI"
:operands "reg32,xmmrm64|sae"
:code-string "[rm:t1f64: evex.128.f2.0f.w0 2c /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTTSD2SI-reg64.xmmrm64-sae (make-instance 'x86-asm-instruction
:name "VCVTTSD2SI"
:operands "reg64,xmmrm64|sae"
:code-string "[rm:t1f64: evex.128.f2.0f.w1 2c /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTTSD2USI-reg32.xmmrm64-sae (make-instance 'x86-asm-instruction
:name "VCVTTSD2USI"
:operands "reg32,xmmrm64|sae"
:code-string "[rm:t1f64: evex.128.f2.0f.w0 78 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTTSD2USI-reg64.xmmrm64-sae (make-instance 'x86-asm-instruction
:name "VCVTTSD2USI"
:operands "reg64,xmmrm64|sae"
:code-string "[rm:t1f64: evex.128.f2.0f.w1 78 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTTSS2SI-reg32.xmmrm32-sae (make-instance 'x86-asm-instruction
:name "VCVTTSS2SI"
:operands "reg32,xmmrm32|sae"
:code-string "[rm:t1f32: evex.128.f3.0f.w0 2c /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTTSS2SI-reg64.xmmrm32-sae (make-instance 'x86-asm-instruction
:name "VCVTTSS2SI"
:operands "reg64,xmmrm32|sae"
:code-string "[rm:t1f32: evex.128.f3.0f.w1 2c /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTTSS2USI-reg32.xmmrm32-sae (make-instance 'x86-asm-instruction
:name "VCVTTSS2USI"
:operands "reg32,xmmrm32|sae"
:code-string "[rm:t1f32: evex.128.f3.0f.w0 78 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTTSS2USI-reg64.xmmrm32-sae (make-instance 'x86-asm-instruction
:name "VCVTTSS2USI"
:operands "reg64,xmmrm32|sae"
:code-string "[rm:t1f32: evex.128.f3.0f.w1 78 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTUDQ2PD-xmmreg-mask-z.xmmrm64-b32 (make-instance 'x86-asm-instruction
:name "VCVTUDQ2PD"
:operands "xmmreg|mask|z,xmmrm64|b32"
:code-string "[rm:hv: evex.128.f3.0f.w0 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTUDQ2PD-ymmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VCVTUDQ2PD"
:operands "ymmreg|mask|z,xmmrm128|b32"
:code-string "[rm:hv: evex.256.f3.0f.w0 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTUDQ2PD-zmmreg-mask-z.ymmrm256-b32-er (make-instance 'x86-asm-instruction
:name "VCVTUDQ2PD"
:operands "zmmreg|mask|z,ymmrm256|b32|er"
:code-string "[rm:hv: evex.512.f3.0f.w0 7a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTUDQ2PS-xmmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VCVTUDQ2PS"
:operands "xmmreg|mask|z,xmmrm128|b32"
:code-string "[rm:fv: evex.128.f2.0f.w0 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTUDQ2PS-ymmreg-mask-z.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VCVTUDQ2PS"
:operands "ymmreg|mask|z,ymmrm256|b32"
:code-string "[rm:fv: evex.256.f2.0f.w0 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VCVTUDQ2PS-zmmreg-mask-z.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VCVTUDQ2PS"
:operands "zmmreg|mask|z,zmmrm512|b32|er"
:code-string "[rm:fv: evex.512.f2.0f.w0 7a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTUQQ2PD-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VCVTUQQ2PD"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.f3.0f.w1 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTUQQ2PD-ymmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VCVTUQQ2PD"
:operands "ymmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.f3.0f.w1 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTUQQ2PD-zmmreg-mask-z.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VCVTUQQ2PD"
:operands "zmmreg|mask|z,zmmrm512|b64|er"
:code-string "[rm:fv: evex.512.f3.0f.w1 7a /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VCVTUQQ2PS-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VCVTUQQ2PS"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.f2.0f.w1 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTUQQ2PS-xmmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VCVTUQQ2PS"
:operands "xmmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.f2.0f.w1 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VCVTUQQ2PS-ymmreg-mask-z.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VCVTUQQ2PS"
:operands "ymmreg|mask|z,zmmrm512|b64|er"
:code-string "[rm:fv: evex.512.f2.0f.w1 7a /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VCVTUSI2SD-xmmreg.xmmreg-er.rm32 (make-instance 'x86-asm-instruction
:name "VCVTUSI2SD"
:operands "xmmreg,xmmreg|er,rm32"
:code-string "[rvm:t1s: evex.nds.128.f2.0f.w0 7b /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTUSI2SD-xmmreg.xmmreg-er.rm64 (make-instance 'x86-asm-instruction
:name "VCVTUSI2SD"
:operands "xmmreg,xmmreg|er,rm64"
:code-string "[rvm:t1s: evex.nds.128.f2.0f.w1 7b /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTUSI2SS-xmmreg.xmmreg-er.rm32 (make-instance 'x86-asm-instruction
:name "VCVTUSI2SS"
:operands "xmmreg,xmmreg|er,rm32"
:code-string "[rvm:t1s: evex.nds.128.f3.0f.w0 7b /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VCVTUSI2SS-xmmreg.xmmreg-er.rm64 (make-instance 'x86-asm-instruction
:name "VCVTUSI2SS"
:operands "xmmreg,xmmreg|er,rm64"
:code-string "[rvm:t1s: evex.nds.128.f3.0f.w1 7b /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VDBPSADBW-xmmreg-mask-z.xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VDBPSADBW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128,imm8"
:code-string "[rvmi:fvm: evex.nds.128.66.0f3a.w0 42 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VDBPSADBW-ymmreg-mask-z.ymmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VDBPSADBW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256,imm8"
:code-string "[rvmi:fvm: evex.nds.256.66.0f3a.w0 42 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VDBPSADBW-zmmreg-mask-z.zmmreg.zmmrm512.imm8 (make-instance 'x86-asm-instruction
:name "VDBPSADBW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512,imm8"
:code-string "[rvmi:fvm: evex.nds.512.66.0f3a.w0 42 /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VDIVPD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VDIVPD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 5e /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VDIVPD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VDIVPD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 5e /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VDIVPD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VDIVPD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 5e /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VDIVPS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VDIVPS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.0f.w0 5e /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VDIVPS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VDIVPS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.0f.w0 5e /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VDIVPS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VDIVPS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.0f.w0 5e /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VDIVSD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VDIVSD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|er"
:code-string "[rvm:t1s: evex.nds.128.f2.0f.w1 5e /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VDIVSS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VDIVSS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|er"
:code-string "[rvm:t1s: evex.nds.128.f3.0f.w0 5e /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VEXP2PD-zmmreg-mask-z.zmmrm512-b64-sae (make-instance 'x86-asm-instruction
:name "VEXP2PD"
:operands "zmmreg|mask|z,zmmrm512|b64|sae"
:code-string "[rm:fv: evex.512.66.0f38.w1 c8 /r ]"
:arch-flags (list "AVX512ER" "FUTURE")))

(setf VEXP2PS-zmmreg-mask-z.zmmrm512-b32-sae (make-instance 'x86-asm-instruction
:name "VEXP2PS"
:operands "zmmreg|mask|z,zmmrm512|b32|sae"
:code-string "[rm:fv: evex.512.66.0f38.w0 c8 /r ]"
:arch-flags (list "AVX512ER" "FUTURE")))

(setf VEXPANDPD-xmmreg-mask-z.mem128 (make-instance 'x86-asm-instruction
:name "VEXPANDPD"
:operands "xmmreg|mask|z,mem128"
:code-string "[rm:t1s: evex.128.66.0f38.w1 88 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VEXPANDPD-ymmreg-mask-z.mem256 (make-instance 'x86-asm-instruction
:name "VEXPANDPD"
:operands "ymmreg|mask|z,mem256"
:code-string "[rm:t1s: evex.256.66.0f38.w1 88 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VEXPANDPD-zmmreg-mask-z.mem512 (make-instance 'x86-asm-instruction
:name "VEXPANDPD"
:operands "zmmreg|mask|z,mem512"
:code-string "[rm:t1s: evex.512.66.0f38.w1 88 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VEXPANDPD-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VEXPANDPD"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[rm:t1s: evex.128.66.0f38.w1 88 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VEXPANDPD-ymmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VEXPANDPD"
:operands "ymmreg|mask|z,ymmreg"
:code-string "[rm:t1s: evex.256.66.0f38.w1 88 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VEXPANDPD-zmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VEXPANDPD"
:operands "zmmreg|mask|z,zmmreg"
:code-string "[rm:t1s: evex.512.66.0f38.w1 88 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VEXPANDPS-xmmreg-mask-z.mem128 (make-instance 'x86-asm-instruction
:name "VEXPANDPS"
:operands "xmmreg|mask|z,mem128"
:code-string "[rm:t1s: evex.128.66.0f38.w0 88 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VEXPANDPS-ymmreg-mask-z.mem256 (make-instance 'x86-asm-instruction
:name "VEXPANDPS"
:operands "ymmreg|mask|z,mem256"
:code-string "[rm:t1s: evex.256.66.0f38.w0 88 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VEXPANDPS-zmmreg-mask-z.mem512 (make-instance 'x86-asm-instruction
:name "VEXPANDPS"
:operands "zmmreg|mask|z,mem512"
:code-string "[rm:t1s: evex.512.66.0f38.w0 88 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VEXPANDPS-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VEXPANDPS"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[rm:t1s: evex.128.66.0f38.w0 88 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VEXPANDPS-ymmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VEXPANDPS"
:operands "ymmreg|mask|z,ymmreg"
:code-string "[rm:t1s: evex.256.66.0f38.w0 88 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VEXPANDPS-zmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VEXPANDPS"
:operands "zmmreg|mask|z,zmmreg"
:code-string "[rm:t1s: evex.512.66.0f38.w0 88 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VEXTRACTF32X4-xmmreg-mask-z.ymmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTF32X4"
:operands "xmmreg|mask|z,ymmreg,imm8"
:code-string "[mri: evex.256.66.0f3a.w0 19 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VEXTRACTF32X4-xmmreg-mask-z.zmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTF32X4"
:operands "xmmreg|mask|z,zmmreg,imm8"
:code-string "[mri: evex.512.66.0f3a.w0 19 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VEXTRACTF32X4-mem128-mask.ymmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTF32X4"
:operands "mem128|mask,ymmreg,imm8"
:code-string "[mri:t4: evex.256.66.0f3a.w0 19 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VEXTRACTF32X4-mem128-mask.zmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTF32X4"
:operands "mem128|mask,zmmreg,imm8"
:code-string "[mri:t4: evex.512.66.0f3a.w0 19 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VEXTRACTF32X8-ymmreg-mask-z.zmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTF32X8"
:operands "ymmreg|mask|z,zmmreg,imm8"
:code-string "[mri: evex.512.66.0f3a.w0 1b /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VEXTRACTF32X8-mem256-mask.zmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTF32X8"
:operands "mem256|mask,zmmreg,imm8"
:code-string "[mri:t8: evex.512.66.0f3a.w0 1b /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VEXTRACTF64X2-xmmreg-mask-z.ymmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTF64X2"
:operands "xmmreg|mask|z,ymmreg,imm8"
:code-string "[mri: evex.256.66.0f3a.w1 19 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VEXTRACTF64X2-xmmreg-mask-z.zmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTF64X2"
:operands "xmmreg|mask|z,zmmreg,imm8"
:code-string "[mri: evex.512.66.0f3a.w1 19 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VEXTRACTF64X2-mem128-mask.ymmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTF64X2"
:operands "mem128|mask,ymmreg,imm8"
:code-string "[mri:t2: evex.256.66.0f3a.w1 19 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VEXTRACTF64X2-mem128-mask.zmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTF64X2"
:operands "mem128|mask,zmmreg,imm8"
:code-string "[mri:t2: evex.512.66.0f3a.w1 19 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VEXTRACTF64X4-ymmreg-mask-z.zmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTF64X4"
:operands "ymmreg|mask|z,zmmreg,imm8"
:code-string "[mri: evex.512.66.0f3a.w1 1b /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VEXTRACTF64X4-mem256-mask.zmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTF64X4"
:operands "mem256|mask,zmmreg,imm8"
:code-string "[mri:t4: evex.512.66.0f3a.w1 1b /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VEXTRACTI32X4-xmmreg-mask-z.ymmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTI32X4"
:operands "xmmreg|mask|z,ymmreg,imm8"
:code-string "[mri: evex.256.66.0f3a.w0 39 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VEXTRACTI32X4-xmmreg-mask-z.zmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTI32X4"
:operands "xmmreg|mask|z,zmmreg,imm8"
:code-string "[mri: evex.512.66.0f3a.w0 39 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VEXTRACTI32X4-mem128-mask.ymmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTI32X4"
:operands "mem128|mask,ymmreg,imm8"
:code-string "[mri:t4: evex.256.66.0f3a.w0 39 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VEXTRACTI32X4-mem128-mask.zmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTI32X4"
:operands "mem128|mask,zmmreg,imm8"
:code-string "[mri:t4: evex.512.66.0f3a.w0 39 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VEXTRACTI32X8-ymmreg-mask-z.zmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTI32X8"
:operands "ymmreg|mask|z,zmmreg,imm8"
:code-string "[mri: evex.512.66.0f3a.w0 3b /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VEXTRACTI32X8-mem256-mask.zmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTI32X8"
:operands "mem256|mask,zmmreg,imm8"
:code-string "[mri:t8: evex.512.66.0f3a.w0 3b /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VEXTRACTI64X2-xmmreg-mask-z.ymmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTI64X2"
:operands "xmmreg|mask|z,ymmreg,imm8"
:code-string "[mri: evex.256.66.0f3a.w1 39 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VEXTRACTI64X2-xmmreg-mask-z.zmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTI64X2"
:operands "xmmreg|mask|z,zmmreg,imm8"
:code-string "[mri: evex.512.66.0f3a.w1 39 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VEXTRACTI64X2-mem128-mask.ymmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTI64X2"
:operands "mem128|mask,ymmreg,imm8"
:code-string "[mri:t2: evex.256.66.0f3a.w1 39 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VEXTRACTI64X2-mem128-mask.zmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTI64X2"
:operands "mem128|mask,zmmreg,imm8"
:code-string "[mri:t2: evex.512.66.0f3a.w1 39 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VEXTRACTI64X4-ymmreg-mask-z.zmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTI64X4"
:operands "ymmreg|mask|z,zmmreg,imm8"
:code-string "[mri: evex.512.66.0f3a.w1 3b /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VEXTRACTI64X4-mem256-mask.zmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTI64X4"
:operands "mem256|mask,zmmreg,imm8"
:code-string "[mri:t4: evex.512.66.0f3a.w1 3b /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VEXTRACTPS-reg32.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTPS"
:operands "reg32,xmmreg,imm8"
:code-string "[mri:t1s: evex.128.66.0f3a.wig 17 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VEXTRACTPS-reg64.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTPS"
:operands "reg64,xmmreg,imm8"
:code-string "[mri:t1s: evex.128.66.0f3a.wig 17 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VEXTRACTPS-mem32.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VEXTRACTPS"
:operands "mem32,xmmreg,imm8"
:code-string "[mri:t1s: evex.128.66.0f3a.wig 17 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFIXUPIMMPD-xmmreg-mask-z.xmmreg.xmmrm128-b64.imm8 (make-instance 'x86-asm-instruction
:name "VFIXUPIMMPD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64,imm8"
:code-string "[rvmi:fv: evex.nds.128.66.0f3a.w1 54 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFIXUPIMMPD-ymmreg-mask-z.ymmreg.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VFIXUPIMMPD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64,imm8"
:code-string "[rvmi:fv: evex.nds.256.66.0f3a.w1 54 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFIXUPIMMPD-zmmreg-mask-z.zmmreg.zmmrm512-b64-sae.imm8 (make-instance 'x86-asm-instruction
:name "VFIXUPIMMPD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|sae,imm8"
:code-string "[rvmi:fv: evex.nds.512.66.0f3a.w1 54 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFIXUPIMMPS-xmmreg-mask-z.xmmreg.xmmrm128-b32.imm8 (make-instance 'x86-asm-instruction
:name "VFIXUPIMMPS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32,imm8"
:code-string "[rvmi:fv: evex.nds.128.66.0f3a.w0 54 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFIXUPIMMPS-ymmreg-mask-z.ymmreg.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VFIXUPIMMPS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32,imm8"
:code-string "[rvmi:fv: evex.nds.256.66.0f3a.w0 54 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFIXUPIMMPS-zmmreg-mask-z.zmmreg.zmmrm512-b32-sae.imm8 (make-instance 'x86-asm-instruction
:name "VFIXUPIMMPS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|sae,imm8"
:code-string "[rvmi:fv: evex.nds.512.66.0f3a.w0 54 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFIXUPIMMSD-xmmreg-mask-z.xmmreg.xmmrm64-sae.imm8 (make-instance 'x86-asm-instruction
:name "VFIXUPIMMSD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|sae,imm8"
:code-string "[rvmi:t1s: evex.nds.128.66.0f3a.w1 55 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFIXUPIMMSS-xmmreg-mask-z.xmmreg.xmmrm32-sae.imm8 (make-instance 'x86-asm-instruction
:name "VFIXUPIMMSS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|sae,imm8"
:code-string "[rvmi:t1s: evex.nds.128.66.0f3a.w0 55 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMADD132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VFMADD132PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 98 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADD132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VFMADD132PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 98 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADD132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VFMADD132PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 98 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMADD132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VFMADD132PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 98 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADD132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VFMADD132PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 98 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADD132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VFMADD132PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 98 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMADD132SD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VFMADD132SD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w1 99 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMADD132SS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VFMADD132SS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w0 99 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMADD213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VFMADD213PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 a8 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADD213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VFMADD213PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 a8 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADD213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VFMADD213PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 a8 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMADD213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VFMADD213PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 a8 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADD213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VFMADD213PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 a8 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADD213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VFMADD213PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 a8 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMADD213SD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VFMADD213SD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w1 a9 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMADD213SS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VFMADD213SS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w0 a9 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMADD231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VFMADD231PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 b8 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADD231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VFMADD231PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 b8 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADD231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VFMADD231PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 b8 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMADD231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VFMADD231PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 b8 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADD231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VFMADD231PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 b8 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADD231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VFMADD231PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 b8 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMADD231SD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VFMADD231SD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w1 b9 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMADD231SS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VFMADD231SS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w0 b9 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMADDSUB132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VFMADDSUB132PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 96 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADDSUB132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VFMADDSUB132PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 96 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADDSUB132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VFMADDSUB132PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 96 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMADDSUB132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VFMADDSUB132PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 96 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADDSUB132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VFMADDSUB132PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 96 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADDSUB132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VFMADDSUB132PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 96 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMADDSUB213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VFMADDSUB213PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 a6 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADDSUB213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VFMADDSUB213PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 a6 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADDSUB213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VFMADDSUB213PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 a6 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMADDSUB213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VFMADDSUB213PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 a6 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADDSUB213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VFMADDSUB213PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 a6 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADDSUB213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VFMADDSUB213PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 a6 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMADDSUB231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VFMADDSUB231PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 b6 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADDSUB231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VFMADDSUB231PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 b6 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADDSUB231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VFMADDSUB231PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 b6 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMADDSUB231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VFMADDSUB231PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 b6 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADDSUB231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VFMADDSUB231PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 b6 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMADDSUB231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VFMADDSUB231PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 b6 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMSUB132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VFMSUB132PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 9a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUB132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VFMSUB132PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 9a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUB132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VFMSUB132PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 9a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMSUB132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VFMSUB132PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 9a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUB132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VFMSUB132PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 9a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUB132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VFMSUB132PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 9a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMSUB132SD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VFMSUB132SD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w1 9b /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMSUB132SS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VFMSUB132SS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w0 9b /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMSUB213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VFMSUB213PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 aa /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUB213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VFMSUB213PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 aa /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUB213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VFMSUB213PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 aa /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMSUB213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VFMSUB213PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 aa /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUB213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VFMSUB213PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 aa /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUB213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VFMSUB213PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 aa /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMSUB213SD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VFMSUB213SD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w1 ab /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMSUB213SS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VFMSUB213SS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w0 ab /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMSUB231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VFMSUB231PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 ba /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUB231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VFMSUB231PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 ba /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUB231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VFMSUB231PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 ba /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMSUB231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VFMSUB231PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 ba /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUB231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VFMSUB231PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 ba /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUB231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VFMSUB231PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 ba /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMSUB231SD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VFMSUB231SD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w1 bb /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMSUB231SS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VFMSUB231SS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w0 bb /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMSUBADD132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VFMSUBADD132PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 97 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUBADD132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VFMSUBADD132PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 97 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUBADD132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VFMSUBADD132PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 97 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMSUBADD132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VFMSUBADD132PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 97 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUBADD132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VFMSUBADD132PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 97 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUBADD132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VFMSUBADD132PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 97 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMSUBADD213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VFMSUBADD213PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 a7 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUBADD213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VFMSUBADD213PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 a7 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUBADD213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VFMSUBADD213PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 a7 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMSUBADD213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VFMSUBADD213PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 a7 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUBADD213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VFMSUBADD213PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 a7 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUBADD213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VFMSUBADD213PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 a7 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMSUBADD231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VFMSUBADD231PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 b7 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUBADD231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VFMSUBADD231PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 b7 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUBADD231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VFMSUBADD231PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 b7 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFMSUBADD231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VFMSUBADD231PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 b7 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUBADD231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VFMSUBADD231PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 b7 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFMSUBADD231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VFMSUBADD231PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 b7 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMADD132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VFNMADD132PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 9c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMADD132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VFNMADD132PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 9c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMADD132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VFNMADD132PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 9c /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMADD132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VFNMADD132PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 9c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMADD132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VFNMADD132PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 9c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMADD132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VFNMADD132PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 9c /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMADD132SD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VFNMADD132SD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w1 9d /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMADD132SS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VFNMADD132SS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w0 9d /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMADD213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VFNMADD213PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 ac /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMADD213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VFNMADD213PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 ac /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMADD213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VFNMADD213PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 ac /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMADD213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VFNMADD213PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 ac /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMADD213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VFNMADD213PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 ac /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMADD213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VFNMADD213PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 ac /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMADD213SD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VFNMADD213SD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w1 ad /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMADD213SS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VFNMADD213SS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w0 ad /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMADD231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VFNMADD231PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 bc /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMADD231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VFNMADD231PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 bc /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMADD231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VFNMADD231PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 bc /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMADD231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VFNMADD231PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 bc /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMADD231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VFNMADD231PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 bc /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMADD231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VFNMADD231PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 bc /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMADD231SD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VFNMADD231SD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w1 bd /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMADD231SS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VFNMADD231SS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w0 bd /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMSUB132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VFNMSUB132PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 9e /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMSUB132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VFNMSUB132PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 9e /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMSUB132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VFNMSUB132PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 9e /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMSUB132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VFNMSUB132PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 9e /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMSUB132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VFNMSUB132PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 9e /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMSUB132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VFNMSUB132PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 9e /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMSUB132SD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VFNMSUB132SD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w1 9f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMSUB132SS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VFNMSUB132SS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w0 9f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMSUB213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VFNMSUB213PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 ae /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMSUB213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VFNMSUB213PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 ae /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMSUB213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VFNMSUB213PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 ae /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMSUB213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VFNMSUB213PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 ae /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMSUB213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VFNMSUB213PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 ae /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMSUB213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VFNMSUB213PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 ae /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMSUB213SD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VFNMSUB213SD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w1 af /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMSUB213SS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VFNMSUB213SS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w0 af /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMSUB231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VFNMSUB231PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 be /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMSUB231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VFNMSUB231PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 be /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMSUB231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VFNMSUB231PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 be /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMSUB231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VFNMSUB231PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 be /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMSUB231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VFNMSUB231PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 be /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VFNMSUB231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VFNMSUB231PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 be /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMSUB231SD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VFNMSUB231SD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w1 bf /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFNMSUB231SS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VFNMSUB231SS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w0 bf /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VFPCLASSPD-kreg-mask.xmmrm128-b64.imm8 (make-instance 'x86-asm-instruction
:name "VFPCLASSPD"
:operands "kreg|mask,xmmrm128|b64,imm8"
:code-string "[rmi:fv: evex.128.66.0f3a.w1 66 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VFPCLASSPD-kreg-mask.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VFPCLASSPD"
:operands "kreg|mask,ymmrm256|b64,imm8"
:code-string "[rmi:fv: evex.256.66.0f3a.w1 66 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VFPCLASSPD-kreg-mask.zmmrm512-b64.imm8 (make-instance 'x86-asm-instruction
:name "VFPCLASSPD"
:operands "kreg|mask,zmmrm512|b64,imm8"
:code-string "[rmi:fv: evex.512.66.0f3a.w1 66 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VFPCLASSPS-kreg-mask.xmmrm128-b32.imm8 (make-instance 'x86-asm-instruction
:name "VFPCLASSPS"
:operands "kreg|mask,xmmrm128|b32,imm8"
:code-string "[rmi:fv: evex.128.66.0f3a.w0 66 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VFPCLASSPS-kreg-mask.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VFPCLASSPS"
:operands "kreg|mask,ymmrm256|b32,imm8"
:code-string "[rmi:fv: evex.256.66.0f3a.w0 66 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VFPCLASSPS-kreg-mask.zmmrm512-b32.imm8 (make-instance 'x86-asm-instruction
:name "VFPCLASSPS"
:operands "kreg|mask,zmmrm512|b32,imm8"
:code-string "[rmi:fv: evex.512.66.0f3a.w0 66 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VFPCLASSSD-kreg-mask.xmmrm64.imm8 (make-instance 'x86-asm-instruction
:name "VFPCLASSSD"
:operands "kreg|mask,xmmrm64,imm8"
:code-string "[rmi:t1s: evex.128.66.0f3a.w1 67 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VFPCLASSSS-kreg-mask.xmmrm32.imm8 (make-instance 'x86-asm-instruction
:name "VFPCLASSSS"
:operands "kreg|mask,xmmrm32,imm8"
:code-string "[rmi:t1s: evex.128.66.0f3a.w0 67 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VGATHERDPD-xmmreg-mask.xmem64 (make-instance 'x86-asm-instruction
:name "VGATHERDPD"
:operands "xmmreg|mask,xmem64"
:code-string "[rm:t1s: vsibx evex.128.66.0f38.w1 92 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VGATHERDPD-ymmreg-mask.xmem64 (make-instance 'x86-asm-instruction
:name "VGATHERDPD"
:operands "ymmreg|mask,xmem64"
:code-string "[rm:t1s: vsibx evex.256.66.0f38.w1 92 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VGATHERDPD-zmmreg-mask.ymem64 (make-instance 'x86-asm-instruction
:name "VGATHERDPD"
:operands "zmmreg|mask,ymem64"
:code-string "[rm:t1s: vsiby evex.512.66.0f38.w1 92 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VGATHERDPS-xmmreg-mask.xmem32 (make-instance 'x86-asm-instruction
:name "VGATHERDPS"
:operands "xmmreg|mask,xmem32"
:code-string "[rm:t1s: vsibx evex.128.66.0f38.w0 92 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VGATHERDPS-ymmreg-mask.ymem32 (make-instance 'x86-asm-instruction
:name "VGATHERDPS"
:operands "ymmreg|mask,ymem32"
:code-string "[rm:t1s: vsiby evex.256.66.0f38.w0 92 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VGATHERDPS-zmmreg-mask.zmem32 (make-instance 'x86-asm-instruction
:name "VGATHERDPS"
:operands "zmmreg|mask,zmem32"
:code-string "[rm:t1s: vsibz evex.512.66.0f38.w0 92 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VGATHERPF0DPD-ymem64-mask (make-instance 'x86-asm-instruction
:name "VGATHERPF0DPD"
:operands "ymem64|mask"
:code-string "[m:t1s: vsiby evex.512.66.0f38.w1 c6 /1 ]"
:arch-flags (list "AVX512PF" "FUTURE")))

(setf VGATHERPF0DPS-zmem32-mask (make-instance 'x86-asm-instruction
:name "VGATHERPF0DPS"
:operands "zmem32|mask"
:code-string "[m:t1s: vsibz evex.512.66.0f38.w0 c6 /1 ]"
:arch-flags (list "AVX512PF" "FUTURE")))

(setf VGATHERPF0QPD-zmem64-mask (make-instance 'x86-asm-instruction
:name "VGATHERPF0QPD"
:operands "zmem64|mask"
:code-string "[m:t1s: vsibz evex.512.66.0f38.w1 c7 /1 ]"
:arch-flags (list "AVX512PF" "FUTURE")))

(setf VGATHERPF0QPS-zmem32-mask (make-instance 'x86-asm-instruction
:name "VGATHERPF0QPS"
:operands "zmem32|mask"
:code-string "[m:t1s: vsibz evex.512.66.0f38.w0 c7 /1 ]"
:arch-flags (list "AVX512PF" "FUTURE")))

(setf VGATHERPF1DPD-ymem64-mask (make-instance 'x86-asm-instruction
:name "VGATHERPF1DPD"
:operands "ymem64|mask"
:code-string "[m:t1s: vsiby evex.512.66.0f38.w1 c6 /2 ]"
:arch-flags (list "AVX512PF" "FUTURE")))

(setf VGATHERPF1DPS-zmem32-mask (make-instance 'x86-asm-instruction
:name "VGATHERPF1DPS"
:operands "zmem32|mask"
:code-string "[m:t1s: vsibz evex.512.66.0f38.w0 c6 /2 ]"
:arch-flags (list "AVX512PF" "FUTURE")))

(setf VGATHERPF1QPD-zmem64-mask (make-instance 'x86-asm-instruction
:name "VGATHERPF1QPD"
:operands "zmem64|mask"
:code-string "[m:t1s: vsibz evex.512.66.0f38.w1 c7 /2 ]"
:arch-flags (list "AVX512PF" "FUTURE")))

(setf VGATHERPF1QPS-zmem32-mask (make-instance 'x86-asm-instruction
:name "VGATHERPF1QPS"
:operands "zmem32|mask"
:code-string "[m:t1s: vsibz evex.512.66.0f38.w0 c7 /2 ]"
:arch-flags (list "AVX512PF" "FUTURE")))

(setf VGATHERQPD-xmmreg-mask.xmem64 (make-instance 'x86-asm-instruction
:name "VGATHERQPD"
:operands "xmmreg|mask,xmem64"
:code-string "[rm:t1s: vsibx evex.128.66.0f38.w1 93 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VGATHERQPD-ymmreg-mask.ymem64 (make-instance 'x86-asm-instruction
:name "VGATHERQPD"
:operands "ymmreg|mask,ymem64"
:code-string "[rm:t1s: vsiby evex.256.66.0f38.w1 93 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VGATHERQPD-zmmreg-mask.zmem64 (make-instance 'x86-asm-instruction
:name "VGATHERQPD"
:operands "zmmreg|mask,zmem64"
:code-string "[rm:t1s: vsibz evex.512.66.0f38.w1 93 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VGATHERQPS-xmmreg-mask.xmem32 (make-instance 'x86-asm-instruction
:name "VGATHERQPS"
:operands "xmmreg|mask,xmem32"
:code-string "[rm:t1s: vsibx evex.128.66.0f38.w0 93 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VGATHERQPS-xmmreg-mask.ymem32 (make-instance 'x86-asm-instruction
:name "VGATHERQPS"
:operands "xmmreg|mask,ymem32"
:code-string "[rm:t1s: vsiby evex.256.66.0f38.w0 93 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VGATHERQPS-ymmreg-mask.zmem32 (make-instance 'x86-asm-instruction
:name "VGATHERQPS"
:operands "ymmreg|mask,zmem32"
:code-string "[rm:t1s: vsibz evex.512.66.0f38.w0 93 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VGETEXPPD-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VGETEXPPD"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.66.0f38.w1 42 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VGETEXPPD-ymmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VGETEXPPD"
:operands "ymmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.66.0f38.w1 42 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VGETEXPPD-zmmreg-mask-z.zmmrm512-b64-sae (make-instance 'x86-asm-instruction
:name "VGETEXPPD"
:operands "zmmreg|mask|z,zmmrm512|b64|sae"
:code-string "[rm:fv: evex.512.66.0f38.w1 42 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VGETEXPPS-xmmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VGETEXPPS"
:operands "xmmreg|mask|z,xmmrm128|b32"
:code-string "[rm:fv: evex.128.66.0f38.w0 42 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VGETEXPPS-ymmreg-mask-z.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VGETEXPPS"
:operands "ymmreg|mask|z,ymmrm256|b32"
:code-string "[rm:fv: evex.256.66.0f38.w0 42 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VGETEXPPS-zmmreg-mask-z.zmmrm512-b32-sae (make-instance 'x86-asm-instruction
:name "VGETEXPPS"
:operands "zmmreg|mask|z,zmmrm512|b32|sae"
:code-string "[rm:fv: evex.512.66.0f38.w0 42 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VGETEXPSD-xmmreg-mask-z.xmmreg.xmmrm64-sae (make-instance 'x86-asm-instruction
:name "VGETEXPSD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|sae"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w1 43 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VGETEXPSS-xmmreg-mask-z.xmmreg.xmmrm32-sae (make-instance 'x86-asm-instruction
:name "VGETEXPSS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|sae"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w0 43 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VGETMANTPD-xmmreg-mask-z.xmmrm128-b64.imm8 (make-instance 'x86-asm-instruction
:name "VGETMANTPD"
:operands "xmmreg|mask|z,xmmrm128|b64,imm8"
:code-string "[rmi:fv: evex.128.66.0f3a.w1 26 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VGETMANTPD-ymmreg-mask-z.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VGETMANTPD"
:operands "ymmreg|mask|z,ymmrm256|b64,imm8"
:code-string "[rmi:fv: evex.256.66.0f3a.w1 26 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VGETMANTPD-zmmreg-mask-z.zmmrm512-b64-sae.imm8 (make-instance 'x86-asm-instruction
:name "VGETMANTPD"
:operands "zmmreg|mask|z,zmmrm512|b64|sae,imm8"
:code-string "[rmi:fv: evex.512.66.0f3a.w1 26 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VGETMANTPS-xmmreg-mask-z.xmmrm128-b32.imm8 (make-instance 'x86-asm-instruction
:name "VGETMANTPS"
:operands "xmmreg|mask|z,xmmrm128|b32,imm8"
:code-string "[rmi:fv: evex.128.66.0f3a.w0 26 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VGETMANTPS-ymmreg-mask-z.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VGETMANTPS"
:operands "ymmreg|mask|z,ymmrm256|b32,imm8"
:code-string "[rmi:fv: evex.256.66.0f3a.w0 26 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VGETMANTPS-zmmreg-mask-z.zmmrm512-b32-sae.imm8 (make-instance 'x86-asm-instruction
:name "VGETMANTPS"
:operands "zmmreg|mask|z,zmmrm512|b32|sae,imm8"
:code-string "[rmi:fv: evex.512.66.0f3a.w0 26 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VGETMANTSD-xmmreg-mask-z.xmmreg.xmmrm64-sae.imm8 (make-instance 'x86-asm-instruction
:name "VGETMANTSD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|sae,imm8"
:code-string "[rvmi:t1s: evex.nds.128.66.0f3a.w1 27 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VGETMANTSS-xmmreg-mask-z.xmmreg.xmmrm32-sae.imm8 (make-instance 'x86-asm-instruction
:name "VGETMANTSS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|sae,imm8"
:code-string "[rvmi:t1s: evex.nds.128.66.0f3a.w0 27 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VINSERTF32X4-ymmreg-mask-z.ymmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VINSERTF32X4"
:operands "ymmreg|mask|z,ymmreg,xmmrm128,imm8"
:code-string "[rvmi:t4: evex.nds.256.66.0f3a.w0 18 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VINSERTF32X4-zmmreg-mask-z.zmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VINSERTF32X4"
:operands "zmmreg|mask|z,zmmreg,xmmrm128,imm8"
:code-string "[rvmi:t4: evex.nds.512.66.0f3a.w0 18 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VINSERTF32X8-zmmreg-mask-z.zmmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VINSERTF32X8"
:operands "zmmreg|mask|z,zmmreg,ymmrm256,imm8"
:code-string "[rvmi:t8: evex.nds.512.66.0f3a.w0 1a /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VINSERTF64X2-ymmreg-mask-z.ymmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VINSERTF64X2"
:operands "ymmreg|mask|z,ymmreg,xmmrm128,imm8"
:code-string "[rvmi:t2: evex.nds.256.66.0f3a.w1 18 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VINSERTF64X2-zmmreg-mask-z.zmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VINSERTF64X2"
:operands "zmmreg|mask|z,zmmreg,xmmrm128,imm8"
:code-string "[rvmi:t2: evex.nds.512.66.0f3a.w1 18 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VINSERTF64X4-zmmreg-mask-z.zmmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VINSERTF64X4"
:operands "zmmreg|mask|z,zmmreg,ymmrm256,imm8"
:code-string "[rvmi:t4: evex.nds.512.66.0f3a.w1 1a /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VINSERTI32X4-ymmreg-mask-z.ymmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VINSERTI32X4"
:operands "ymmreg|mask|z,ymmreg,xmmrm128,imm8"
:code-string "[rvmi:t4: evex.nds.256.66.0f3a.w0 38 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VINSERTI32X4-zmmreg-mask-z.zmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VINSERTI32X4"
:operands "zmmreg|mask|z,zmmreg,xmmrm128,imm8"
:code-string "[rvmi:t4: evex.nds.512.66.0f3a.w0 38 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VINSERTI32X8-zmmreg-mask-z.zmmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VINSERTI32X8"
:operands "zmmreg|mask|z,zmmreg,ymmrm256,imm8"
:code-string "[rvmi:t8: evex.nds.512.66.0f3a.w0 3a /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VINSERTI64X2-ymmreg-mask-z.ymmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VINSERTI64X2"
:operands "ymmreg|mask|z,ymmreg,xmmrm128,imm8"
:code-string "[rvmi:t2: evex.nds.256.66.0f3a.w1 38 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VINSERTI64X2-zmmreg-mask-z.zmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VINSERTI64X2"
:operands "zmmreg|mask|z,zmmreg,xmmrm128,imm8"
:code-string "[rvmi:t2: evex.nds.512.66.0f3a.w1 38 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VINSERTI64X4-zmmreg-mask-z.zmmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VINSERTI64X4"
:operands "zmmreg|mask|z,zmmreg,ymmrm256,imm8"
:code-string "[rvmi:t4: evex.nds.512.66.0f3a.w1 3a /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VINSERTPS-xmmreg.xmmreg.xmmrm32.imm8 (make-instance 'x86-asm-instruction
:name "VINSERTPS"
:operands "xmmreg,xmmreg,xmmrm32,imm8"
:code-string "[rvmi:t1s: evex.nds.128.66.0f3a.w0 21 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMAXPD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VMAXPD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 5f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMAXPD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VMAXPD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 5f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMAXPD-zmmreg-mask-z.zmmreg.zmmrm512-b64-sae (make-instance 'x86-asm-instruction
:name "VMAXPD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|sae"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 5f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMAXPS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VMAXPS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.0f.w0 5f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMAXPS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VMAXPS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.0f.w0 5f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMAXPS-zmmreg-mask-z.zmmreg.zmmrm512-b32-sae (make-instance 'x86-asm-instruction
:name "VMAXPS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|sae"
:code-string "[rvm:fv: evex.nds.512.0f.w0 5f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMAXSD-xmmreg-mask-z.xmmreg.xmmrm64-sae (make-instance 'x86-asm-instruction
:name "VMAXSD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|sae"
:code-string "[rvm:t1s: evex.nds.128.f2.0f.w1 5f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMAXSS-xmmreg-mask-z.xmmreg.xmmrm32-sae (make-instance 'x86-asm-instruction
:name "VMAXSS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|sae"
:code-string "[rvm:t1s: evex.nds.128.f3.0f.w0 5f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMINPD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VMINPD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 5d /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMINPD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VMINPD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 5d /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMINPD-zmmreg-mask-z.zmmreg.zmmrm512-b64-sae (make-instance 'x86-asm-instruction
:name "VMINPD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|sae"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 5d /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMINPS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VMINPS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.0f.w0 5d /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMINPS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VMINPS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.0f.w0 5d /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMINPS-zmmreg-mask-z.zmmreg.zmmrm512-b32-sae (make-instance 'x86-asm-instruction
:name "VMINPS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|sae"
:code-string "[rvm:fv: evex.nds.512.0f.w0 5d /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMINSD-xmmreg-mask-z.xmmreg.xmmrm64-sae (make-instance 'x86-asm-instruction
:name "VMINSD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|sae"
:code-string "[rvm:t1s: evex.nds.128.f2.0f.w1 5d /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMINSS-xmmreg-mask-z.xmmreg.xmmrm32-sae (make-instance 'x86-asm-instruction
:name "VMINSS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|sae"
:code-string "[rvm:t1s: evex.nds.128.f3.0f.w0 5d /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVAPD-xmmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVAPD"
:operands "xmmreg|mask|z,xmmrm128"
:code-string "[rm:fvm: evex.128.66.0f.w1 28 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVAPD-ymmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVAPD"
:operands "ymmreg|mask|z,ymmrm256"
:code-string "[rm:fvm: evex.256.66.0f.w1 28 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVAPD-zmmreg-mask-z.zmmrm512 (make-instance 'x86-asm-instruction
:name "VMOVAPD"
:operands "zmmreg|mask|z,zmmrm512"
:code-string "[rm:fvm: evex.512.66.0f.w1 28 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVAPD-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVAPD"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.66.0f.w1 29 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVAPD-ymmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVAPD"
:operands "ymmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.66.0f.w1 29 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVAPD-zmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVAPD"
:operands "zmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.66.0f.w1 29 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVAPD-mem128-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVAPD"
:operands "mem128|mask,xmmreg"
:code-string "[mr:fvm: evex.128.66.0f.w1 29 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVAPD-mem256-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVAPD"
:operands "mem256|mask,ymmreg"
:code-string "[mr:fvm: evex.256.66.0f.w1 29 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVAPD-mem512-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVAPD"
:operands "mem512|mask,zmmreg"
:code-string "[mr:fvm: evex.512.66.0f.w1 29 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVAPS-xmmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVAPS"
:operands "xmmreg|mask|z,xmmrm128"
:code-string "[rm:fvm: evex.128.0f.w0 28 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVAPS-ymmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVAPS"
:operands "ymmreg|mask|z,ymmrm256"
:code-string "[rm:fvm: evex.256.0f.w0 28 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVAPS-zmmreg-mask-z.zmmrm512 (make-instance 'x86-asm-instruction
:name "VMOVAPS"
:operands "zmmreg|mask|z,zmmrm512"
:code-string "[rm:fvm: evex.512.0f.w0 28 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVAPS-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVAPS"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.0f.w0 29 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVAPS-ymmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVAPS"
:operands "ymmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.0f.w0 29 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVAPS-zmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVAPS"
:operands "zmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.0f.w0 29 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVAPS-mem128-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVAPS"
:operands "mem128|mask,xmmreg"
:code-string "[mr:fvm: evex.128.0f.w0 29 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVAPS-mem256-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVAPS"
:operands "mem256|mask,ymmreg"
:code-string "[mr:fvm: evex.256.0f.w0 29 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVAPS-mem512-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVAPS"
:operands "mem512|mask,zmmreg"
:code-string "[mr:fvm: evex.512.0f.w0 29 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVD-xmmreg.rm32 (make-instance 'x86-asm-instruction
:name "VMOVD"
:operands "xmmreg,rm32"
:code-string "[rm:t1s: evex.128.66.0f.w0 6e /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVD-rm32.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVD"
:operands "rm32,xmmreg"
:code-string "[mr:t1s: evex.128.66.0f.w0 7e /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVDDUP-xmmreg-mask-z.xmmrm64 (make-instance 'x86-asm-instruction
:name "VMOVDDUP"
:operands "xmmreg|mask|z,xmmrm64"
:code-string "[rm:dup: evex.128.f2.0f.w1 12 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDDUP-ymmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVDDUP"
:operands "ymmreg|mask|z,ymmrm256"
:code-string "[rm:dup: evex.256.f2.0f.w1 12 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDDUP-zmmreg-mask-z.zmmrm512 (make-instance 'x86-asm-instruction
:name "VMOVDDUP"
:operands "zmmreg|mask|z,zmmrm512"
:code-string "[rm:dup: evex.512.f2.0f.w1 12 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVDQA32-xmmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVDQA32"
:operands "xmmreg|mask|z,xmmrm128"
:code-string "[rm:fvm: evex.128.66.0f.w0 6f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQA32-ymmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVDQA32"
:operands "ymmreg|mask|z,ymmrm256"
:code-string "[rm:fvm: evex.256.66.0f.w0 6f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQA32-zmmreg-mask-z.zmmrm512 (make-instance 'x86-asm-instruction
:name "VMOVDQA32"
:operands "zmmreg|mask|z,zmmrm512"
:code-string "[rm:fvm: evex.512.66.0f.w0 6f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVDQA32-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQA32"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.66.0f.w0 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQA32-ymmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVDQA32"
:operands "ymmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.66.0f.w0 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQA32-zmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQA32"
:operands "zmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.66.0f.w0 7f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVDQA32-mem128-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQA32"
:operands "mem128|mask,xmmreg"
:code-string "[mr:fvm: evex.128.66.0f.w0 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQA32-mem256-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVDQA32"
:operands "mem256|mask,ymmreg"
:code-string "[mr:fvm: evex.256.66.0f.w0 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQA32-mem512-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQA32"
:operands "mem512|mask,zmmreg"
:code-string "[mr:fvm: evex.512.66.0f.w0 7f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVDQA64-xmmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVDQA64"
:operands "xmmreg|mask|z,xmmrm128"
:code-string "[rm:fvm: evex.128.66.0f.w1 6f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQA64-ymmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVDQA64"
:operands "ymmreg|mask|z,ymmrm256"
:code-string "[rm:fvm: evex.256.66.0f.w1 6f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQA64-zmmreg-mask-z.zmmrm512 (make-instance 'x86-asm-instruction
:name "VMOVDQA64"
:operands "zmmreg|mask|z,zmmrm512"
:code-string "[rm:fvm: evex.512.66.0f.w1 6f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVDQA64-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQA64"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.66.0f.w1 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQA64-ymmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVDQA64"
:operands "ymmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.66.0f.w1 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQA64-zmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQA64"
:operands "zmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.66.0f.w1 7f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVDQA64-mem128-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQA64"
:operands "mem128|mask,xmmreg"
:code-string "[mr:fvm: evex.128.66.0f.w1 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQA64-mem256-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVDQA64"
:operands "mem256|mask,ymmreg"
:code-string "[mr:fvm: evex.256.66.0f.w1 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQA64-mem512-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQA64"
:operands "mem512|mask,zmmreg"
:code-string "[mr:fvm: evex.512.66.0f.w1 7f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVDQU16-xmmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVDQU16"
:operands "xmmreg|mask|z,xmmrm128"
:code-string "[rm:fvm: evex.128.f2.0f.w1 6f /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VMOVDQU16-ymmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVDQU16"
:operands "ymmreg|mask|z,ymmrm256"
:code-string "[rm:fvm: evex.256.f2.0f.w1 6f /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VMOVDQU16-zmmreg-mask-z.zmmrm512 (make-instance 'x86-asm-instruction
:name "VMOVDQU16"
:operands "zmmreg|mask|z,zmmrm512"
:code-string "[rm:fvm: evex.512.f2.0f.w1 6f /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VMOVDQU16-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU16"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f2.0f.w1 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VMOVDQU16-ymmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU16"
:operands "ymmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f2.0f.w1 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VMOVDQU16-zmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU16"
:operands "zmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f2.0f.w1 7f /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VMOVDQU16-mem128-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU16"
:operands "mem128|mask,xmmreg"
:code-string "[mr:fvm: evex.128.f2.0f.w1 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VMOVDQU16-mem256-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU16"
:operands "mem256|mask,ymmreg"
:code-string "[mr:fvm: evex.256.f2.0f.w1 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VMOVDQU16-mem512-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU16"
:operands "mem512|mask,zmmreg"
:code-string "[mr:fvm: evex.512.f2.0f.w1 7f /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VMOVDQU32-xmmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVDQU32"
:operands "xmmreg|mask|z,xmmrm128"
:code-string "[rm:fvm: evex.128.f3.0f.w0 6f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQU32-ymmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVDQU32"
:operands "ymmreg|mask|z,ymmrm256"
:code-string "[rm:fvm: evex.256.f3.0f.w0 6f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQU32-zmmreg-mask-z.zmmrm512 (make-instance 'x86-asm-instruction
:name "VMOVDQU32"
:operands "zmmreg|mask|z,zmmrm512"
:code-string "[rm:fvm: evex.512.f3.0f.w0 6f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVDQU32-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU32"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f.w0 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQU32-ymmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU32"
:operands "ymmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f.w0 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQU32-zmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU32"
:operands "zmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f.w0 7f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVDQU32-mem128-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU32"
:operands "mem128|mask,xmmreg"
:code-string "[mr:fvm: evex.128.f3.0f.w0 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQU32-mem256-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU32"
:operands "mem256|mask,ymmreg"
:code-string "[mr:fvm: evex.256.f3.0f.w0 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQU32-mem512-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU32"
:operands "mem512|mask,zmmreg"
:code-string "[mr:fvm: evex.512.f3.0f.w0 7f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVDQU64-xmmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVDQU64"
:operands "xmmreg|mask|z,xmmrm128"
:code-string "[rm:fvm: evex.128.f3.0f.w1 6f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQU64-ymmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVDQU64"
:operands "ymmreg|mask|z,ymmrm256"
:code-string "[rm:fvm: evex.256.f3.0f.w1 6f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQU64-zmmreg-mask-z.zmmrm512 (make-instance 'x86-asm-instruction
:name "VMOVDQU64"
:operands "zmmreg|mask|z,zmmrm512"
:code-string "[rm:fvm: evex.512.f3.0f.w1 6f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVDQU64-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU64"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f.w1 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQU64-ymmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU64"
:operands "ymmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f.w1 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQU64-zmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU64"
:operands "zmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f.w1 7f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVDQU64-mem128-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU64"
:operands "mem128|mask,xmmreg"
:code-string "[mr:fvm: evex.128.f3.0f.w1 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQU64-mem256-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU64"
:operands "mem256|mask,ymmreg"
:code-string "[mr:fvm: evex.256.f3.0f.w1 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVDQU64-mem512-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU64"
:operands "mem512|mask,zmmreg"
:code-string "[mr:fvm: evex.512.f3.0f.w1 7f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVDQU8-xmmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVDQU8"
:operands "xmmreg|mask|z,xmmrm128"
:code-string "[rm:fvm: evex.128.f2.0f.w0 6f /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VMOVDQU8-ymmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVDQU8"
:operands "ymmreg|mask|z,ymmrm256"
:code-string "[rm:fvm: evex.256.f2.0f.w0 6f /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VMOVDQU8-zmmreg-mask-z.zmmrm512 (make-instance 'x86-asm-instruction
:name "VMOVDQU8"
:operands "zmmreg|mask|z,zmmrm512"
:code-string "[rm:fvm: evex.512.f2.0f.w0 6f /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VMOVDQU8-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU8"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f2.0f.w0 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VMOVDQU8-ymmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU8"
:operands "ymmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f2.0f.w0 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VMOVDQU8-zmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU8"
:operands "zmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f2.0f.w0 7f /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VMOVDQU8-mem128-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU8"
:operands "mem128|mask,xmmreg"
:code-string "[mr:fvm: evex.128.f2.0f.w0 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VMOVDQU8-mem256-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU8"
:operands "mem256|mask,ymmreg"
:code-string "[mr:fvm: evex.256.f2.0f.w0 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VMOVDQU8-mem512-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVDQU8"
:operands "mem512|mask,zmmreg"
:code-string "[mr:fvm: evex.512.f2.0f.w0 7f /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VMOVHLPS-xmmreg.xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVHLPS"
:operands "xmmreg,xmmreg,xmmreg"
:code-string "[rvm: evex.nds.128.0f.w0 12 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVHPD-xmmreg.xmmreg.mem64 (make-instance 'x86-asm-instruction
:name "VMOVHPD"
:operands "xmmreg,xmmreg,mem64"
:code-string "[rvm:t1s: evex.nds.128.66.0f.w1 16 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVHPD-mem64.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVHPD"
:operands "mem64,xmmreg"
:code-string "[mr:t1s: evex.128.66.0f.w1 17 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVHPS-xmmreg.xmmreg.mem64 (make-instance 'x86-asm-instruction
:name "VMOVHPS"
:operands "xmmreg,xmmreg,mem64"
:code-string "[rvm:t2: evex.nds.128.0f.w0 16 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVHPS-mem64.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVHPS"
:operands "mem64,xmmreg"
:code-string "[mr:t2: evex.128.0f.w0 17 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVLHPS-xmmreg.xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVLHPS"
:operands "xmmreg,xmmreg,xmmreg"
:code-string "[rvm: evex.nds.128.0f.w0 16 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVLPD-xmmreg.xmmreg.mem64 (make-instance 'x86-asm-instruction
:name "VMOVLPD"
:operands "xmmreg,xmmreg,mem64"
:code-string "[rvm:t1s: evex.nds.128.66.0f.w1 12 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVLPD-mem64.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVLPD"
:operands "mem64,xmmreg"
:code-string "[mr:t1s: evex.128.66.0f.w1 13 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVLPS-xmmreg.xmmreg.mem64 (make-instance 'x86-asm-instruction
:name "VMOVLPS"
:operands "xmmreg,xmmreg,mem64"
:code-string "[rvm:t2: evex.nds.128.0f.w0 12 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVLPS-mem64.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVLPS"
:operands "mem64,xmmreg"
:code-string "[mr:t2: evex.128.0f.w0 13 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVNTDQ-mem128.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVNTDQ"
:operands "mem128,xmmreg"
:code-string "[mr:fvm: evex.128.66.0f.w0 e7 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVNTDQ-mem256.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVNTDQ"
:operands "mem256,ymmreg"
:code-string "[mr:fvm: evex.256.66.0f.w0 e7 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVNTDQ-mem512.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVNTDQ"
:operands "mem512,zmmreg"
:code-string "[mr:fvm: evex.512.66.0f.w0 e7 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVNTDQA-xmmreg.mem128 (make-instance 'x86-asm-instruction
:name "VMOVNTDQA"
:operands "xmmreg,mem128"
:code-string "[rm:fvm: evex.128.66.0f38.w0 2a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVNTDQA-ymmreg.mem256 (make-instance 'x86-asm-instruction
:name "VMOVNTDQA"
:operands "ymmreg,mem256"
:code-string "[rm:fvm: evex.256.66.0f38.w0 2a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVNTDQA-zmmreg.mem512 (make-instance 'x86-asm-instruction
:name "VMOVNTDQA"
:operands "zmmreg,mem512"
:code-string "[rm:fvm: evex.512.66.0f38.w0 2a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVNTPD-mem128.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVNTPD"
:operands "mem128,xmmreg"
:code-string "[mr:fvm: evex.128.66.0f.w1 2b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVNTPD-mem256.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVNTPD"
:operands "mem256,ymmreg"
:code-string "[mr:fvm: evex.256.66.0f.w1 2b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVNTPD-mem512.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVNTPD"
:operands "mem512,zmmreg"
:code-string "[mr:fvm: evex.512.66.0f.w1 2b /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVNTPS-mem128.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVNTPS"
:operands "mem128,xmmreg"
:code-string "[mr:fvm: evex.128.0f.w0 2b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVNTPS-mem256.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVNTPS"
:operands "mem256,ymmreg"
:code-string "[mr:fvm: evex.256.0f.w0 2b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVNTPS-mem512.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVNTPS"
:operands "mem512,zmmreg"
:code-string "[mr:fvm: evex.512.0f.w0 2b /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVQ-xmmreg.rm64 (make-instance 'x86-asm-instruction
:name "VMOVQ"
:operands "xmmreg,rm64"
:code-string "[rm:t1s: evex.128.66.0f.w1 6e /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVQ-rm64.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVQ"
:operands "rm64,xmmreg"
:code-string "[mr:t1s: evex.128.66.0f.w1 7e /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVQ-xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VMOVQ"
:operands "xmmreg,xmmrm64"
:code-string "[rm:t1s: evex.128.f3.0f.w1 7e /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVQ-xmmrm64.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVQ"
:operands "xmmrm64,xmmreg"
:code-string "[mr:t1s: evex.128.66.0f.w1 d6 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVSD-xmmreg-mask-z.mem64 (make-instance 'x86-asm-instruction
:name "VMOVSD"
:operands "xmmreg|mask|z,mem64"
:code-string "[rm:t1s: evex.128.f2.0f.w1 10 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVSD-mem64-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVSD"
:operands "mem64|mask,xmmreg"
:code-string "[mr:t1s: evex.128.f2.0f.w1 11 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVSD-xmmreg-mask-z.xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVSD"
:operands "xmmreg|mask|z,xmmreg,xmmreg"
:code-string "[rvm: evex.nds.128.f2.0f.w1 10 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVSD-xmmreg-mask-z.xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVSD"
:operands "xmmreg|mask|z,xmmreg,xmmreg"
:code-string "[mvr: evex.nds.128.f2.0f.w1 11 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVSHDUP-xmmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVSHDUP"
:operands "xmmreg|mask|z,xmmrm128"
:code-string "[rm:fvm: evex.128.f3.0f.w0 16 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVSHDUP-ymmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVSHDUP"
:operands "ymmreg|mask|z,ymmrm256"
:code-string "[rm:fvm: evex.256.f3.0f.w0 16 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVSHDUP-zmmreg-mask-z.zmmrm512 (make-instance 'x86-asm-instruction
:name "VMOVSHDUP"
:operands "zmmreg|mask|z,zmmrm512"
:code-string "[rm:fvm: evex.512.f3.0f.w0 16 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVSLDUP-xmmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVSLDUP"
:operands "xmmreg|mask|z,xmmrm128"
:code-string "[rm:fvm: evex.128.f3.0f.w0 12 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVSLDUP-ymmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVSLDUP"
:operands "ymmreg|mask|z,ymmrm256"
:code-string "[rm:fvm: evex.256.f3.0f.w0 12 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVSLDUP-zmmreg-mask-z.zmmrm512 (make-instance 'x86-asm-instruction
:name "VMOVSLDUP"
:operands "zmmreg|mask|z,zmmrm512"
:code-string "[rm:fvm: evex.512.f3.0f.w0 12 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVSS-xmmreg-mask-z.mem32 (make-instance 'x86-asm-instruction
:name "VMOVSS"
:operands "xmmreg|mask|z,mem32"
:code-string "[rm:t1s: evex.128.f3.0f.w0 10 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVSS-mem32-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVSS"
:operands "mem32|mask,xmmreg"
:code-string "[mr:t1s: evex.128.f3.0f.w0 11 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVSS-xmmreg-mask-z.xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVSS"
:operands "xmmreg|mask|z,xmmreg,xmmreg"
:code-string "[rvm: evex.nds.128.f3.0f.w0 10 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVSS-xmmreg-mask-z.xmmreg.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVSS"
:operands "xmmreg|mask|z,xmmreg,xmmreg"
:code-string "[mvr: evex.nds.128.f3.0f.w0 11 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVUPD-xmmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVUPD"
:operands "xmmreg|mask|z,xmmrm128"
:code-string "[rm:fvm: evex.128.66.0f.w1 10 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVUPD-ymmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVUPD"
:operands "ymmreg|mask|z,ymmrm256"
:code-string "[rm:fvm: evex.256.66.0f.w1 10 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVUPD-zmmreg-mask-z.zmmrm512 (make-instance 'x86-asm-instruction
:name "VMOVUPD"
:operands "zmmreg|mask|z,zmmrm512"
:code-string "[rm:fvm: evex.512.66.0f.w1 10 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVUPD-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVUPD"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.66.0f.w1 11 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVUPD-ymmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVUPD"
:operands "ymmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.66.0f.w1 11 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVUPD-zmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVUPD"
:operands "zmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.66.0f.w1 11 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVUPD-mem128-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVUPD"
:operands "mem128|mask,xmmreg"
:code-string "[mr:fvm: evex.128.66.0f.w1 11 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVUPD-mem256-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVUPD"
:operands "mem256|mask,ymmreg"
:code-string "[mr:fvm: evex.256.66.0f.w1 11 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVUPD-mem512-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVUPD"
:operands "mem512|mask,zmmreg"
:code-string "[mr:fvm: evex.512.66.0f.w1 11 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVUPS-xmmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VMOVUPS"
:operands "xmmreg|mask|z,xmmrm128"
:code-string "[rm:fvm: evex.128.0f.w0 10 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVUPS-ymmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VMOVUPS"
:operands "ymmreg|mask|z,ymmrm256"
:code-string "[rm:fvm: evex.256.0f.w0 10 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVUPS-zmmreg-mask-z.zmmrm512 (make-instance 'x86-asm-instruction
:name "VMOVUPS"
:operands "zmmreg|mask|z,zmmrm512"
:code-string "[rm:fvm: evex.512.0f.w0 10 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVUPS-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVUPS"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.0f.w0 11 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVUPS-ymmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVUPS"
:operands "ymmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.0f.w0 11 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVUPS-zmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVUPS"
:operands "zmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.0f.w0 11 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMOVUPS-mem128-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VMOVUPS"
:operands "mem128|mask,xmmreg"
:code-string "[mr:fvm: evex.128.0f.w0 11 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVUPS-mem256-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VMOVUPS"
:operands "mem256|mask,ymmreg"
:code-string "[mr:fvm: evex.256.0f.w0 11 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMOVUPS-mem512-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VMOVUPS"
:operands "mem512|mask,zmmreg"
:code-string "[mr:fvm: evex.512.0f.w0 11 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMULPD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VMULPD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 59 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMULPD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VMULPD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 59 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMULPD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VMULPD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 59 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMULPS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VMULPS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.0f.w0 59 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMULPS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VMULPS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.0f.w0 59 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VMULPS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VMULPS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.0f.w0 59 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMULSD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VMULSD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|er"
:code-string "[rvm:t1s: evex.nds.128.f2.0f.w1 59 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VMULSS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VMULSS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|er"
:code-string "[rvm:t1s: evex.nds.128.f3.0f.w0 59 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VORPD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VORPD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 56 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VORPD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VORPD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 56 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VORPD-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VORPD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 56 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VORPS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VORPS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.0f.w0 56 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VORPS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VORPS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.0f.w0 56 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VORPS-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VORPS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.0f.w0 56 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VPABSB-xmmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPABSB"
:operands "xmmreg|mask|z,xmmrm128"
:code-string "[rm:fvm: evex.128.66.0f38.wig 1c /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPABSB-ymmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPABSB"
:operands "ymmreg|mask|z,ymmrm256"
:code-string "[rm:fvm: evex.256.66.0f38.wig 1c /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPABSB-zmmreg-mask-z.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPABSB"
:operands "zmmreg|mask|z,zmmrm512"
:code-string "[rm:fvm: evex.512.66.0f38.wig 1c /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPABSD-xmmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPABSD"
:operands "xmmreg|mask|z,xmmrm128|b32"
:code-string "[rm:fv: evex.128.66.0f38.w0 1e /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPABSD-ymmreg-mask-z.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPABSD"
:operands "ymmreg|mask|z,ymmrm256|b32"
:code-string "[rm:fv: evex.256.66.0f38.w0 1e /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPABSD-zmmreg-mask-z.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPABSD"
:operands "zmmreg|mask|z,zmmrm512|b32"
:code-string "[rm:fv: evex.512.66.0f38.w0 1e /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPABSQ-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPABSQ"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.66.0f38.w1 1f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPABSQ-ymmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPABSQ"
:operands "ymmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.66.0f38.w1 1f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPABSQ-zmmreg-mask-z.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPABSQ"
:operands "zmmreg|mask|z,zmmrm512|b64"
:code-string "[rm:fv: evex.512.66.0f38.w1 1f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPABSW-xmmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPABSW"
:operands "xmmreg|mask|z,xmmrm128"
:code-string "[rm:fvm: evex.128.66.0f38.wig 1d /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPABSW-ymmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPABSW"
:operands "ymmreg|mask|z,ymmrm256"
:code-string "[rm:fvm: evex.256.66.0f38.wig 1d /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPABSW-zmmreg-mask-z.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPABSW"
:operands "zmmreg|mask|z,zmmrm512"
:code-string "[rm:fvm: evex.512.66.0f38.wig 1d /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPACKSSDW-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPACKSSDW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f.w0 6b /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPACKSSDW-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPACKSSDW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f.w0 6b /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPACKSSDW-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPACKSSDW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f.w0 6b /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPACKSSWB-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPACKSSWB"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig 63 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPACKSSWB-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPACKSSWB"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig 63 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPACKSSWB-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPACKSSWB"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig 63 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPACKUSDW-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPACKUSDW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 2b /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPACKUSDW-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPACKUSDW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 2b /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPACKUSDW-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPACKUSDW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 2b /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPACKUSWB-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPACKUSWB"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig 67 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPACKUSWB-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPACKUSWB"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig 67 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPACKUSWB-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPACKUSWB"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig 67 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPADDB-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPADDB"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig fc /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPADDB-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPADDB"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig fc /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPADDB-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPADDB"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig fc /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPADDD-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPADDD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f.w0 fe /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPADDD-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPADDD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f.w0 fe /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPADDD-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPADDD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f.w0 fe /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPADDQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPADDQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 d4 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPADDQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPADDQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 d4 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPADDQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPADDQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 d4 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPADDSB-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPADDSB"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig ec /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPADDSB-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPADDSB"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig ec /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPADDSB-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPADDSB"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig ec /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPADDSW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPADDSW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig ed /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPADDSW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPADDSW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig ed /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPADDSW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPADDSW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig ed /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPADDUSB-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPADDUSB"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig dc /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPADDUSB-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPADDUSB"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig dc /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPADDUSB-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPADDUSB"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig dc /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPADDUSW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPADDUSW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig dd /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPADDUSW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPADDUSW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig dd /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPADDUSW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPADDUSW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig dd /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPADDW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPADDW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig fd /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPADDW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPADDW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig fd /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPADDW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPADDW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig fd /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPALIGNR-xmmreg-mask-z.xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPALIGNR"
:operands "xmmreg|mask|z,xmmreg,xmmrm128,imm8"
:code-string "[rvmi:fvm: evex.nds.128.66.0f3a.wig 0f /r ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPALIGNR-ymmreg-mask-z.ymmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPALIGNR"
:operands "ymmreg|mask|z,ymmreg,ymmrm256,imm8"
:code-string "[rvmi:fvm: evex.nds.256.66.0f3a.wig 0f /r ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPALIGNR-zmmreg-mask-z.zmmreg.zmmrm512.imm8 (make-instance 'x86-asm-instruction
:name "VPALIGNR"
:operands "zmmreg|mask|z,zmmreg,zmmrm512,imm8"
:code-string "[rvmi:fvm: evex.nds.512.66.0f3a.wig 0f /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPANDD-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPANDD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f.w0 db /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPANDD-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPANDD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f.w0 db /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPANDD-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPANDD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f.w0 db /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPANDND-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPANDND"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f.w0 df /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPANDND-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPANDND"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f.w0 df /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPANDND-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPANDND"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f.w0 df /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPANDNQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPANDNQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 df /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPANDNQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPANDNQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 df /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPANDNQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPANDNQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 df /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPANDQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPANDQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 db /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPANDQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPANDQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 db /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPANDQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPANDQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 db /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPAVGB-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPAVGB"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig e0 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPAVGB-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPAVGB"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig e0 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPAVGB-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPAVGB"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig e0 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPAVGW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPAVGW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig e3 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPAVGW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPAVGW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig e3 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPAVGW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPAVGW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig e3 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPBLENDMB-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPBLENDMB"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.w0 66 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBLENDMB-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPBLENDMB"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.w0 66 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBLENDMB-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPBLENDMB"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.w0 66 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPBLENDMD-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPBLENDMD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 64 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPBLENDMD-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPBLENDMD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 64 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPBLENDMD-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPBLENDMD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 64 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPBLENDMQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPBLENDMQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 64 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPBLENDMQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPBLENDMQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 64 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPBLENDMQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPBLENDMQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 64 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPBLENDMW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPBLENDMW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.w1 66 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBLENDMW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPBLENDMW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.w1 66 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBLENDMW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPBLENDMW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.w1 66 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPBROADCASTB-xmmreg-mask-z.xmmrm8 (make-instance 'x86-asm-instruction
:name "VPBROADCASTB"
:operands "xmmreg|mask|z,xmmrm8"
:code-string "[rm:t1s: evex.128.66.0f38.w0 78 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBROADCASTB-ymmreg-mask-z.xmmrm8 (make-instance 'x86-asm-instruction
:name "VPBROADCASTB"
:operands "ymmreg|mask|z,xmmrm8"
:code-string "[rm:t1s: evex.256.66.0f38.w0 78 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBROADCASTB-zmmreg-mask-z.xmmrm8 (make-instance 'x86-asm-instruction
:name "VPBROADCASTB"
:operands "zmmreg|mask|z,xmmrm8"
:code-string "[rm:t1s: evex.512.66.0f38.w0 78 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPBROADCASTB-xmmreg-mask-z.reg8 (make-instance 'x86-asm-instruction
:name "VPBROADCASTB"
:operands "xmmreg|mask|z,reg8"
:code-string "[rm: evex.128.66.0f38.w0 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBROADCASTB-xmmreg-mask-z.reg16 (make-instance 'x86-asm-instruction
:name "VPBROADCASTB"
:operands "xmmreg|mask|z,reg16"
:code-string "[rm: evex.128.66.0f38.w0 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBROADCASTB-xmmreg-mask-z.reg32 (make-instance 'x86-asm-instruction
:name "VPBROADCASTB"
:operands "xmmreg|mask|z,reg32"
:code-string "[rm: evex.128.66.0f38.w0 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBROADCASTB-xmmreg-mask-z.reg64 (make-instance 'x86-asm-instruction
:name "VPBROADCASTB"
:operands "xmmreg|mask|z,reg64"
:code-string "[rm: evex.128.66.0f38.w0 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBROADCASTB-ymmreg-mask-z.reg8 (make-instance 'x86-asm-instruction
:name "VPBROADCASTB"
:operands "ymmreg|mask|z,reg8"
:code-string "[rm: evex.256.66.0f38.w0 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBROADCASTB-ymmreg-mask-z.reg16 (make-instance 'x86-asm-instruction
:name "VPBROADCASTB"
:operands "ymmreg|mask|z,reg16"
:code-string "[rm: evex.256.66.0f38.w0 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBROADCASTB-ymmreg-mask-z.reg32 (make-instance 'x86-asm-instruction
:name "VPBROADCASTB"
:operands "ymmreg|mask|z,reg32"
:code-string "[rm: evex.256.66.0f38.w0 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBROADCASTB-ymmreg-mask-z.reg64 (make-instance 'x86-asm-instruction
:name "VPBROADCASTB"
:operands "ymmreg|mask|z,reg64"
:code-string "[rm: evex.256.66.0f38.w0 7a /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBROADCASTB-zmmreg-mask-z.reg8 (make-instance 'x86-asm-instruction
:name "VPBROADCASTB"
:operands "zmmreg|mask|z,reg8"
:code-string "[rm: evex.512.66.0f38.w0 7a /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPBROADCASTB-zmmreg-mask-z.reg16 (make-instance 'x86-asm-instruction
:name "VPBROADCASTB"
:operands "zmmreg|mask|z,reg16"
:code-string "[rm: evex.512.66.0f38.w0 7a /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPBROADCASTB-zmmreg-mask-z.reg32 (make-instance 'x86-asm-instruction
:name "VPBROADCASTB"
:operands "zmmreg|mask|z,reg32"
:code-string "[rm: evex.512.66.0f38.w0 7a /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPBROADCASTB-zmmreg-mask-z.reg64 (make-instance 'x86-asm-instruction
:name "VPBROADCASTB"
:operands "zmmreg|mask|z,reg64"
:code-string "[rm: evex.512.66.0f38.w0 7a /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPBROADCASTD-xmmreg-mask-z.mem32 (make-instance 'x86-asm-instruction
:name "VPBROADCASTD"
:operands "xmmreg|mask|z,mem32"
:code-string "[rm:t1s: evex.128.66.0f38.w0 58 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPBROADCASTD-ymmreg-mask-z.mem32 (make-instance 'x86-asm-instruction
:name "VPBROADCASTD"
:operands "ymmreg|mask|z,mem32"
:code-string "[rm:t1s: evex.256.66.0f38.w0 58 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPBROADCASTD-zmmreg-mask-z.mem32 (make-instance 'x86-asm-instruction
:name "VPBROADCASTD"
:operands "zmmreg|mask|z,mem32"
:code-string "[rm:t1s: evex.512.66.0f38.w0 58 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPBROADCASTD-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTD"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[rm: evex.128.66.0f38.w0 58 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPBROADCASTD-ymmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTD"
:operands "ymmreg|mask|z,xmmreg"
:code-string "[rm: evex.256.66.0f38.w0 58 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPBROADCASTD-zmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTD"
:operands "zmmreg|mask|z,xmmreg"
:code-string "[rm: evex.512.66.0f38.w0 58 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPBROADCASTD-xmmreg-mask-z.reg32 (make-instance 'x86-asm-instruction
:name "VPBROADCASTD"
:operands "xmmreg|mask|z,reg32"
:code-string "[rm: evex.128.66.0f38.w0 7c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPBROADCASTD-ymmreg-mask-z.reg32 (make-instance 'x86-asm-instruction
:name "VPBROADCASTD"
:operands "ymmreg|mask|z,reg32"
:code-string "[rm: evex.256.66.0f38.w0 7c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPBROADCASTD-zmmreg-mask-z.reg32 (make-instance 'x86-asm-instruction
:name "VPBROADCASTD"
:operands "zmmreg|mask|z,reg32"
:code-string "[rm: evex.512.66.0f38.w0 7c /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPBROADCASTMB2Q-xmmreg.kreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTMB2Q"
:operands "xmmreg,kreg"
:code-string "[rm: evex.128.f3.0f38.w1 2a /r ]"
:arch-flags (list "AVX512VL" "AVX512CD" "FUTURE")))

(setf VPBROADCASTMB2Q-ymmreg.kreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTMB2Q"
:operands "ymmreg,kreg"
:code-string "[rm: evex.256.f3.0f38.w1 2a /r ]"
:arch-flags (list "AVX512VL" "AVX512CD" "FUTURE")))

(setf VPBROADCASTMB2Q-zmmreg.kreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTMB2Q"
:operands "zmmreg,kreg"
:code-string "[rm: evex.512.f3.0f38.w1 2a /r ]"
:arch-flags (list "AVX512CD" "FUTURE")))

(setf VPBROADCASTMW2D-xmmreg.kreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTMW2D"
:operands "xmmreg,kreg"
:code-string "[rm: evex.128.f3.0f38.w0 3a /r ]"
:arch-flags (list "AVX512VL" "AVX512CD" "FUTURE")))

(setf VPBROADCASTMW2D-ymmreg.kreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTMW2D"
:operands "ymmreg,kreg"
:code-string "[rm: evex.256.f3.0f38.w0 3a /r ]"
:arch-flags (list "AVX512VL" "AVX512CD" "FUTURE")))

(setf VPBROADCASTMW2D-zmmreg.kreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTMW2D"
:operands "zmmreg,kreg"
:code-string "[rm: evex.512.f3.0f38.w0 3a /r ]"
:arch-flags (list "AVX512CD" "FUTURE")))

(setf VPBROADCASTQ-xmmreg-mask-z.mem64 (make-instance 'x86-asm-instruction
:name "VPBROADCASTQ"
:operands "xmmreg|mask|z,mem64"
:code-string "[rm:t1s: evex.128.66.0f38.w1 59 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPBROADCASTQ-ymmreg-mask-z.mem64 (make-instance 'x86-asm-instruction
:name "VPBROADCASTQ"
:operands "ymmreg|mask|z,mem64"
:code-string "[rm:t1s: evex.256.66.0f38.w1 59 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPBROADCASTQ-zmmreg-mask-z.mem64 (make-instance 'x86-asm-instruction
:name "VPBROADCASTQ"
:operands "zmmreg|mask|z,mem64"
:code-string "[rm:t1s: evex.512.66.0f38.w1 59 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPBROADCASTQ-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTQ"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[rm: evex.128.66.0f38.w1 59 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPBROADCASTQ-ymmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTQ"
:operands "ymmreg|mask|z,xmmreg"
:code-string "[rm: evex.256.66.0f38.w1 59 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPBROADCASTQ-zmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPBROADCASTQ"
:operands "zmmreg|mask|z,xmmreg"
:code-string "[rm: evex.512.66.0f38.w1 59 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPBROADCASTQ-xmmreg-mask-z.reg64 (make-instance 'x86-asm-instruction
:name "VPBROADCASTQ"
:operands "xmmreg|mask|z,reg64"
:code-string "[rm: evex.128.66.0f38.w1 7c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPBROADCASTQ-ymmreg-mask-z.reg64 (make-instance 'x86-asm-instruction
:name "VPBROADCASTQ"
:operands "ymmreg|mask|z,reg64"
:code-string "[rm: evex.256.66.0f38.w1 7c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPBROADCASTQ-zmmreg-mask-z.reg64 (make-instance 'x86-asm-instruction
:name "VPBROADCASTQ"
:operands "zmmreg|mask|z,reg64"
:code-string "[rm: evex.512.66.0f38.w1 7c /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPBROADCASTW-xmmreg-mask-z.xmmrm16 (make-instance 'x86-asm-instruction
:name "VPBROADCASTW"
:operands "xmmreg|mask|z,xmmrm16"
:code-string "[rm:t1s: evex.128.66.0f38.w0 79 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBROADCASTW-ymmreg-mask-z.xmmrm16 (make-instance 'x86-asm-instruction
:name "VPBROADCASTW"
:operands "ymmreg|mask|z,xmmrm16"
:code-string "[rm:t1s: evex.256.66.0f38.w0 79 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBROADCASTW-zmmreg-mask-z.xmmrm16 (make-instance 'x86-asm-instruction
:name "VPBROADCASTW"
:operands "zmmreg|mask|z,xmmrm16"
:code-string "[rm:t1s: evex.512.66.0f38.w0 79 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPBROADCASTW-xmmreg-mask-z.reg16 (make-instance 'x86-asm-instruction
:name "VPBROADCASTW"
:operands "xmmreg|mask|z,reg16"
:code-string "[rm: evex.128.66.0f38.w0 7b /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBROADCASTW-xmmreg-mask-z.reg32 (make-instance 'x86-asm-instruction
:name "VPBROADCASTW"
:operands "xmmreg|mask|z,reg32"
:code-string "[rm: evex.128.66.0f38.w0 7b /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBROADCASTW-xmmreg-mask-z.reg64 (make-instance 'x86-asm-instruction
:name "VPBROADCASTW"
:operands "xmmreg|mask|z,reg64"
:code-string "[rm: evex.128.66.0f38.w0 7b /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBROADCASTW-ymmreg-mask-z.reg16 (make-instance 'x86-asm-instruction
:name "VPBROADCASTW"
:operands "ymmreg|mask|z,reg16"
:code-string "[rm: evex.256.66.0f38.w0 7b /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBROADCASTW-ymmreg-mask-z.reg32 (make-instance 'x86-asm-instruction
:name "VPBROADCASTW"
:operands "ymmreg|mask|z,reg32"
:code-string "[rm: evex.256.66.0f38.w0 7b /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBROADCASTW-ymmreg-mask-z.reg64 (make-instance 'x86-asm-instruction
:name "VPBROADCASTW"
:operands "ymmreg|mask|z,reg64"
:code-string "[rm: evex.256.66.0f38.w0 7b /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPBROADCASTW-zmmreg-mask-z.reg16 (make-instance 'x86-asm-instruction
:name "VPBROADCASTW"
:operands "zmmreg|mask|z,reg16"
:code-string "[rm: evex.512.66.0f38.w0 7b /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPBROADCASTW-zmmreg-mask-z.reg32 (make-instance 'x86-asm-instruction
:name "VPBROADCASTW"
:operands "zmmreg|mask|z,reg32"
:code-string "[rm: evex.512.66.0f38.w0 7b /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPBROADCASTW-zmmreg-mask-z.reg64 (make-instance 'x86-asm-instruction
:name "VPBROADCASTW"
:operands "zmmreg|mask|z,reg64"
:code-string "[rm: evex.512.66.0f38.w0 7b /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPCMPB-kreg-mask.xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPB"
:operands "kreg|mask,xmmreg,xmmrm128,imm8"
:code-string "[rvmi:fvm: evex.nds.128.66.0f3a.w0 3f /r ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPCMPB-kreg-mask.ymmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPB"
:operands "kreg|mask,ymmreg,ymmrm256,imm8"
:code-string "[rvmi:fvm: evex.nds.256.66.0f3a.w0 3f /r ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPCMPB-kreg-mask.zmmreg.zmmrm512.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPB"
:operands "kreg|mask,zmmreg,zmmrm512,imm8"
:code-string "[rvmi:fvm: evex.nds.512.66.0f3a.w0 3f /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPCMPD-kreg-mask.xmmreg.xmmrm128-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPD"
:operands "kreg|mask,xmmreg,xmmrm128|b32,imm8"
:code-string "[rvmi:fv: evex.nds.128.66.0f3a.w0 1f /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCMPD-kreg-mask.ymmreg.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPD"
:operands "kreg|mask,ymmreg,ymmrm256|b32,imm8"
:code-string "[rvmi:fv: evex.nds.256.66.0f3a.w0 1f /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCMPD-kreg-mask.zmmreg.zmmrm512-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPD"
:operands "kreg|mask,zmmreg,zmmrm512|b32,imm8"
:code-string "[rvmi:fv: evex.nds.512.66.0f3a.w0 1f /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPCMPEQB-kreg-mask.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPCMPEQB"
:operands "kreg|mask,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig 74 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPCMPEQB-kreg-mask.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPCMPEQB"
:operands "kreg|mask,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig 74 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPCMPEQB-kreg-mask.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPCMPEQB"
:operands "kreg|mask,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig 74 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPCMPEQD-kreg-mask.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPCMPEQD"
:operands "kreg|mask,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f.w0 76 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCMPEQD-kreg-mask.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPCMPEQD"
:operands "kreg|mask,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f.w0 76 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCMPEQD-kreg-mask.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPCMPEQD"
:operands "kreg|mask,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f.w0 76 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPCMPEQQ-kreg-mask.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPCMPEQQ"
:operands "kreg|mask,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 29 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCMPEQQ-kreg-mask.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPCMPEQQ"
:operands "kreg|mask,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 29 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCMPEQQ-kreg-mask.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPCMPEQQ"
:operands "kreg|mask,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 29 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPCMPEQW-kreg-mask.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPCMPEQW"
:operands "kreg|mask,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig 75 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPCMPEQW-kreg-mask.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPCMPEQW"
:operands "kreg|mask,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig 75 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPCMPEQW-kreg-mask.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPCMPEQW"
:operands "kreg|mask,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig 75 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPCMPGTB-kreg-mask.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPCMPGTB"
:operands "kreg|mask,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig 64 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPCMPGTB-kreg-mask.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPCMPGTB"
:operands "kreg|mask,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig 64 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPCMPGTB-kreg-mask.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPCMPGTB"
:operands "kreg|mask,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig 64 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPCMPGTD-kreg-mask.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPCMPGTD"
:operands "kreg|mask,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f.w0 66 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCMPGTD-kreg-mask.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPCMPGTD"
:operands "kreg|mask,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f.w0 66 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCMPGTD-kreg-mask.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPCMPGTD"
:operands "kreg|mask,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f.w0 66 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPCMPGTQ-kreg-mask.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPCMPGTQ"
:operands "kreg|mask,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 37 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCMPGTQ-kreg-mask.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPCMPGTQ"
:operands "kreg|mask,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 37 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCMPGTQ-kreg-mask.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPCMPGTQ"
:operands "kreg|mask,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 37 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPCMPGTW-kreg-mask.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPCMPGTW"
:operands "kreg|mask,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig 65 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPCMPGTW-kreg-mask.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPCMPGTW"
:operands "kreg|mask,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig 65 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPCMPGTW-kreg-mask.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPCMPGTW"
:operands "kreg|mask,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig 65 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPCMPQ-kreg-mask.xmmreg.xmmrm128-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPQ"
:operands "kreg|mask,xmmreg,xmmrm128|b64,imm8"
:code-string "[rvmi:fv: evex.nds.128.66.0f3a.w1 1f /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCMPQ-kreg-mask.ymmreg.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPQ"
:operands "kreg|mask,ymmreg,ymmrm256|b64,imm8"
:code-string "[rvmi:fv: evex.nds.256.66.0f3a.w1 1f /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCMPQ-kreg-mask.zmmreg.zmmrm512-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPQ"
:operands "kreg|mask,zmmreg,zmmrm512|b64,imm8"
:code-string "[rvmi:fv: evex.nds.512.66.0f3a.w1 1f /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPCMPUB-kreg-mask.xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPUB"
:operands "kreg|mask,xmmreg,xmmrm128,imm8"
:code-string "[rvmi:fvm: evex.nds.128.66.0f3a.w0 3e /r ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPCMPUB-kreg-mask.ymmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPUB"
:operands "kreg|mask,ymmreg,ymmrm256,imm8"
:code-string "[rvmi:fvm: evex.nds.256.66.0f3a.w0 3e /r ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPCMPUB-kreg-mask.zmmreg.zmmrm512.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPUB"
:operands "kreg|mask,zmmreg,zmmrm512,imm8"
:code-string "[rvmi:fvm: evex.nds.512.66.0f3a.w0 3e /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPCMPUD-kreg-mask.xmmreg.xmmrm128-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPUD"
:operands "kreg|mask,xmmreg,xmmrm128|b32,imm8"
:code-string "[rvmi:fv: evex.nds.128.66.0f3a.w0 1e /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCMPUD-kreg-mask.ymmreg.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPUD"
:operands "kreg|mask,ymmreg,ymmrm256|b32,imm8"
:code-string "[rvmi:fv: evex.nds.256.66.0f3a.w0 1e /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCMPUD-kreg-mask.zmmreg.zmmrm512-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPUD"
:operands "kreg|mask,zmmreg,zmmrm512|b32,imm8"
:code-string "[rvmi:fv: evex.nds.512.66.0f3a.w0 1e /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPCMPUQ-kreg-mask.xmmreg.xmmrm128-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPUQ"
:operands "kreg|mask,xmmreg,xmmrm128|b64,imm8"
:code-string "[rvmi:fv: evex.nds.128.66.0f3a.w1 1e /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCMPUQ-kreg-mask.ymmreg.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPUQ"
:operands "kreg|mask,ymmreg,ymmrm256|b64,imm8"
:code-string "[rvmi:fv: evex.nds.256.66.0f3a.w1 1e /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCMPUQ-kreg-mask.zmmreg.zmmrm512-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPUQ"
:operands "kreg|mask,zmmreg,zmmrm512|b64,imm8"
:code-string "[rvmi:fv: evex.nds.512.66.0f3a.w1 1e /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPCMPUW-kreg-mask.xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPUW"
:operands "kreg|mask,xmmreg,xmmrm128,imm8"
:code-string "[rvmi:fvm: evex.nds.128.66.0f3a.w1 3e /r ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPCMPUW-kreg-mask.ymmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPUW"
:operands "kreg|mask,ymmreg,ymmrm256,imm8"
:code-string "[rvmi:fvm: evex.nds.256.66.0f3a.w1 3e /r ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPCMPUW-kreg-mask.zmmreg.zmmrm512.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPUW"
:operands "kreg|mask,zmmreg,zmmrm512,imm8"
:code-string "[rvmi:fvm: evex.nds.512.66.0f3a.w1 3e /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPCMPW-kreg-mask.xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPW"
:operands "kreg|mask,xmmreg,xmmrm128,imm8"
:code-string "[rvmi:fvm: evex.nds.128.66.0f3a.w1 3f /r ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPCMPW-kreg-mask.ymmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPW"
:operands "kreg|mask,ymmreg,ymmrm256,imm8"
:code-string "[rvmi:fvm: evex.nds.256.66.0f3a.w1 3f /r ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPCMPW-kreg-mask.zmmreg.zmmrm512.imm8 (make-instance 'x86-asm-instruction
:name "VPCMPW"
:operands "kreg|mask,zmmreg,zmmrm512,imm8"
:code-string "[rvmi:fvm: evex.nds.512.66.0f3a.w1 3f /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPCOMPRESSD-mem128-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPCOMPRESSD"
:operands "mem128|mask,xmmreg"
:code-string "[mr:t1s: evex.128.66.0f38.w0 8b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCOMPRESSD-mem256-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPCOMPRESSD"
:operands "mem256|mask,ymmreg"
:code-string "[mr:t1s: evex.256.66.0f38.w0 8b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCOMPRESSD-mem512-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPCOMPRESSD"
:operands "mem512|mask,zmmreg"
:code-string "[mr:t1s: evex.512.66.0f38.w0 8b /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPCOMPRESSD-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPCOMPRESSD"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.66.0f38.w0 8b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCOMPRESSD-ymmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPCOMPRESSD"
:operands "ymmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.66.0f38.w0 8b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCOMPRESSD-zmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPCOMPRESSD"
:operands "zmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.66.0f38.w0 8b /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPCOMPRESSQ-mem128-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPCOMPRESSQ"
:operands "mem128|mask,xmmreg"
:code-string "[mr:t1s: evex.128.66.0f38.w1 8b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCOMPRESSQ-mem256-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPCOMPRESSQ"
:operands "mem256|mask,ymmreg"
:code-string "[mr:t1s: evex.256.66.0f38.w1 8b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCOMPRESSQ-mem512-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPCOMPRESSQ"
:operands "mem512|mask,zmmreg"
:code-string "[mr:t1s: evex.512.66.0f38.w1 8b /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPCOMPRESSQ-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPCOMPRESSQ"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.66.0f38.w1 8b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCOMPRESSQ-ymmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPCOMPRESSQ"
:operands "ymmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.66.0f38.w1 8b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPCOMPRESSQ-zmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPCOMPRESSQ"
:operands "zmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.66.0f38.w1 8b /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPCONFLICTD-xmmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPCONFLICTD"
:operands "xmmreg|mask|z,xmmrm128|b32"
:code-string "[rm:fv: evex.128.66.0f38.w0 c4 /r ]"
:arch-flags (list "AVX512VL" "AVX512CD" "FUTURE")))

(setf VPCONFLICTD-ymmreg-mask-z.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPCONFLICTD"
:operands "ymmreg|mask|z,ymmrm256|b32"
:code-string "[rm:fv: evex.256.66.0f38.w0 c4 /r ]"
:arch-flags (list "AVX512VL" "AVX512CD" "FUTURE")))

(setf VPCONFLICTD-zmmreg-mask-z.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPCONFLICTD"
:operands "zmmreg|mask|z,zmmrm512|b32"
:code-string "[rm:fv: evex.512.66.0f38.w0 c4 /r ]"
:arch-flags (list "AVX512CD" "FUTURE")))

(setf VPCONFLICTQ-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPCONFLICTQ"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.66.0f38.w1 c4 /r ]"
:arch-flags (list "AVX512VL" "AVX512CD" "FUTURE")))

(setf VPCONFLICTQ-ymmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPCONFLICTQ"
:operands "ymmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.66.0f38.w1 c4 /r ]"
:arch-flags (list "AVX512VL" "AVX512CD" "FUTURE")))

(setf VPCONFLICTQ-zmmreg-mask-z.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPCONFLICTQ"
:operands "zmmreg|mask|z,zmmrm512|b64"
:code-string "[rm:fv: evex.512.66.0f38.w1 c4 /r ]"
:arch-flags (list "AVX512CD" "FUTURE")))

(setf VPERMB-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPERMB"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.w0 8d /r ]"
:arch-flags (list "AVX512VL" "AVX512VBMI" "FUTURE")))

(setf VPERMB-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPERMB"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.w0 8d /r ]"
:arch-flags (list "AVX512VL" "AVX512VBMI" "FUTURE")))

(setf VPERMB-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPERMB"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.w0 8d /r ]"
:arch-flags (list "AVX512VBMI" "FUTURE")))

(setf VPERMD-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPERMD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 36 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMD-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPERMD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 36 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPERMI2B-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPERMI2B"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.w0 75 /r ]"
:arch-flags (list "AVX512VL" "AVX512VBMI" "FUTURE")))

(setf VPERMI2B-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPERMI2B"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.w0 75 /r ]"
:arch-flags (list "AVX512VL" "AVX512VBMI" "FUTURE")))

(setf VPERMI2B-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPERMI2B"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.w0 75 /r ]"
:arch-flags (list "AVX512VBMI" "FUTURE")))

(setf VPERMI2D-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPERMI2D"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 76 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMI2D-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPERMI2D"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 76 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMI2D-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPERMI2D"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 76 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPERMI2PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPERMI2PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 77 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMI2PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPERMI2PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 77 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMI2PD-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPERMI2PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 77 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPERMI2PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPERMI2PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 77 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMI2PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPERMI2PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 77 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMI2PS-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPERMI2PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 77 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPERMI2Q-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPERMI2Q"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 76 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMI2Q-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPERMI2Q"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 76 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMI2Q-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPERMI2Q"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 76 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPERMI2W-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPERMI2W"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.w1 75 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPERMI2W-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPERMI2W"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.w1 75 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPERMI2W-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPERMI2W"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.w1 75 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPERMILPD-xmmreg-mask-z.xmmrm128-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPERMILPD"
:operands "xmmreg|mask|z,xmmrm128|b64,imm8"
:code-string "[rmi:fv: evex.128.66.0f3a.w1 05 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMILPD-ymmreg-mask-z.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPERMILPD"
:operands "ymmreg|mask|z,ymmrm256|b64,imm8"
:code-string "[rmi:fv: evex.256.66.0f3a.w1 05 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMILPD-zmmreg-mask-z.zmmrm512-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPERMILPD"
:operands "zmmreg|mask|z,zmmrm512|b64,imm8"
:code-string "[rmi:fv: evex.512.66.0f3a.w1 05 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPERMILPD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPERMILPD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 0d /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMILPD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPERMILPD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 0d /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMILPD-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPERMILPD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 0d /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPERMILPS-xmmreg-mask-z.xmmrm128-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPERMILPS"
:operands "xmmreg|mask|z,xmmrm128|b32,imm8"
:code-string "[rmi:fv: evex.128.66.0f3a.w0 04 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMILPS-ymmreg-mask-z.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPERMILPS"
:operands "ymmreg|mask|z,ymmrm256|b32,imm8"
:code-string "[rmi:fv: evex.256.66.0f3a.w0 04 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMILPS-zmmreg-mask-z.zmmrm512-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPERMILPS"
:operands "zmmreg|mask|z,zmmrm512|b32,imm8"
:code-string "[rmi:fv: evex.512.66.0f3a.w0 04 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPERMILPS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPERMILPS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 0c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMILPS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPERMILPS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 0c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMILPS-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPERMILPS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 0c /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPERMPD-ymmreg-mask-z.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPERMPD"
:operands "ymmreg|mask|z,ymmrm256|b64,imm8"
:code-string "[rmi:fv: evex.256.66.0f3a.w1 01 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMPD-zmmreg-mask-z.zmmrm512-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPERMPD"
:operands "zmmreg|mask|z,zmmrm512|b64,imm8"
:code-string "[rmi:fv: evex.512.66.0f3a.w1 01 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPERMPD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPERMPD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 16 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMPD-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPERMPD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 16 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPERMPS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPERMPS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 16 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMPS-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPERMPS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 16 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPERMQ-ymmreg-mask-z.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPERMQ"
:operands "ymmreg|mask|z,ymmrm256|b64,imm8"
:code-string "[rmi:fv: evex.256.66.0f3a.w1 00 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMQ-zmmreg-mask-z.zmmrm512-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPERMQ"
:operands "zmmreg|mask|z,zmmrm512|b64,imm8"
:code-string "[rmi:fv: evex.512.66.0f3a.w1 00 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPERMQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPERMQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 36 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPERMQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 36 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPERMT2B-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPERMT2B"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.w0 7d /r ]"
:arch-flags (list "AVX512VL" "AVX512VBMI" "FUTURE")))

(setf VPERMT2B-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPERMT2B"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.w0 7d /r ]"
:arch-flags (list "AVX512VL" "AVX512VBMI" "FUTURE")))

(setf VPERMT2B-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPERMT2B"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.w0 7d /r ]"
:arch-flags (list "AVX512VBMI" "FUTURE")))

(setf VPERMT2D-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPERMT2D"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 7e /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMT2D-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPERMT2D"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 7e /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMT2D-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPERMT2D"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 7e /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPERMT2PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPERMT2PD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMT2PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPERMT2PD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMT2PD-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPERMT2PD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 7f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPERMT2PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPERMT2PS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMT2PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPERMT2PS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 7f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMT2PS-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPERMT2PS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 7f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPERMT2Q-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPERMT2Q"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 7e /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMT2Q-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPERMT2Q"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 7e /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPERMT2Q-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPERMT2Q"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 7e /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPERMT2W-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPERMT2W"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.w1 7d /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPERMT2W-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPERMT2W"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.w1 7d /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPERMT2W-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPERMT2W"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.w1 7d /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPERMW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPERMW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.w1 8d /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPERMW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPERMW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.w1 8d /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPERMW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPERMW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.w1 8d /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPEXPANDD-xmmreg-mask-z.mem128 (make-instance 'x86-asm-instruction
:name "VPEXPANDD"
:operands "xmmreg|mask|z,mem128"
:code-string "[rm:t1s: evex.128.66.0f38.w0 89 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPEXPANDD-ymmreg-mask-z.mem256 (make-instance 'x86-asm-instruction
:name "VPEXPANDD"
:operands "ymmreg|mask|z,mem256"
:code-string "[rm:t1s: evex.256.66.0f38.w0 89 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPEXPANDD-zmmreg-mask-z.mem512 (make-instance 'x86-asm-instruction
:name "VPEXPANDD"
:operands "zmmreg|mask|z,mem512"
:code-string "[rm:t1s: evex.512.66.0f38.w0 89 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPEXPANDD-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPEXPANDD"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[rm:t1s: evex.128.66.0f38.w0 89 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPEXPANDD-ymmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPEXPANDD"
:operands "ymmreg|mask|z,ymmreg"
:code-string "[rm:t1s: evex.256.66.0f38.w0 89 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPEXPANDD-zmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPEXPANDD"
:operands "zmmreg|mask|z,zmmreg"
:code-string "[rm:t1s: evex.512.66.0f38.w0 89 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPEXPANDQ-xmmreg-mask-z.mem128 (make-instance 'x86-asm-instruction
:name "VPEXPANDQ"
:operands "xmmreg|mask|z,mem128"
:code-string "[rm:t1s: evex.128.66.0f38.w1 89 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPEXPANDQ-ymmreg-mask-z.mem256 (make-instance 'x86-asm-instruction
:name "VPEXPANDQ"
:operands "ymmreg|mask|z,mem256"
:code-string "[rm:t1s: evex.256.66.0f38.w1 89 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPEXPANDQ-zmmreg-mask-z.mem512 (make-instance 'x86-asm-instruction
:name "VPEXPANDQ"
:operands "zmmreg|mask|z,mem512"
:code-string "[rm:t1s: evex.512.66.0f38.w1 89 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPEXPANDQ-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPEXPANDQ"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[rm:t1s: evex.128.66.0f38.w1 89 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPEXPANDQ-ymmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPEXPANDQ"
:operands "ymmreg|mask|z,ymmreg"
:code-string "[rm:t1s: evex.256.66.0f38.w1 89 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPEXPANDQ-zmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPEXPANDQ"
:operands "zmmreg|mask|z,zmmreg"
:code-string "[rm:t1s: evex.512.66.0f38.w1 89 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPEXTRB-reg8.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRB"
:operands "reg8,xmmreg,imm8"
:code-string "[mri:t1s: evex.128.66.0f3a.wig 14 /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPEXTRB-reg16.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRB"
:operands "reg16,xmmreg,imm8"
:code-string "[mri:t1s: evex.128.66.0f3a.wig 14 /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPEXTRB-reg32.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRB"
:operands "reg32,xmmreg,imm8"
:code-string "[mri:t1s: evex.128.66.0f3a.wig 14 /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPEXTRB-reg64.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRB"
:operands "reg64,xmmreg,imm8"
:code-string "[mri:t1s: evex.128.66.0f3a.wig 14 /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPEXTRB-mem8.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRB"
:operands "mem8,xmmreg,imm8"
:code-string "[mri:t1s: evex.128.66.0f3a.wig 14 /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPEXTRD-rm32.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRD"
:operands "rm32,xmmreg,imm8"
:code-string "[mri:t1s: evex.128.66.0f3a.w0 16 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VPEXTRQ-rm64.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRQ"
:operands "rm64,xmmreg,imm8"
:code-string "[mri:t1s: evex.128.66.0f3a.w1 16 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VPEXTRW-reg16.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRW"
:operands "reg16,xmmreg,imm8"
:code-string "[mri:t1s: evex.128.66.0f3a.wig 15 /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPEXTRW-reg32.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRW"
:operands "reg32,xmmreg,imm8"
:code-string "[mri:t1s: evex.128.66.0f3a.wig 15 /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPEXTRW-reg64.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRW"
:operands "reg64,xmmreg,imm8"
:code-string "[mri:t1s: evex.128.66.0f3a.wig 15 /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPEXTRW-mem16.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRW"
:operands "mem16,xmmreg,imm8"
:code-string "[mri:t1s: evex.128.66.0f3a.wig 15 /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPEXTRW-reg16.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRW"
:operands "reg16,xmmreg,imm8"
:code-string "[rmi: evex.128.66.0f.wig c5 /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPEXTRW-reg32.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRW"
:operands "reg32,xmmreg,imm8"
:code-string "[rmi: evex.128.66.0f.wig c5 /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPEXTRW-reg64.xmmreg.imm8 (make-instance 'x86-asm-instruction
:name "VPEXTRW"
:operands "reg64,xmmreg,imm8"
:code-string "[rmi: evex.128.66.0f.wig c5 /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPGATHERDD-xmmreg-mask.xmem32 (make-instance 'x86-asm-instruction
:name "VPGATHERDD"
:operands "xmmreg|mask,xmem32"
:code-string "[rm:t1s: vsibx evex.128.66.0f38.w0 90 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPGATHERDD-ymmreg-mask.ymem32 (make-instance 'x86-asm-instruction
:name "VPGATHERDD"
:operands "ymmreg|mask,ymem32"
:code-string "[rm:t1s: vsiby evex.256.66.0f38.w0 90 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPGATHERDD-zmmreg-mask.zmem32 (make-instance 'x86-asm-instruction
:name "VPGATHERDD"
:operands "zmmreg|mask,zmem32"
:code-string "[rm:t1s: vsibz evex.512.66.0f38.w0 90 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPGATHERDQ-xmmreg-mask.xmem64 (make-instance 'x86-asm-instruction
:name "VPGATHERDQ"
:operands "xmmreg|mask,xmem64"
:code-string "[rm:t1s: vsibx evex.128.66.0f38.w1 90 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPGATHERDQ-ymmreg-mask.xmem64 (make-instance 'x86-asm-instruction
:name "VPGATHERDQ"
:operands "ymmreg|mask,xmem64"
:code-string "[rm:t1s: vsibx evex.256.66.0f38.w1 90 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPGATHERDQ-zmmreg-mask.ymem64 (make-instance 'x86-asm-instruction
:name "VPGATHERDQ"
:operands "zmmreg|mask,ymem64"
:code-string "[rm:t1s: vsiby evex.512.66.0f38.w1 90 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPGATHERQD-xmmreg-mask.xmem32 (make-instance 'x86-asm-instruction
:name "VPGATHERQD"
:operands "xmmreg|mask,xmem32"
:code-string "[rm:t1s: vsibx evex.128.66.0f38.w0 91 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPGATHERQD-xmmreg-mask.ymem32 (make-instance 'x86-asm-instruction
:name "VPGATHERQD"
:operands "xmmreg|mask,ymem32"
:code-string "[rm:t1s: vsiby evex.256.66.0f38.w0 91 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPGATHERQD-ymmreg-mask.zmem32 (make-instance 'x86-asm-instruction
:name "VPGATHERQD"
:operands "ymmreg|mask,zmem32"
:code-string "[rm:t1s: vsibz evex.512.66.0f38.w0 91 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPGATHERQQ-xmmreg-mask.xmem64 (make-instance 'x86-asm-instruction
:name "VPGATHERQQ"
:operands "xmmreg|mask,xmem64"
:code-string "[rm:t1s: vsibx evex.128.66.0f38.w1 91 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPGATHERQQ-ymmreg-mask.ymem64 (make-instance 'x86-asm-instruction
:name "VPGATHERQQ"
:operands "ymmreg|mask,ymem64"
:code-string "[rm:t1s: vsiby evex.256.66.0f38.w1 91 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPGATHERQQ-zmmreg-mask.zmem64 (make-instance 'x86-asm-instruction
:name "VPGATHERQQ"
:operands "zmmreg|mask,zmem64"
:code-string "[rm:t1s: vsibz evex.512.66.0f38.w1 91 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPINSRB-xmmreg.xmmreg.reg32.imm8 (make-instance 'x86-asm-instruction
:name "VPINSRB"
:operands "xmmreg,xmmreg,reg32,imm8"
:code-string "[rvmi:t1s: evex.nds.128.66.0f3a.wig 20 /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPINSRB-xmmreg.xmmreg.mem8.imm8 (make-instance 'x86-asm-instruction
:name "VPINSRB"
:operands "xmmreg,xmmreg,mem8,imm8"
:code-string "[rvmi:t1s: evex.nds.128.66.0f3a.wig 20 /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPINSRD-xmmreg.xmmreg.rm32.imm8 (make-instance 'x86-asm-instruction
:name "VPINSRD"
:operands "xmmreg,xmmreg,rm32,imm8"
:code-string "[rvmi:t1s: evex.nds.128.66.0f3a.w0 22 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VPINSRQ-xmmreg.xmmreg.rm64.imm8 (make-instance 'x86-asm-instruction
:name "VPINSRQ"
:operands "xmmreg,xmmreg,rm64,imm8"
:code-string "[rvmi:t1s: evex.nds.128.66.0f3a.w1 22 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VPINSRW-xmmreg.xmmreg.reg32.imm8 (make-instance 'x86-asm-instruction
:name "VPINSRW"
:operands "xmmreg,xmmreg,reg32,imm8"
:code-string "[rvmi:t1s: evex.nds.128.66.0f.wig c4 /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPINSRW-xmmreg.xmmreg.mem16.imm8 (make-instance 'x86-asm-instruction
:name "VPINSRW"
:operands "xmmreg,xmmreg,mem16,imm8"
:code-string "[rvmi:t1s: evex.nds.128.66.0f.wig c4 /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPLZCNTD-xmmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPLZCNTD"
:operands "xmmreg|mask|z,xmmrm128|b32"
:code-string "[rm:fv: evex.128.66.0f38.w0 44 /r ]"
:arch-flags (list "AVX512VL" "AVX512CD" "FUTURE")))

(setf VPLZCNTD-ymmreg-mask-z.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPLZCNTD"
:operands "ymmreg|mask|z,ymmrm256|b32"
:code-string "[rm:fv: evex.256.66.0f38.w0 44 /r ]"
:arch-flags (list "AVX512VL" "AVX512CD" "FUTURE")))

(setf VPLZCNTD-zmmreg-mask-z.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPLZCNTD"
:operands "zmmreg|mask|z,zmmrm512|b32"
:code-string "[rm:fv: evex.512.66.0f38.w0 44 /r ]"
:arch-flags (list "AVX512CD" "FUTURE")))

(setf VPLZCNTQ-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPLZCNTQ"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.66.0f38.w1 44 /r ]"
:arch-flags (list "AVX512VL" "AVX512CD" "FUTURE")))

(setf VPLZCNTQ-ymmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPLZCNTQ"
:operands "ymmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.66.0f38.w1 44 /r ]"
:arch-flags (list "AVX512VL" "AVX512CD" "FUTURE")))

(setf VPLZCNTQ-zmmreg-mask-z.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPLZCNTQ"
:operands "zmmreg|mask|z,zmmrm512|b64"
:code-string "[rm:fv: evex.512.66.0f38.w1 44 /r ]"
:arch-flags (list "AVX512CD" "FUTURE")))

(setf VPMADD52HUQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPMADD52HUQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 b5 /r ]"
:arch-flags (list "AVX512VL" "AVX512IFMA" "FUTURE")))

(setf VPMADD52HUQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPMADD52HUQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 b5 /r ]"
:arch-flags (list "AVX512VL" "AVX512IFMA" "FUTURE")))

(setf VPMADD52HUQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPMADD52HUQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 b5 /r ]"
:arch-flags (list "AVX512IFMA" "FUTURE")))

(setf VPMADD52LUQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPMADD52LUQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 b4 /r ]"
:arch-flags (list "AVX512VL" "AVX512IFMA" "FUTURE")))

(setf VPMADD52LUQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPMADD52LUQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 b4 /r ]"
:arch-flags (list "AVX512VL" "AVX512IFMA" "FUTURE")))

(setf VPMADD52LUQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPMADD52LUQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 b4 /r ]"
:arch-flags (list "AVX512IFMA" "FUTURE")))

(setf VPMADDUBSW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMADDUBSW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.wig 04 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMADDUBSW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMADDUBSW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.wig 04 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMADDUBSW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPMADDUBSW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.wig 04 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMADDWD-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMADDWD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig f5 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMADDWD-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMADDWD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig f5 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMADDWD-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPMADDWD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig f5 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMAXSB-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMAXSB"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.wig 3c /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMAXSB-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMAXSB"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.wig 3c /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMAXSB-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPMAXSB"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.wig 3c /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMAXSD-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPMAXSD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 3d /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMAXSD-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPMAXSD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 3d /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMAXSD-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPMAXSD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 3d /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMAXSQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPMAXSQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 3d /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMAXSQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPMAXSQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 3d /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMAXSQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPMAXSQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 3d /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMAXSW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMAXSW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig ee /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMAXSW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMAXSW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig ee /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMAXSW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPMAXSW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig ee /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMAXUB-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMAXUB"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig de /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMAXUB-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMAXUB"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig de /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMAXUB-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPMAXUB"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig de /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMAXUD-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPMAXUD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 3f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMAXUD-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPMAXUD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 3f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMAXUD-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPMAXUD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 3f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMAXUQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPMAXUQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 3f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMAXUQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPMAXUQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 3f /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMAXUQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPMAXUQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 3f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMAXUW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMAXUW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.wig 3e /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMAXUW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMAXUW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.wig 3e /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMAXUW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPMAXUW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.wig 3e /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMINSB-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMINSB"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.wig 38 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMINSB-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMINSB"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.wig 38 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMINSB-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPMINSB"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.wig 38 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMINSD-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPMINSD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 39 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMINSD-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPMINSD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 39 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMINSD-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPMINSD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 39 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMINSQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPMINSQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 39 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMINSQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPMINSQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 39 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMINSQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPMINSQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 39 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMINSW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMINSW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig ea /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMINSW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMINSW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig ea /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMINSW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPMINSW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig ea /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMINUB-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMINUB"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig da /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMINUB-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMINUB"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig da /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMINUB-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPMINUB"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig da /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMINUD-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPMINUD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 3b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMINUD-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPMINUD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 3b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMINUD-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPMINUD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 3b /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMINUQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPMINUQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 3b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMINUQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPMINUQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 3b /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMINUQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPMINUQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 3b /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMINUW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMINUW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.wig 3a /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMINUW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMINUW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.wig 3a /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMINUW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPMINUW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.wig 3a /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMOVB2M-kreg.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVB2M"
:operands "kreg,xmmreg"
:code-string "[rm: evex.128.f3.0f38.w0 29 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVB2M-kreg.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVB2M"
:operands "kreg,ymmreg"
:code-string "[rm: evex.256.f3.0f38.w0 29 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVB2M-kreg.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVB2M"
:operands "kreg,zmmreg"
:code-string "[rm: evex.512.f3.0f38.w0 29 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMOVD2M-kreg.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVD2M"
:operands "kreg,xmmreg"
:code-string "[rm: evex.128.f3.0f38.w0 39 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VPMOVD2M-kreg.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVD2M"
:operands "kreg,ymmreg"
:code-string "[rm: evex.256.f3.0f38.w0 39 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VPMOVD2M-kreg.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVD2M"
:operands "kreg,zmmreg"
:code-string "[rm: evex.512.f3.0f38.w0 39 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VPMOVDB-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVDB"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f38.w0 31 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVDB-xmmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVDB"
:operands "xmmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f38.w0 31 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVDB-xmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVDB"
:operands "xmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f38.w0 31 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVDB-mem32-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVDB"
:operands "mem32|mask,xmmreg"
:code-string "[mr:qvm: evex.128.f3.0f38.w0 31 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVDB-mem64-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVDB"
:operands "mem64|mask,ymmreg"
:code-string "[mr:qvm: evex.256.f3.0f38.w0 31 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVDB-mem128-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVDB"
:operands "mem128|mask,zmmreg"
:code-string "[mr:qvm: evex.512.f3.0f38.w0 31 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVDW-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVDW"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f38.w0 33 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVDW-xmmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVDW"
:operands "xmmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f38.w0 33 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVDW-ymmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVDW"
:operands "ymmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f38.w0 33 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVDW-mem64-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVDW"
:operands "mem64|mask,xmmreg"
:code-string "[mr:hvm: evex.128.f3.0f38.w0 33 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVDW-mem128-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVDW"
:operands "mem128|mask,ymmreg"
:code-string "[mr:hvm: evex.256.f3.0f38.w0 33 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVDW-mem256-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVDW"
:operands "mem256|mask,zmmreg"
:code-string "[mr:hvm: evex.512.f3.0f38.w0 33 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVM2B-xmmreg.kreg (make-instance 'x86-asm-instruction
:name "VPMOVM2B"
:operands "xmmreg,kreg"
:code-string "[rm: evex.128.f3.0f38.w0 28 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVM2B-ymmreg.kreg (make-instance 'x86-asm-instruction
:name "VPMOVM2B"
:operands "ymmreg,kreg"
:code-string "[rm: evex.256.f3.0f38.w0 28 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVM2B-zmmreg.kreg (make-instance 'x86-asm-instruction
:name "VPMOVM2B"
:operands "zmmreg,kreg"
:code-string "[rm: evex.512.f3.0f38.w0 28 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMOVM2D-xmmreg.kreg (make-instance 'x86-asm-instruction
:name "VPMOVM2D"
:operands "xmmreg,kreg"
:code-string "[rm: evex.128.f3.0f38.w0 38 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VPMOVM2D-ymmreg.kreg (make-instance 'x86-asm-instruction
:name "VPMOVM2D"
:operands "ymmreg,kreg"
:code-string "[rm: evex.256.f3.0f38.w0 38 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VPMOVM2D-zmmreg.kreg (make-instance 'x86-asm-instruction
:name "VPMOVM2D"
:operands "zmmreg,kreg"
:code-string "[rm: evex.512.f3.0f38.w0 38 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VPMOVM2Q-xmmreg.kreg (make-instance 'x86-asm-instruction
:name "VPMOVM2Q"
:operands "xmmreg,kreg"
:code-string "[rm: evex.128.f3.0f38.w1 38 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VPMOVM2Q-ymmreg.kreg (make-instance 'x86-asm-instruction
:name "VPMOVM2Q"
:operands "ymmreg,kreg"
:code-string "[rm: evex.256.f3.0f38.w1 38 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VPMOVM2Q-zmmreg.kreg (make-instance 'x86-asm-instruction
:name "VPMOVM2Q"
:operands "zmmreg,kreg"
:code-string "[rm: evex.512.f3.0f38.w1 38 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VPMOVM2W-xmmreg.kreg (make-instance 'x86-asm-instruction
:name "VPMOVM2W"
:operands "xmmreg,kreg"
:code-string "[rm: evex.128.f3.0f38.w1 28 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVM2W-ymmreg.kreg (make-instance 'x86-asm-instruction
:name "VPMOVM2W"
:operands "ymmreg,kreg"
:code-string "[rm: evex.256.f3.0f38.w1 28 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVM2W-zmmreg.kreg (make-instance 'x86-asm-instruction
:name "VPMOVM2W"
:operands "zmmreg,kreg"
:code-string "[rm: evex.512.f3.0f38.w1 28 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMOVQ2M-kreg.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVQ2M"
:operands "kreg,xmmreg"
:code-string "[rm: evex.128.f3.0f38.w1 39 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VPMOVQ2M-kreg.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVQ2M"
:operands "kreg,ymmreg"
:code-string "[rm: evex.256.f3.0f38.w1 39 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VPMOVQ2M-kreg.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVQ2M"
:operands "kreg,zmmreg"
:code-string "[rm: evex.512.f3.0f38.w1 39 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VPMOVQB-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVQB"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f38.w0 32 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVQB-xmmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVQB"
:operands "xmmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f38.w0 32 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVQB-xmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVQB"
:operands "xmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f38.w0 32 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVQB-mem16-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVQB"
:operands "mem16|mask,xmmreg"
:code-string "[mr:ovm: evex.128.f3.0f38.w0 32 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVQB-mem32-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVQB"
:operands "mem32|mask,ymmreg"
:code-string "[mr:ovm: evex.256.f3.0f38.w0 32 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVQB-mem64-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVQB"
:operands "mem64|mask,zmmreg"
:code-string "[mr:ovm: evex.512.f3.0f38.w0 32 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVQD-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVQD"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f38.w0 35 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVQD-xmmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVQD"
:operands "xmmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f38.w0 35 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVQD-ymmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVQD"
:operands "ymmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f38.w0 35 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVQD-mem64-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVQD"
:operands "mem64|mask,xmmreg"
:code-string "[mr:hvm: evex.128.f3.0f38.w0 35 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVQD-mem128-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVQD"
:operands "mem128|mask,ymmreg"
:code-string "[mr:hvm: evex.256.f3.0f38.w0 35 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVQD-mem256-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVQD"
:operands "mem256|mask,zmmreg"
:code-string "[mr:hvm: evex.512.f3.0f38.w0 35 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVQW-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVQW"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f38.w0 34 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVQW-xmmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVQW"
:operands "xmmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f38.w0 34 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVQW-xmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVQW"
:operands "xmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f38.w0 34 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVQW-mem32-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVQW"
:operands "mem32|mask,xmmreg"
:code-string "[mr:qvm: evex.128.f3.0f38.w0 34 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVQW-mem64-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVQW"
:operands "mem64|mask,ymmreg"
:code-string "[mr:qvm: evex.256.f3.0f38.w0 34 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVQW-mem128-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVQW"
:operands "mem128|mask,zmmreg"
:code-string "[mr:qvm: evex.512.f3.0f38.w0 34 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVSDB-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSDB"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f38.w0 21 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSDB-xmmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVSDB"
:operands "xmmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f38.w0 21 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSDB-xmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSDB"
:operands "xmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f38.w0 21 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVSDB-mem32-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSDB"
:operands "mem32|mask,xmmreg"
:code-string "[mr:qvm: evex.128.f3.0f38.w0 21 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSDB-mem64-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVSDB"
:operands "mem64|mask,ymmreg"
:code-string "[mr:qvm: evex.256.f3.0f38.w0 21 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSDB-mem128-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSDB"
:operands "mem128|mask,zmmreg"
:code-string "[mr:qvm: evex.512.f3.0f38.w0 21 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVSDW-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSDW"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f38.w0 23 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSDW-xmmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVSDW"
:operands "xmmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f38.w0 23 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSDW-ymmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSDW"
:operands "ymmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f38.w0 23 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVSDW-mem64-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSDW"
:operands "mem64|mask,xmmreg"
:code-string "[mr:hvm: evex.128.f3.0f38.w0 23 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSDW-mem128-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVSDW"
:operands "mem128|mask,ymmreg"
:code-string "[mr:hvm: evex.256.f3.0f38.w0 23 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSDW-mem256-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSDW"
:operands "mem256|mask,zmmreg"
:code-string "[mr:hvm: evex.512.f3.0f38.w0 23 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVSQB-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSQB"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f38.w0 22 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSQB-xmmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVSQB"
:operands "xmmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f38.w0 22 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSQB-xmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSQB"
:operands "xmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f38.w0 22 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVSQB-mem16-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSQB"
:operands "mem16|mask,xmmreg"
:code-string "[mr:ovm: evex.128.f3.0f38.w0 22 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSQB-mem32-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVSQB"
:operands "mem32|mask,ymmreg"
:code-string "[mr:ovm: evex.256.f3.0f38.w0 22 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSQB-mem64-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSQB"
:operands "mem64|mask,zmmreg"
:code-string "[mr:ovm: evex.512.f3.0f38.w0 22 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVSQD-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSQD"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f38.w0 25 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSQD-xmmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVSQD"
:operands "xmmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f38.w0 25 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSQD-ymmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSQD"
:operands "ymmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f38.w0 25 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVSQD-mem64-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSQD"
:operands "mem64|mask,xmmreg"
:code-string "[mr:hvm: evex.128.f3.0f38.w0 25 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSQD-mem128-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVSQD"
:operands "mem128|mask,ymmreg"
:code-string "[mr:hvm: evex.256.f3.0f38.w0 25 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSQD-mem256-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSQD"
:operands "mem256|mask,zmmreg"
:code-string "[mr:hvm: evex.512.f3.0f38.w0 25 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVSQW-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSQW"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f38.w0 24 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSQW-xmmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVSQW"
:operands "xmmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f38.w0 24 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSQW-xmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSQW"
:operands "xmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f38.w0 24 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVSQW-mem32-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSQW"
:operands "mem32|mask,xmmreg"
:code-string "[mr:qvm: evex.128.f3.0f38.w0 24 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSQW-mem64-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVSQW"
:operands "mem64|mask,ymmreg"
:code-string "[mr:qvm: evex.256.f3.0f38.w0 24 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSQW-mem128-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSQW"
:operands "mem128|mask,zmmreg"
:code-string "[mr:qvm: evex.512.f3.0f38.w0 24 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVSWB-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSWB"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f38.w0 20 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVSWB-xmmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVSWB"
:operands "xmmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f38.w0 20 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVSWB-ymmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSWB"
:operands "ymmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f38.w0 20 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMOVSWB-mem64-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSWB"
:operands "mem64|mask,xmmreg"
:code-string "[mr:hvm: evex.128.f3.0f38.w0 20 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVSWB-mem128-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVSWB"
:operands "mem128|mask,ymmreg"
:code-string "[mr:hvm: evex.256.f3.0f38.w0 20 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVSWB-mem256-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVSWB"
:operands "mem256|mask,zmmreg"
:code-string "[mr:hvm: evex.512.f3.0f38.w0 20 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMOVSXBD-xmmreg-mask-z.xmmrm32 (make-instance 'x86-asm-instruction
:name "VPMOVSXBD"
:operands "xmmreg|mask|z,xmmrm32"
:code-string "[rm:qvm: evex.128.66.0f38.wig 21 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSXBD-ymmreg-mask-z.xmmrm64 (make-instance 'x86-asm-instruction
:name "VPMOVSXBD"
:operands "ymmreg|mask|z,xmmrm64"
:code-string "[rm:qvm: evex.256.66.0f38.wig 21 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSXBD-zmmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMOVSXBD"
:operands "zmmreg|mask|z,xmmrm128"
:code-string "[rm:qvm: evex.512.66.0f38.wig 21 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVSXBQ-xmmreg-mask-z.xmmrm16 (make-instance 'x86-asm-instruction
:name "VPMOVSXBQ"
:operands "xmmreg|mask|z,xmmrm16"
:code-string "[rm:ovm: evex.128.66.0f38.wig 22 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSXBQ-ymmreg-mask-z.xmmrm32 (make-instance 'x86-asm-instruction
:name "VPMOVSXBQ"
:operands "ymmreg|mask|z,xmmrm32"
:code-string "[rm:ovm: evex.256.66.0f38.wig 22 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSXBQ-zmmreg-mask-z.xmmrm64 (make-instance 'x86-asm-instruction
:name "VPMOVSXBQ"
:operands "zmmreg|mask|z,xmmrm64"
:code-string "[rm:ovm: evex.512.66.0f38.wig 22 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVSXBW-xmmreg-mask-z.xmmrm64 (make-instance 'x86-asm-instruction
:name "VPMOVSXBW"
:operands "xmmreg|mask|z,xmmrm64"
:code-string "[rm:hvm: evex.128.66.0f38.wig 20 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVSXBW-ymmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMOVSXBW"
:operands "ymmreg|mask|z,xmmrm128"
:code-string "[rm:hvm: evex.256.66.0f38.wig 20 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVSXBW-zmmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMOVSXBW"
:operands "zmmreg|mask|z,ymmrm256"
:code-string "[rm:hvm: evex.512.66.0f38.wig 20 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMOVSXDQ-xmmreg-mask-z.xmmrm64 (make-instance 'x86-asm-instruction
:name "VPMOVSXDQ"
:operands "xmmreg|mask|z,xmmrm64"
:code-string "[rm:hvm: evex.128.66.0f38.w0 25 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSXDQ-ymmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMOVSXDQ"
:operands "ymmreg|mask|z,xmmrm128"
:code-string "[rm:hvm: evex.256.66.0f38.w0 25 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSXDQ-zmmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMOVSXDQ"
:operands "zmmreg|mask|z,ymmrm256"
:code-string "[rm:hvm: evex.512.66.0f38.w0 25 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVSXWD-xmmreg-mask-z.xmmrm64 (make-instance 'x86-asm-instruction
:name "VPMOVSXWD"
:operands "xmmreg|mask|z,xmmrm64"
:code-string "[rm:hvm: evex.128.66.0f38.wig 23 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSXWD-ymmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMOVSXWD"
:operands "ymmreg|mask|z,xmmrm128"
:code-string "[rm:hvm: evex.256.66.0f38.wig 23 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSXWD-zmmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMOVSXWD"
:operands "zmmreg|mask|z,ymmrm256"
:code-string "[rm:hvm: evex.512.66.0f38.wig 23 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVSXWQ-xmmreg-mask-z.xmmrm32 (make-instance 'x86-asm-instruction
:name "VPMOVSXWQ"
:operands "xmmreg|mask|z,xmmrm32"
:code-string "[rm:qvm: evex.128.66.0f38.wig 24 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSXWQ-ymmreg-mask-z.xmmrm64 (make-instance 'x86-asm-instruction
:name "VPMOVSXWQ"
:operands "ymmreg|mask|z,xmmrm64"
:code-string "[rm:qvm: evex.256.66.0f38.wig 24 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVSXWQ-zmmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMOVSXWQ"
:operands "zmmreg|mask|z,xmmrm128"
:code-string "[rm:qvm: evex.512.66.0f38.wig 24 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVUSDB-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSDB"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f38.w0 11 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSDB-xmmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSDB"
:operands "xmmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f38.w0 11 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSDB-xmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSDB"
:operands "xmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f38.w0 11 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVUSDB-mem32-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSDB"
:operands "mem32|mask,xmmreg"
:code-string "[mr:qvm: evex.128.f3.0f38.w0 11 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSDB-mem64-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSDB"
:operands "mem64|mask,ymmreg"
:code-string "[mr:qvm: evex.256.f3.0f38.w0 11 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSDB-mem128-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSDB"
:operands "mem128|mask,zmmreg"
:code-string "[mr:qvm: evex.512.f3.0f38.w0 11 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVUSDW-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSDW"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f38.w0 13 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSDW-xmmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSDW"
:operands "xmmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f38.w0 13 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSDW-ymmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSDW"
:operands "ymmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f38.w0 13 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVUSDW-mem64-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSDW"
:operands "mem64|mask,xmmreg"
:code-string "[mr:hvm: evex.128.f3.0f38.w0 13 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSDW-mem128-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSDW"
:operands "mem128|mask,ymmreg"
:code-string "[mr:hvm: evex.256.f3.0f38.w0 13 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSDW-mem256-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSDW"
:operands "mem256|mask,zmmreg"
:code-string "[mr:hvm: evex.512.f3.0f38.w0 13 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVUSQB-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSQB"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f38.w0 12 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSQB-xmmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSQB"
:operands "xmmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f38.w0 12 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSQB-xmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSQB"
:operands "xmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f38.w0 12 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVUSQB-mem16-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSQB"
:operands "mem16|mask,xmmreg"
:code-string "[mr:ovm: evex.128.f3.0f38.w0 12 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSQB-mem32-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSQB"
:operands "mem32|mask,ymmreg"
:code-string "[mr:ovm: evex.256.f3.0f38.w0 12 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSQB-mem64-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSQB"
:operands "mem64|mask,zmmreg"
:code-string "[mr:ovm: evex.512.f3.0f38.w0 12 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVUSQD-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSQD"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f38.w0 15 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSQD-xmmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSQD"
:operands "xmmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f38.w0 15 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSQD-ymmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSQD"
:operands "ymmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f38.w0 15 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVUSQD-mem64-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSQD"
:operands "mem64|mask,xmmreg"
:code-string "[mr:hvm: evex.128.f3.0f38.w0 15 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSQD-mem128-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSQD"
:operands "mem128|mask,ymmreg"
:code-string "[mr:hvm: evex.256.f3.0f38.w0 15 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSQD-mem256-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSQD"
:operands "mem256|mask,zmmreg"
:code-string "[mr:hvm: evex.512.f3.0f38.w0 15 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVUSQW-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSQW"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f38.w0 14 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSQW-xmmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSQW"
:operands "xmmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f38.w0 14 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSQW-xmmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSQW"
:operands "xmmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f38.w0 14 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVUSQW-mem32-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSQW"
:operands "mem32|mask,xmmreg"
:code-string "[mr:qvm: evex.128.f3.0f38.w0 14 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSQW-mem64-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSQW"
:operands "mem64|mask,ymmreg"
:code-string "[mr:qvm: evex.256.f3.0f38.w0 14 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVUSQW-mem128-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSQW"
:operands "mem128|mask,zmmreg"
:code-string "[mr:qvm: evex.512.f3.0f38.w0 14 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVUSWB-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSWB"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f38.w0 10 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVUSWB-xmmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSWB"
:operands "xmmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f38.w0 10 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVUSWB-ymmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSWB"
:operands "ymmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f38.w0 10 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMOVUSWB-mem64-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSWB"
:operands "mem64|mask,xmmreg"
:code-string "[mr:hvm: evex.128.f3.0f38.w0 10 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVUSWB-mem128-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSWB"
:operands "mem128|mask,ymmreg"
:code-string "[mr:hvm: evex.256.f3.0f38.w0 10 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVUSWB-mem256-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVUSWB"
:operands "mem256|mask,zmmreg"
:code-string "[mr:hvm: evex.512.f3.0f38.w0 10 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMOVW2M-kreg.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVW2M"
:operands "kreg,xmmreg"
:code-string "[rm: evex.128.f3.0f38.w1 29 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVW2M-kreg.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVW2M"
:operands "kreg,ymmreg"
:code-string "[rm: evex.256.f3.0f38.w1 29 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVW2M-kreg.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVW2M"
:operands "kreg,zmmreg"
:code-string "[rm: evex.512.f3.0f38.w1 29 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMOVWB-xmmreg-mask-z.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVWB"
:operands "xmmreg|mask|z,xmmreg"
:code-string "[mr: evex.128.f3.0f38.w0 30 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVWB-xmmreg-mask-z.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVWB"
:operands "xmmreg|mask|z,ymmreg"
:code-string "[mr: evex.256.f3.0f38.w0 30 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVWB-ymmreg-mask-z.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVWB"
:operands "ymmreg|mask|z,zmmreg"
:code-string "[mr: evex.512.f3.0f38.w0 30 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMOVWB-mem64-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPMOVWB"
:operands "mem64|mask,xmmreg"
:code-string "[mr:hvm: evex.128.f3.0f38.w0 30 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVWB-mem128-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPMOVWB"
:operands "mem128|mask,ymmreg"
:code-string "[mr:hvm: evex.256.f3.0f38.w0 30 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVWB-mem256-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPMOVWB"
:operands "mem256|mask,zmmreg"
:code-string "[mr:hvm: evex.512.f3.0f38.w0 30 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMOVZXBD-xmmreg-mask-z.xmmrm32 (make-instance 'x86-asm-instruction
:name "VPMOVZXBD"
:operands "xmmreg|mask|z,xmmrm32"
:code-string "[rm:qvm: evex.128.66.0f38.wig 31 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVZXBD-ymmreg-mask-z.xmmrm64 (make-instance 'x86-asm-instruction
:name "VPMOVZXBD"
:operands "ymmreg|mask|z,xmmrm64"
:code-string "[rm:qvm: evex.256.66.0f38.wig 31 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVZXBD-zmmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMOVZXBD"
:operands "zmmreg|mask|z,xmmrm128"
:code-string "[rm:qvm: evex.512.66.0f38.wig 31 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVZXBQ-xmmreg-mask-z.xmmrm16 (make-instance 'x86-asm-instruction
:name "VPMOVZXBQ"
:operands "xmmreg|mask|z,xmmrm16"
:code-string "[rm:ovm: evex.128.66.0f38.wig 32 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVZXBQ-ymmreg-mask-z.xmmrm32 (make-instance 'x86-asm-instruction
:name "VPMOVZXBQ"
:operands "ymmreg|mask|z,xmmrm32"
:code-string "[rm:ovm: evex.256.66.0f38.wig 32 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVZXBQ-zmmreg-mask-z.xmmrm64 (make-instance 'x86-asm-instruction
:name "VPMOVZXBQ"
:operands "zmmreg|mask|z,xmmrm64"
:code-string "[rm:ovm: evex.512.66.0f38.wig 32 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVZXBW-xmmreg-mask-z.xmmrm64 (make-instance 'x86-asm-instruction
:name "VPMOVZXBW"
:operands "xmmreg|mask|z,xmmrm64"
:code-string "[rm:hvm: evex.128.66.0f38.wig 30 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVZXBW-ymmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMOVZXBW"
:operands "ymmreg|mask|z,xmmrm128"
:code-string "[rm:hvm: evex.256.66.0f38.wig 30 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMOVZXBW-zmmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMOVZXBW"
:operands "zmmreg|mask|z,ymmrm256"
:code-string "[rm:hvm: evex.512.66.0f38.wig 30 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMOVZXDQ-xmmreg-mask-z.xmmrm64 (make-instance 'x86-asm-instruction
:name "VPMOVZXDQ"
:operands "xmmreg|mask|z,xmmrm64"
:code-string "[rm:hvm: evex.128.66.0f38.w0 35 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVZXDQ-ymmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMOVZXDQ"
:operands "ymmreg|mask|z,xmmrm128"
:code-string "[rm:hvm: evex.256.66.0f38.w0 35 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVZXDQ-zmmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMOVZXDQ"
:operands "zmmreg|mask|z,ymmrm256"
:code-string "[rm:hvm: evex.512.66.0f38.w0 35 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVZXWD-xmmreg-mask-z.xmmrm64 (make-instance 'x86-asm-instruction
:name "VPMOVZXWD"
:operands "xmmreg|mask|z,xmmrm64"
:code-string "[rm:hvm: evex.128.66.0f38.wig 33 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVZXWD-ymmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMOVZXWD"
:operands "ymmreg|mask|z,xmmrm128"
:code-string "[rm:hvm: evex.256.66.0f38.wig 33 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVZXWD-zmmreg-mask-z.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMOVZXWD"
:operands "zmmreg|mask|z,ymmrm256"
:code-string "[rm:hvm: evex.512.66.0f38.wig 33 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMOVZXWQ-xmmreg-mask-z.xmmrm32 (make-instance 'x86-asm-instruction
:name "VPMOVZXWQ"
:operands "xmmreg|mask|z,xmmrm32"
:code-string "[rm:qvm: evex.128.66.0f38.wig 34 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVZXWQ-ymmreg-mask-z.xmmrm64 (make-instance 'x86-asm-instruction
:name "VPMOVZXWQ"
:operands "ymmreg|mask|z,xmmrm64"
:code-string "[rm:qvm: evex.256.66.0f38.wig 34 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMOVZXWQ-zmmreg-mask-z.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMOVZXWQ"
:operands "zmmreg|mask|z,xmmrm128"
:code-string "[rm:qvm: evex.512.66.0f38.wig 34 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMULDQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPMULDQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 28 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMULDQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPMULDQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 28 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMULDQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPMULDQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 28 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMULHRSW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMULHRSW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.wig 0b /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMULHRSW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMULHRSW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.wig 0b /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMULHRSW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPMULHRSW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.wig 0b /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMULHUW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMULHUW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig e4 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMULHUW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMULHUW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig e4 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMULHUW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPMULHUW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig e4 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMULHW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMULHW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig e5 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMULHW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMULHW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig e5 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMULHW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPMULHW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig e5 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMULLD-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPMULLD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 40 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMULLD-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPMULLD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 40 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMULLD-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPMULLD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 40 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPMULLQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPMULLQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 40 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VPMULLQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPMULLQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 40 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VPMULLQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPMULLQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 40 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VPMULLW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPMULLW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig d5 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMULLW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPMULLW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig d5 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPMULLW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPMULLW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig d5 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPMULTISHIFTQB-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPMULTISHIFTQB"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 83 /r ]"
:arch-flags (list "AVX512VL" "AVX512VBMI" "FUTURE")))

(setf VPMULTISHIFTQB-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPMULTISHIFTQB"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 83 /r ]"
:arch-flags (list "AVX512VL" "AVX512VBMI" "FUTURE")))

(setf VPMULTISHIFTQB-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPMULTISHIFTQB"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 83 /r ]"
:arch-flags (list "AVX512VBMI" "FUTURE")))

(setf VPMULUDQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPMULUDQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 f4 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMULUDQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPMULUDQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 f4 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPMULUDQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPMULUDQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 f4 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPORD-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPORD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f.w0 eb /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPORD-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPORD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f.w0 eb /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPORD-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPORD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f.w0 eb /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPORQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPORQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 eb /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPORQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPORQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 eb /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPORQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPORQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 eb /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPROLD-xmmreg-mask-z.xmmrm128-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPROLD"
:operands "xmmreg|mask|z,xmmrm128|b32,imm8"
:code-string "[vmi:fv: evex.nds.128.66.0f.w0 72 /1 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPROLD-ymmreg-mask-z.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPROLD"
:operands "ymmreg|mask|z,ymmrm256|b32,imm8"
:code-string "[vmi:fv: evex.nds.256.66.0f.w0 72 /1 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPROLD-zmmreg-mask-z.zmmrm512-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPROLD"
:operands "zmmreg|mask|z,zmmrm512|b32,imm8"
:code-string "[vmi:fv: evex.nds.512.66.0f.w0 72 /1 ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPROLQ-xmmreg-mask-z.xmmrm128-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPROLQ"
:operands "xmmreg|mask|z,xmmrm128|b64,imm8"
:code-string "[vmi:fv: evex.nds.128.66.0f.w1 72 /1 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPROLQ-ymmreg-mask-z.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPROLQ"
:operands "ymmreg|mask|z,ymmrm256|b64,imm8"
:code-string "[vmi:fv: evex.nds.256.66.0f.w1 72 /1 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPROLQ-zmmreg-mask-z.zmmrm512-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPROLQ"
:operands "zmmreg|mask|z,zmmrm512|b64,imm8"
:code-string "[vmi:fv: evex.nds.512.66.0f.w1 72 /1 ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPROLVD-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPROLVD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 15 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPROLVD-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPROLVD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 15 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPROLVD-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPROLVD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 15 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPROLVQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPROLVQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 15 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPROLVQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPROLVQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 15 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPROLVQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPROLVQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 15 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPRORD-xmmreg-mask-z.xmmrm128-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPRORD"
:operands "xmmreg|mask|z,xmmrm128|b32,imm8"
:code-string "[vmi:fv: evex.nds.128.66.0f.w0 72 /0 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPRORD-ymmreg-mask-z.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPRORD"
:operands "ymmreg|mask|z,ymmrm256|b32,imm8"
:code-string "[vmi:fv: evex.nds.256.66.0f.w0 72 /0 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPRORD-zmmreg-mask-z.zmmrm512-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPRORD"
:operands "zmmreg|mask|z,zmmrm512|b32,imm8"
:code-string "[vmi:fv: evex.nds.512.66.0f.w0 72 /0 ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPRORQ-xmmreg-mask-z.xmmrm128-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPRORQ"
:operands "xmmreg|mask|z,xmmrm128|b64,imm8"
:code-string "[vmi:fv: evex.nds.128.66.0f.w1 72 /0 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPRORQ-ymmreg-mask-z.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPRORQ"
:operands "ymmreg|mask|z,ymmrm256|b64,imm8"
:code-string "[vmi:fv: evex.nds.256.66.0f.w1 72 /0 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPRORQ-zmmreg-mask-z.zmmrm512-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPRORQ"
:operands "zmmreg|mask|z,zmmrm512|b64,imm8"
:code-string "[vmi:fv: evex.nds.512.66.0f.w1 72 /0 ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPRORVD-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPRORVD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 14 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPRORVD-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPRORVD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 14 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPRORVD-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPRORVD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 14 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPRORVQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPRORVQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 14 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPRORVQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPRORVQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 14 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPRORVQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPRORVQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 14 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSADBW-xmmreg.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSADBW"
:operands "xmmreg,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig f6 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSADBW-ymmreg.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSADBW"
:operands "ymmreg,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig f6 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSADBW-zmmreg.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPSADBW"
:operands "zmmreg,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig f6 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSCATTERDD-xmem32-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPSCATTERDD"
:operands "xmem32|mask,xmmreg"
:code-string "[mr:t1s: vsibx evex.128.66.0f38.w0 a0 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSCATTERDD-ymem32-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPSCATTERDD"
:operands "ymem32|mask,ymmreg"
:code-string "[mr:t1s: vsiby evex.256.66.0f38.w0 a0 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSCATTERDD-zmem32-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPSCATTERDD"
:operands "zmem32|mask,zmmreg"
:code-string "[mr:t1s: vsibz evex.512.66.0f38.w0 a0 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSCATTERDQ-xmem64-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPSCATTERDQ"
:operands "xmem64|mask,xmmreg"
:code-string "[mr:t1s: vsibx evex.128.66.0f38.w1 a0 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSCATTERDQ-xmem64-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPSCATTERDQ"
:operands "xmem64|mask,ymmreg"
:code-string "[mr:t1s: vsibx evex.256.66.0f38.w1 a0 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSCATTERDQ-ymem64-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPSCATTERDQ"
:operands "ymem64|mask,zmmreg"
:code-string "[mr:t1s: vsiby evex.512.66.0f38.w1 a0 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSCATTERQD-xmem32-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPSCATTERQD"
:operands "xmem32|mask,xmmreg"
:code-string "[mr:t1s: vsibx evex.128.66.0f38.w0 a1 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSCATTERQD-ymem32-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPSCATTERQD"
:operands "ymem32|mask,xmmreg"
:code-string "[mr:t1s: vsiby evex.256.66.0f38.w0 a1 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSCATTERQD-zmem32-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPSCATTERQD"
:operands "zmem32|mask,ymmreg"
:code-string "[mr:t1s: vsibz evex.512.66.0f38.w0 a1 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSCATTERQQ-xmem64-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VPSCATTERQQ"
:operands "xmem64|mask,xmmreg"
:code-string "[mr:t1s: vsibx evex.128.66.0f38.w1 a1 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSCATTERQQ-ymem64-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VPSCATTERQQ"
:operands "ymem64|mask,ymmreg"
:code-string "[mr:t1s: vsiby evex.256.66.0f38.w1 a1 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSCATTERQQ-zmem64-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VPSCATTERQQ"
:operands "zmem64|mask,zmmreg"
:code-string "[mr:t1s: vsibz evex.512.66.0f38.w1 a1 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSHUFB-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSHUFB"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.wig 00 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSHUFB-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSHUFB"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.wig 00 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSHUFB-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPSHUFB"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.wig 00 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSHUFD-xmmreg-mask-z.xmmrm128-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPSHUFD"
:operands "xmmreg|mask|z,xmmrm128|b32,imm8"
:code-string "[rmi:fv: evex.128.66.0f.w0 70 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSHUFD-ymmreg-mask-z.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPSHUFD"
:operands "ymmreg|mask|z,ymmrm256|b32,imm8"
:code-string "[rmi:fv: evex.256.66.0f.w0 70 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSHUFD-zmmreg-mask-z.zmmrm512-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPSHUFD"
:operands "zmmreg|mask|z,zmmrm512|b32,imm8"
:code-string "[rmi:fv: evex.512.66.0f.w0 70 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSHUFHW-xmmreg-mask-z.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPSHUFHW"
:operands "xmmreg|mask|z,xmmrm128,imm8"
:code-string "[rmi:fvm: evex.128.f3.0f.wig 70 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSHUFHW-ymmreg-mask-z.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPSHUFHW"
:operands "ymmreg|mask|z,ymmrm256,imm8"
:code-string "[rmi:fvm: evex.256.f3.0f.wig 70 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSHUFHW-zmmreg-mask-z.zmmrm512.imm8 (make-instance 'x86-asm-instruction
:name "VPSHUFHW"
:operands "zmmreg|mask|z,zmmrm512,imm8"
:code-string "[rmi:fvm: evex.512.f3.0f.wig 70 /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSHUFLW-xmmreg-mask-z.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPSHUFLW"
:operands "xmmreg|mask|z,xmmrm128,imm8"
:code-string "[rmi:fvm: evex.128.f2.0f.wig 70 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSHUFLW-ymmreg-mask-z.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPSHUFLW"
:operands "ymmreg|mask|z,ymmrm256,imm8"
:code-string "[rmi:fvm: evex.256.f2.0f.wig 70 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSHUFLW-zmmreg-mask-z.zmmrm512.imm8 (make-instance 'x86-asm-instruction
:name "VPSHUFLW"
:operands "zmmreg|mask|z,zmmrm512,imm8"
:code-string "[rmi:fvm: evex.512.f2.0f.wig 70 /r ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSLLD-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSLLD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.128.66.0f.w0 f2 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSLLD-ymmreg-mask-z.ymmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSLLD"
:operands "ymmreg|mask|z,ymmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.256.66.0f.w0 f2 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSLLD-zmmreg-mask-z.zmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSLLD"
:operands "zmmreg|mask|z,zmmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.512.66.0f.w0 f2 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSLLD-xmmreg-mask-z.xmmrm128-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLD"
:operands "xmmreg|mask|z,xmmrm128|b32,imm8"
:code-string "[vmi:fv: evex.nds.128.66.0f.w0 72 /6 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSLLD-ymmreg-mask-z.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLD"
:operands "ymmreg|mask|z,ymmrm256|b32,imm8"
:code-string "[vmi:fv: evex.nds.256.66.0f.w0 72 /6 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSLLD-zmmreg-mask-z.zmmrm512-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLD"
:operands "zmmreg|mask|z,zmmrm512|b32,imm8"
:code-string "[vmi:fv: evex.nds.512.66.0f.w0 72 /6 ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSLLDQ-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLDQ"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[vmi:fvm: evex.nds.128.66.0f.wig 73 /7 ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSLLDQ-ymmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLDQ"
:operands "ymmreg,ymmrm256,imm8"
:code-string "[vmi:fvm: evex.nds.256.66.0f.wig 73 /7 ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSLLDQ-zmmreg.zmmrm512.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLDQ"
:operands "zmmreg,zmmrm512,imm8"
:code-string "[vmi:fvm: evex.nds.512.66.0f.wig 73 /7 ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSLLQ-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSLLQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.128.66.0f.w1 f3 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSLLQ-ymmreg-mask-z.ymmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSLLQ"
:operands "ymmreg|mask|z,ymmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.256.66.0f.w1 f3 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSLLQ-zmmreg-mask-z.zmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSLLQ"
:operands "zmmreg|mask|z,zmmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.512.66.0f.w1 f3 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSLLQ-xmmreg-mask-z.xmmrm128-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLQ"
:operands "xmmreg|mask|z,xmmrm128|b64,imm8"
:code-string "[vmi:fv: evex.nds.128.66.0f.w1 73 /6 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSLLQ-ymmreg-mask-z.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLQ"
:operands "ymmreg|mask|z,ymmrm256|b64,imm8"
:code-string "[vmi:fv: evex.nds.256.66.0f.w1 73 /6 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSLLQ-zmmreg-mask-z.zmmrm512-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLQ"
:operands "zmmreg|mask|z,zmmrm512|b64,imm8"
:code-string "[vmi:fv: evex.nds.512.66.0f.w1 73 /6 ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSLLVD-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPSLLVD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 47 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSLLVD-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPSLLVD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 47 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSLLVD-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPSLLVD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 47 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSLLVQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPSLLVQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 47 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSLLVQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPSLLVQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 47 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSLLVQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPSLLVQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 47 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSLLVW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSLLVW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.w1 12 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSLLVW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSLLVW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.w1 12 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSLLVW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPSLLVW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.w1 12 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSLLW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSLLW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.128.66.0f.wig f1 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSLLW-ymmreg-mask-z.ymmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSLLW"
:operands "ymmreg|mask|z,ymmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.256.66.0f.wig f1 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSLLW-zmmreg-mask-z.zmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSLLW"
:operands "zmmreg|mask|z,zmmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.512.66.0f.wig f1 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSLLW-xmmreg-mask-z.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLW"
:operands "xmmreg|mask|z,xmmrm128,imm8"
:code-string "[vmi:fvm: evex.nds.128.66.0f.wig 71 /6 ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSLLW-ymmreg-mask-z.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLW"
:operands "ymmreg|mask|z,ymmrm256,imm8"
:code-string "[vmi:fvm: evex.nds.256.66.0f.wig 71 /6 ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSLLW-zmmreg-mask-z.zmmrm512.imm8 (make-instance 'x86-asm-instruction
:name "VPSLLW"
:operands "zmmreg|mask|z,zmmrm512,imm8"
:code-string "[vmi:fvm: evex.nds.512.66.0f.wig 71 /6 ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSRAD-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRAD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.128.66.0f.w0 e2 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRAD-ymmreg-mask-z.ymmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRAD"
:operands "ymmreg|mask|z,ymmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.256.66.0f.w0 e2 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRAD-zmmreg-mask-z.zmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRAD"
:operands "zmmreg|mask|z,zmmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.512.66.0f.w0 e2 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSRAD-xmmreg-mask-z.xmmrm128-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPSRAD"
:operands "xmmreg|mask|z,xmmrm128|b32,imm8"
:code-string "[vmi:fv: evex.nds.128.66.0f.w0 72 /4 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRAD-ymmreg-mask-z.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPSRAD"
:operands "ymmreg|mask|z,ymmrm256|b32,imm8"
:code-string "[vmi:fv: evex.nds.256.66.0f.w0 72 /4 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRAD-zmmreg-mask-z.zmmrm512-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPSRAD"
:operands "zmmreg|mask|z,zmmrm512|b32,imm8"
:code-string "[vmi:fv: evex.nds.512.66.0f.w0 72 /4 ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSRAQ-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRAQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.128.66.0f.w1 e2 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRAQ-ymmreg-mask-z.ymmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRAQ"
:operands "ymmreg|mask|z,ymmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.256.66.0f.w1 e2 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRAQ-zmmreg-mask-z.zmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRAQ"
:operands "zmmreg|mask|z,zmmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.512.66.0f.w1 e2 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSRAQ-xmmreg-mask-z.xmmrm128-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPSRAQ"
:operands "xmmreg|mask|z,xmmrm128|b64,imm8"
:code-string "[vmi:fv: evex.nds.128.66.0f.w1 72 /4 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRAQ-ymmreg-mask-z.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPSRAQ"
:operands "ymmreg|mask|z,ymmrm256|b64,imm8"
:code-string "[vmi:fv: evex.nds.256.66.0f.w1 72 /4 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRAQ-zmmreg-mask-z.zmmrm512-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPSRAQ"
:operands "zmmreg|mask|z,zmmrm512|b64,imm8"
:code-string "[vmi:fv: evex.nds.512.66.0f.w1 72 /4 ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSRAVD-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPSRAVD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 46 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRAVD-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPSRAVD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 46 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRAVD-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPSRAVD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 46 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSRAVQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPSRAVQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 46 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRAVQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPSRAVQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 46 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRAVQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPSRAVQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 46 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSRAVW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRAVW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.w1 11 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSRAVW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSRAVW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.w1 11 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSRAVW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPSRAVW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.w1 11 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSRAW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRAW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.128.66.0f.wig e1 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSRAW-ymmreg-mask-z.ymmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRAW"
:operands "ymmreg|mask|z,ymmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.256.66.0f.wig e1 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSRAW-zmmreg-mask-z.zmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRAW"
:operands "zmmreg|mask|z,zmmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.512.66.0f.wig e1 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSRAW-xmmreg-mask-z.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPSRAW"
:operands "xmmreg|mask|z,xmmrm128,imm8"
:code-string "[vmi:fvm: evex.nds.128.66.0f.wig 71 /4 ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSRAW-ymmreg-mask-z.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPSRAW"
:operands "ymmreg|mask|z,ymmrm256,imm8"
:code-string "[vmi:fvm: evex.nds.256.66.0f.wig 71 /4 ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSRAW-zmmreg-mask-z.zmmrm512.imm8 (make-instance 'x86-asm-instruction
:name "VPSRAW"
:operands "zmmreg|mask|z,zmmrm512,imm8"
:code-string "[vmi:fvm: evex.nds.512.66.0f.wig 71 /4 ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSRLD-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRLD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.128.66.0f.w0 d2 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRLD-ymmreg-mask-z.ymmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRLD"
:operands "ymmreg|mask|z,ymmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.256.66.0f.w0 d2 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRLD-zmmreg-mask-z.zmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRLD"
:operands "zmmreg|mask|z,zmmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.512.66.0f.w0 d2 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSRLD-xmmreg-mask-z.xmmrm128-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLD"
:operands "xmmreg|mask|z,xmmrm128|b32,imm8"
:code-string "[vmi:fv: evex.nds.128.66.0f.w0 72 /2 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRLD-ymmreg-mask-z.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLD"
:operands "ymmreg|mask|z,ymmrm256|b32,imm8"
:code-string "[vmi:fv: evex.nds.256.66.0f.w0 72 /2 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRLD-zmmreg-mask-z.zmmrm512-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLD"
:operands "zmmreg|mask|z,zmmrm512|b32,imm8"
:code-string "[vmi:fv: evex.nds.512.66.0f.w0 72 /2 ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSRLDQ-xmmreg.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLDQ"
:operands "xmmreg,xmmrm128,imm8"
:code-string "[vmi:fvm: evex.nds.128.66.0f.wig 73 /3 ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSRLDQ-ymmreg.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLDQ"
:operands "ymmreg,ymmrm256,imm8"
:code-string "[vmi:fvm: evex.nds.256.66.0f.wig 73 /3 ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSRLDQ-zmmreg.zmmrm512.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLDQ"
:operands "zmmreg,zmmrm512,imm8"
:code-string "[vmi:fvm: evex.nds.512.66.0f.wig 73 /3 ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSRLQ-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRLQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.128.66.0f.w1 d3 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRLQ-ymmreg-mask-z.ymmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRLQ"
:operands "ymmreg|mask|z,ymmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.256.66.0f.w1 d3 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRLQ-zmmreg-mask-z.zmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRLQ"
:operands "zmmreg|mask|z,zmmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.512.66.0f.w1 d3 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSRLQ-xmmreg-mask-z.xmmrm128-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLQ"
:operands "xmmreg|mask|z,xmmrm128|b64,imm8"
:code-string "[vmi:fv: evex.nds.128.66.0f.w1 73 /2 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRLQ-ymmreg-mask-z.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLQ"
:operands "ymmreg|mask|z,ymmrm256|b64,imm8"
:code-string "[vmi:fv: evex.nds.256.66.0f.w1 73 /2 ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRLQ-zmmreg-mask-z.zmmrm512-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLQ"
:operands "zmmreg|mask|z,zmmrm512|b64,imm8"
:code-string "[vmi:fv: evex.nds.512.66.0f.w1 73 /2 ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSRLVD-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPSRLVD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 45 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRLVD-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPSRLVD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 45 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRLVD-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPSRLVD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 45 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSRLVQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPSRLVQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 45 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRLVQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPSRLVQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 45 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSRLVQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPSRLVQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 45 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSRLVW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRLVW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.w1 10 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSRLVW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSRLVW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.w1 10 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSRLVW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPSRLVW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.w1 10 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSRLW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRLW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.128.66.0f.wig d1 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSRLW-ymmreg-mask-z.ymmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRLW"
:operands "ymmreg|mask|z,ymmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.256.66.0f.wig d1 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSRLW-zmmreg-mask-z.zmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSRLW"
:operands "zmmreg|mask|z,zmmreg,xmmrm128"
:code-string "[rvm:m128: evex.nds.512.66.0f.wig d1 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSRLW-xmmreg-mask-z.xmmrm128.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLW"
:operands "xmmreg|mask|z,xmmrm128,imm8"
:code-string "[vmi:fvm: evex.nds.128.66.0f.wig 71 /2 ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSRLW-ymmreg-mask-z.ymmrm256.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLW"
:operands "ymmreg|mask|z,ymmrm256,imm8"
:code-string "[vmi:fvm: evex.nds.256.66.0f.wig 71 /2 ib ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSRLW-zmmreg-mask-z.zmmrm512.imm8 (make-instance 'x86-asm-instruction
:name "VPSRLW"
:operands "zmmreg|mask|z,zmmrm512,imm8"
:code-string "[vmi:fvm: evex.nds.512.66.0f.wig 71 /2 ib ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSUBB-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSUBB"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig f8 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSUBB-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSUBB"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig f8 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSUBB-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPSUBB"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig f8 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSUBD-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPSUBD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f.w0 fa /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSUBD-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPSUBD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f.w0 fa /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSUBD-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPSUBD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f.w0 fa /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSUBQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPSUBQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 fb /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSUBQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPSUBQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 fb /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPSUBQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPSUBQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 fb /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPSUBSB-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSUBSB"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig e8 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSUBSB-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSUBSB"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig e8 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSUBSB-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPSUBSB"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig e8 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSUBSW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSUBSW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig e9 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSUBSW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSUBSW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig e9 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSUBSW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPSUBSW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig e9 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSUBUSB-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSUBUSB"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig d8 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSUBUSB-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSUBUSB"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig d8 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSUBUSB-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPSUBUSB"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig d8 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSUBUSW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSUBUSW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig d9 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSUBUSW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSUBUSW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig d9 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSUBUSW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPSUBUSW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig d9 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPSUBW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPSUBW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig f9 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSUBW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPSUBW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig f9 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPSUBW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPSUBW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig f9 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPTERNLOGD-xmmreg-mask-z.xmmreg.xmmrm128-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPTERNLOGD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32,imm8"
:code-string "[rvmi:fv: evex.nds.128.66.0f3a.w0 25 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPTERNLOGD-ymmreg-mask-z.ymmreg.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPTERNLOGD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32,imm8"
:code-string "[rvmi:fv: evex.nds.256.66.0f3a.w0 25 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPTERNLOGD-zmmreg-mask-z.zmmreg.zmmrm512-b32.imm8 (make-instance 'x86-asm-instruction
:name "VPTERNLOGD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32,imm8"
:code-string "[rvmi:fv: evex.nds.512.66.0f3a.w0 25 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPTERNLOGQ-xmmreg-mask-z.xmmreg.xmmrm128-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPTERNLOGQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64,imm8"
:code-string "[rvmi:fv: evex.nds.128.66.0f3a.w1 25 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPTERNLOGQ-ymmreg-mask-z.ymmreg.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPTERNLOGQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64,imm8"
:code-string "[rvmi:fv: evex.nds.256.66.0f3a.w1 25 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPTERNLOGQ-zmmreg-mask-z.zmmreg.zmmrm512-b64.imm8 (make-instance 'x86-asm-instruction
:name "VPTERNLOGQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64,imm8"
:code-string "[rvmi:fv: evex.nds.512.66.0f3a.w1 25 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPTESTMB-kreg-mask.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPTESTMB"
:operands "kreg|mask,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.w0 26 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPTESTMB-kreg-mask.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPTESTMB"
:operands "kreg|mask,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.w0 26 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPTESTMB-kreg-mask.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPTESTMB"
:operands "kreg|mask,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.w0 26 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPTESTMD-kreg-mask.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPTESTMD"
:operands "kreg|mask,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 27 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPTESTMD-kreg-mask.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPTESTMD"
:operands "kreg|mask,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 27 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPTESTMD-kreg-mask.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPTESTMD"
:operands "kreg|mask,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 27 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPTESTMQ-kreg-mask.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPTESTMQ"
:operands "kreg|mask,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 27 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPTESTMQ-kreg-mask.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPTESTMQ"
:operands "kreg|mask,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 27 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPTESTMQ-kreg-mask.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPTESTMQ"
:operands "kreg|mask,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 27 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPTESTMW-kreg-mask.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPTESTMW"
:operands "kreg|mask,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f38.w1 26 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPTESTMW-kreg-mask.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPTESTMW"
:operands "kreg|mask,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f38.w1 26 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPTESTMW-kreg-mask.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPTESTMW"
:operands "kreg|mask,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f38.w1 26 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPTESTNMB-kreg-mask.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPTESTNMB"
:operands "kreg|mask,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.f3.0f38.w0 26 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPTESTNMB-kreg-mask.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPTESTNMB"
:operands "kreg|mask,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.f3.0f38.w0 26 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPTESTNMB-kreg-mask.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPTESTNMB"
:operands "kreg|mask,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.f3.0f38.w0 26 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPTESTNMD-kreg-mask.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPTESTNMD"
:operands "kreg|mask,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.f3.0f38.w0 27 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPTESTNMD-kreg-mask.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPTESTNMD"
:operands "kreg|mask,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.f3.0f38.w0 27 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPTESTNMD-kreg-mask.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPTESTNMD"
:operands "kreg|mask,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.f3.0f38.w0 27 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPTESTNMQ-kreg-mask.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPTESTNMQ"
:operands "kreg|mask,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.f3.0f38.w1 27 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPTESTNMQ-kreg-mask.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPTESTNMQ"
:operands "kreg|mask,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.f3.0f38.w1 27 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPTESTNMQ-kreg-mask.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPTESTNMQ"
:operands "kreg|mask,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.f3.0f38.w1 27 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPTESTNMW-kreg-mask.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPTESTNMW"
:operands "kreg|mask,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.f3.0f38.w1 26 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPTESTNMW-kreg-mask.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPTESTNMW"
:operands "kreg|mask,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.f3.0f38.w1 26 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPTESTNMW-kreg-mask.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPTESTNMW"
:operands "kreg|mask,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.f3.0f38.w1 26 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPUNPCKHBW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPUNPCKHBW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig 68 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPUNPCKHBW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPUNPCKHBW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig 68 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPUNPCKHBW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPUNPCKHBW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig 68 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPUNPCKHDQ-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPUNPCKHDQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f.w0 6a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPUNPCKHDQ-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPUNPCKHDQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f.w0 6a /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPUNPCKHDQ-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPUNPCKHDQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f.w0 6a /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPUNPCKHQDQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPUNPCKHQDQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 6d /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPUNPCKHQDQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPUNPCKHQDQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 6d /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPUNPCKHQDQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPUNPCKHQDQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 6d /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPUNPCKHWD-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPUNPCKHWD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig 69 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPUNPCKHWD-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPUNPCKHWD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig 69 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPUNPCKHWD-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPUNPCKHWD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig 69 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPUNPCKLBW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPUNPCKLBW"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig 60 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPUNPCKLBW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPUNPCKLBW"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig 60 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPUNPCKLBW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPUNPCKLBW"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig 60 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPUNPCKLDQ-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPUNPCKLDQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f.w0 62 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPUNPCKLDQ-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPUNPCKLDQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f.w0 62 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPUNPCKLDQ-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPUNPCKLDQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f.w0 62 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPUNPCKLQDQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPUNPCKLQDQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 6c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPUNPCKLQDQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPUNPCKLQDQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 6c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPUNPCKLQDQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPUNPCKLQDQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 6c /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPUNPCKLWD-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x86-asm-instruction
:name "VPUNPCKLWD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128"
:code-string "[rvm:fvm: evex.nds.128.66.0f.wig 61 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPUNPCKLWD-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x86-asm-instruction
:name "VPUNPCKLWD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256"
:code-string "[rvm:fvm: evex.nds.256.66.0f.wig 61 /r ]"
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(setf VPUNPCKLWD-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x86-asm-instruction
:name "VPUNPCKLWD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512"
:code-string "[rvm:fvm: evex.nds.512.66.0f.wig 61 /r ]"
:arch-flags (list "AVX512BW" "FUTURE")))

(setf VPXORD-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VPXORD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f.w0 ef /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPXORD-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VPXORD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f.w0 ef /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPXORD-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VPXORD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.66.0f.w0 ef /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VPXORQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VPXORQ"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 ef /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPXORQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VPXORQ"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 ef /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VPXORQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VPXORQ"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 ef /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VRANGEPD-xmmreg-mask-z.xmmreg.xmmrm128-b64.imm8 (make-instance 'x86-asm-instruction
:name "VRANGEPD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64,imm8"
:code-string "[rvmi:fv: evex.nds.128.66.0f3a.w1 50 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VRANGEPD-ymmreg-mask-z.ymmreg.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VRANGEPD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64,imm8"
:code-string "[rvmi:fv: evex.nds.256.66.0f3a.w1 50 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VRANGEPD-zmmreg-mask-z.zmmreg.zmmrm512-b64-sae.imm8 (make-instance 'x86-asm-instruction
:name "VRANGEPD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|sae,imm8"
:code-string "[rvmi:fv: evex.nds.512.66.0f3a.w1 50 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VRANGEPS-xmmreg-mask-z.xmmreg.xmmrm128-b32.imm8 (make-instance 'x86-asm-instruction
:name "VRANGEPS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32,imm8"
:code-string "[rvmi:fv: evex.nds.128.66.0f3a.w0 50 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VRANGEPS-ymmreg-mask-z.ymmreg.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VRANGEPS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32,imm8"
:code-string "[rvmi:fv: evex.nds.256.66.0f3a.w0 50 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VRANGEPS-zmmreg-mask-z.zmmreg.zmmrm512-b32-sae.imm8 (make-instance 'x86-asm-instruction
:name "VRANGEPS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|sae,imm8"
:code-string "[rvmi:fv: evex.nds.512.66.0f3a.w0 50 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VRANGESD-xmmreg-mask-z.xmmreg.xmmrm64-sae.imm8 (make-instance 'x86-asm-instruction
:name "VRANGESD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|sae,imm8"
:code-string "[rvmi:t1s: evex.nds.128.66.0f3a.w1 51 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VRANGESS-xmmreg-mask-z.xmmreg.xmmrm32-sae.imm8 (make-instance 'x86-asm-instruction
:name "VRANGESS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|sae,imm8"
:code-string "[rvmi:t1s: evex.nds.128.66.0f3a.w0 51 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VRCP14PD-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VRCP14PD"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.66.0f38.w1 4c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VRCP14PD-ymmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VRCP14PD"
:operands "ymmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.66.0f38.w1 4c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VRCP14PD-zmmreg-mask-z.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VRCP14PD"
:operands "zmmreg|mask|z,zmmrm512|b64"
:code-string "[rm:fv: evex.512.66.0f38.w1 4c /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VRCP14PS-xmmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VRCP14PS"
:operands "xmmreg|mask|z,xmmrm128|b32"
:code-string "[rm:fv: evex.128.66.0f38.w0 4c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VRCP14PS-ymmreg-mask-z.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VRCP14PS"
:operands "ymmreg|mask|z,ymmrm256|b32"
:code-string "[rm:fv: evex.256.66.0f38.w0 4c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VRCP14PS-zmmreg-mask-z.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VRCP14PS"
:operands "zmmreg|mask|z,zmmrm512|b32"
:code-string "[rm:fv: evex.512.66.0f38.w0 4c /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VRCP14SD-xmmreg-mask-z.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VRCP14SD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w1 4d /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VRCP14SS-xmmreg-mask-z.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VRCP14SS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w0 4d /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VRCP28PD-zmmreg-mask-z.zmmrm512-b64-sae (make-instance 'x86-asm-instruction
:name "VRCP28PD"
:operands "zmmreg|mask|z,zmmrm512|b64|sae"
:code-string "[rm:fv: evex.512.66.0f38.w1 ca /r ]"
:arch-flags (list "AVX512ER" "FUTURE")))

(setf VRCP28PS-zmmreg-mask-z.zmmrm512-b32-sae (make-instance 'x86-asm-instruction
:name "VRCP28PS"
:operands "zmmreg|mask|z,zmmrm512|b32|sae"
:code-string "[rm:fv: evex.512.66.0f38.w0 ca /r ]"
:arch-flags (list "AVX512ER" "FUTURE")))

(setf VRCP28SD-xmmreg-mask-z.xmmreg.xmmrm64-sae (make-instance 'x86-asm-instruction
:name "VRCP28SD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|sae"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w1 cb /r ]"
:arch-flags (list "AVX512ER" "FUTURE")))

(setf VRCP28SS-xmmreg-mask-z.xmmreg.xmmrm32-sae (make-instance 'x86-asm-instruction
:name "VRCP28SS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|sae"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w0 cb /r ]"
:arch-flags (list "AVX512ER" "FUTURE")))

(setf VREDUCEPD-xmmreg-mask-z.xmmrm128-b64.imm8 (make-instance 'x86-asm-instruction
:name "VREDUCEPD"
:operands "xmmreg|mask|z,xmmrm128|b64,imm8"
:code-string "[rmi:fv: evex.128.66.0f3a.w1 56 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VREDUCEPD-ymmreg-mask-z.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VREDUCEPD"
:operands "ymmreg|mask|z,ymmrm256|b64,imm8"
:code-string "[rmi:fv: evex.256.66.0f3a.w1 56 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VREDUCEPD-zmmreg-mask-z.zmmrm512-b64-sae.imm8 (make-instance 'x86-asm-instruction
:name "VREDUCEPD"
:operands "zmmreg|mask|z,zmmrm512|b64|sae,imm8"
:code-string "[rmi:fv: evex.512.66.0f3a.w1 56 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VREDUCEPS-xmmreg-mask-z.xmmrm128-b32.imm8 (make-instance 'x86-asm-instruction
:name "VREDUCEPS"
:operands "xmmreg|mask|z,xmmrm128|b32,imm8"
:code-string "[rmi:fv: evex.128.66.0f3a.w0 56 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VREDUCEPS-ymmreg-mask-z.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VREDUCEPS"
:operands "ymmreg|mask|z,ymmrm256|b32,imm8"
:code-string "[rmi:fv: evex.256.66.0f3a.w0 56 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VREDUCEPS-zmmreg-mask-z.zmmrm512-b32-sae.imm8 (make-instance 'x86-asm-instruction
:name "VREDUCEPS"
:operands "zmmreg|mask|z,zmmrm512|b32|sae,imm8"
:code-string "[rmi:fv: evex.512.66.0f3a.w0 56 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VREDUCESD-xmmreg-mask-z.xmmreg.xmmrm64-sae.imm8 (make-instance 'x86-asm-instruction
:name "VREDUCESD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|sae,imm8"
:code-string "[rvmi:t1s: evex.nds.128.66.0f3a.w1 57 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VREDUCESS-xmmreg-mask-z.xmmreg.xmmrm32-sae.imm8 (make-instance 'x86-asm-instruction
:name "VREDUCESS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|sae,imm8"
:code-string "[rvmi:t1s: evex.nds.128.66.0f3a.w0 57 /r ib ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VRNDSCALEPD-xmmreg-mask-z.xmmrm128-b64.imm8 (make-instance 'x86-asm-instruction
:name "VRNDSCALEPD"
:operands "xmmreg|mask|z,xmmrm128|b64,imm8"
:code-string "[rmi:fv: evex.128.66.0f3a.w1 09 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VRNDSCALEPD-ymmreg-mask-z.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VRNDSCALEPD"
:operands "ymmreg|mask|z,ymmrm256|b64,imm8"
:code-string "[rmi:fv: evex.256.66.0f3a.w1 09 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VRNDSCALEPD-zmmreg-mask-z.zmmrm512-b64-sae.imm8 (make-instance 'x86-asm-instruction
:name "VRNDSCALEPD"
:operands "zmmreg|mask|z,zmmrm512|b64|sae,imm8"
:code-string "[rmi:fv: evex.512.66.0f3a.w1 09 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VRNDSCALEPS-xmmreg-mask-z.xmmrm128-b32.imm8 (make-instance 'x86-asm-instruction
:name "VRNDSCALEPS"
:operands "xmmreg|mask|z,xmmrm128|b32,imm8"
:code-string "[rmi:fv: evex.128.66.0f3a.w0 08 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VRNDSCALEPS-ymmreg-mask-z.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VRNDSCALEPS"
:operands "ymmreg|mask|z,ymmrm256|b32,imm8"
:code-string "[rmi:fv: evex.256.66.0f3a.w0 08 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VRNDSCALEPS-zmmreg-mask-z.zmmrm512-b32-sae.imm8 (make-instance 'x86-asm-instruction
:name "VRNDSCALEPS"
:operands "zmmreg|mask|z,zmmrm512|b32|sae,imm8"
:code-string "[rmi:fv: evex.512.66.0f3a.w0 08 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VRNDSCALESD-xmmreg-mask-z.xmmreg.xmmrm64-sae.imm8 (make-instance 'x86-asm-instruction
:name "VRNDSCALESD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|sae,imm8"
:code-string "[rvmi:t1s: evex.nds.128.66.0f3a.w1 0b /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VRNDSCALESS-xmmreg-mask-z.xmmreg.xmmrm32-sae.imm8 (make-instance 'x86-asm-instruction
:name "VRNDSCALESS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|sae,imm8"
:code-string "[rvmi:t1s: evex.nds.128.66.0f3a.w0 0a /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VRSQRT14PD-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VRSQRT14PD"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.66.0f38.w1 4e /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VRSQRT14PD-ymmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VRSQRT14PD"
:operands "ymmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.66.0f38.w1 4e /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VRSQRT14PD-zmmreg-mask-z.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VRSQRT14PD"
:operands "zmmreg|mask|z,zmmrm512|b64"
:code-string "[rm:fv: evex.512.66.0f38.w1 4e /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VRSQRT14PS-xmmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VRSQRT14PS"
:operands "xmmreg|mask|z,xmmrm128|b32"
:code-string "[rm:fv: evex.128.66.0f38.w0 4e /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VRSQRT14PS-ymmreg-mask-z.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VRSQRT14PS"
:operands "ymmreg|mask|z,ymmrm256|b32"
:code-string "[rm:fv: evex.256.66.0f38.w0 4e /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VRSQRT14PS-zmmreg-mask-z.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VRSQRT14PS"
:operands "zmmreg|mask|z,zmmrm512|b32"
:code-string "[rm:fv: evex.512.66.0f38.w0 4e /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VRSQRT14SD-xmmreg-mask-z.xmmreg.xmmrm64 (make-instance 'x86-asm-instruction
:name "VRSQRT14SD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w1 4f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VRSQRT14SS-xmmreg-mask-z.xmmreg.xmmrm32 (make-instance 'x86-asm-instruction
:name "VRSQRT14SS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w0 4f /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VRSQRT28PD-zmmreg-mask-z.zmmrm512-b64-sae (make-instance 'x86-asm-instruction
:name "VRSQRT28PD"
:operands "zmmreg|mask|z,zmmrm512|b64|sae"
:code-string "[rm:fv: evex.512.66.0f38.w1 cc /r ]"
:arch-flags (list "AVX512ER" "FUTURE")))

(setf VRSQRT28PS-zmmreg-mask-z.zmmrm512-b32-sae (make-instance 'x86-asm-instruction
:name "VRSQRT28PS"
:operands "zmmreg|mask|z,zmmrm512|b32|sae"
:code-string "[rm:fv: evex.512.66.0f38.w0 cc /r ]"
:arch-flags (list "AVX512ER" "FUTURE")))

(setf VRSQRT28SD-xmmreg-mask-z.xmmreg.xmmrm64-sae (make-instance 'x86-asm-instruction
:name "VRSQRT28SD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|sae"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w1 cd /r ]"
:arch-flags (list "AVX512ER" "FUTURE")))

(setf VRSQRT28SS-xmmreg-mask-z.xmmreg.xmmrm32-sae (make-instance 'x86-asm-instruction
:name "VRSQRT28SS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|sae"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w0 cd /r ]"
:arch-flags (list "AVX512ER" "FUTURE")))

(setf VSCALEFPD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VSCALEFPD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w1 2c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSCALEFPD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VSCALEFPD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w1 2c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSCALEFPD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VSCALEFPD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w1 2c /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSCALEFPS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VSCALEFPS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.66.0f38.w0 2c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSCALEFPS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VSCALEFPS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.66.0f38.w0 2c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSCALEFPS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VSCALEFPS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.66.0f38.w0 2c /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSCALEFSD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VSCALEFSD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w1 2d /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSCALEFSS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VSCALEFSS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|er"
:code-string "[rvm:t1s: evex.nds.128.66.0f38.w0 2d /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSCATTERDPD-xmem64-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VSCATTERDPD"
:operands "xmem64|mask,xmmreg"
:code-string "[mr:t1s: vsibx evex.128.66.0f38.w1 a2 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSCATTERDPD-xmem64-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VSCATTERDPD"
:operands "xmem64|mask,ymmreg"
:code-string "[mr:t1s: vsibx evex.256.66.0f38.w1 a2 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSCATTERDPD-ymem64-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VSCATTERDPD"
:operands "ymem64|mask,zmmreg"
:code-string "[mr:t1s: vsiby evex.512.66.0f38.w1 a2 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSCATTERDPS-xmem32-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VSCATTERDPS"
:operands "xmem32|mask,xmmreg"
:code-string "[mr:t1s: vsibx evex.128.66.0f38.w0 a2 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSCATTERDPS-ymem32-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VSCATTERDPS"
:operands "ymem32|mask,ymmreg"
:code-string "[mr:t1s: vsiby evex.256.66.0f38.w0 a2 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSCATTERDPS-zmem32-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VSCATTERDPS"
:operands "zmem32|mask,zmmreg"
:code-string "[mr:t1s: vsibz evex.512.66.0f38.w0 a2 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSCATTERPF0DPD-ymem64-mask (make-instance 'x86-asm-instruction
:name "VSCATTERPF0DPD"
:operands "ymem64|mask"
:code-string "[m:t1s: vsiby evex.512.66.0f38.w1 c6 /5 ]"
:arch-flags (list "AVX512PF" "FUTURE")))

(setf VSCATTERPF0DPS-zmem32-mask (make-instance 'x86-asm-instruction
:name "VSCATTERPF0DPS"
:operands "zmem32|mask"
:code-string "[m:t1s: vsibz evex.512.66.0f38.w0 c6 /5 ]"
:arch-flags (list "AVX512PF" "FUTURE")))

(setf VSCATTERPF0QPD-zmem64-mask (make-instance 'x86-asm-instruction
:name "VSCATTERPF0QPD"
:operands "zmem64|mask"
:code-string "[m:t1s: vsibz evex.512.66.0f38.w1 c7 /5 ]"
:arch-flags (list "AVX512PF" "FUTURE")))

(setf VSCATTERPF0QPS-zmem32-mask (make-instance 'x86-asm-instruction
:name "VSCATTERPF0QPS"
:operands "zmem32|mask"
:code-string "[m:t1s: vsibz evex.512.66.0f38.w0 c7 /5 ]"
:arch-flags (list "AVX512PF" "FUTURE")))

(setf VSCATTERPF1DPD-ymem64-mask (make-instance 'x86-asm-instruction
:name "VSCATTERPF1DPD"
:operands "ymem64|mask"
:code-string "[m:t1s: vsiby evex.512.66.0f38.w1 c6 /6 ]"
:arch-flags (list "AVX512PF" "FUTURE")))

(setf VSCATTERPF1DPS-zmem32-mask (make-instance 'x86-asm-instruction
:name "VSCATTERPF1DPS"
:operands "zmem32|mask"
:code-string "[m:t1s: vsibz evex.512.66.0f38.w0 c6 /6 ]"
:arch-flags (list "AVX512PF" "FUTURE")))

(setf VSCATTERPF1QPD-zmem64-mask (make-instance 'x86-asm-instruction
:name "VSCATTERPF1QPD"
:operands "zmem64|mask"
:code-string "[m:t1s: vsibz evex.512.66.0f38.w1 c7 /6 ]"
:arch-flags (list "AVX512PF" "FUTURE")))

(setf VSCATTERPF1QPS-zmem32-mask (make-instance 'x86-asm-instruction
:name "VSCATTERPF1QPS"
:operands "zmem32|mask"
:code-string "[m:t1s: vsibz evex.512.66.0f38.w0 c7 /6 ]"
:arch-flags (list "AVX512PF" "FUTURE")))

(setf VSCATTERQPD-xmem64-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VSCATTERQPD"
:operands "xmem64|mask,xmmreg"
:code-string "[mr:t1s: vsibx evex.128.66.0f38.w1 a3 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSCATTERQPD-ymem64-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VSCATTERQPD"
:operands "ymem64|mask,ymmreg"
:code-string "[mr:t1s: vsiby evex.256.66.0f38.w1 a3 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSCATTERQPD-zmem64-mask.zmmreg (make-instance 'x86-asm-instruction
:name "VSCATTERQPD"
:operands "zmem64|mask,zmmreg"
:code-string "[mr:t1s: vsibz evex.512.66.0f38.w1 a3 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSCATTERQPS-xmem32-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VSCATTERQPS"
:operands "xmem32|mask,xmmreg"
:code-string "[mr:t1s: vsibx evex.128.66.0f38.w0 a3 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSCATTERQPS-ymem32-mask.xmmreg (make-instance 'x86-asm-instruction
:name "VSCATTERQPS"
:operands "ymem32|mask,xmmreg"
:code-string "[mr:t1s: vsiby evex.256.66.0f38.w0 a3 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSCATTERQPS-zmem32-mask.ymmreg (make-instance 'x86-asm-instruction
:name "VSCATTERQPS"
:operands "zmem32|mask,ymmreg"
:code-string "[mr:t1s: vsibz evex.512.66.0f38.w0 a3 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSHUFF32X4-ymmreg-mask-z.ymmreg.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VSHUFF32X4"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32,imm8"
:code-string "[rvmi:fv: evex.nds.256.66.0f3a.w0 23 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSHUFF32X4-zmmreg-mask-z.zmmreg.zmmrm512-b32.imm8 (make-instance 'x86-asm-instruction
:name "VSHUFF32X4"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32,imm8"
:code-string "[rvmi:fv: evex.nds.512.66.0f3a.w0 23 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSHUFF64X2-ymmreg-mask-z.ymmreg.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VSHUFF64X2"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64,imm8"
:code-string "[rvmi:fv: evex.nds.256.66.0f3a.w1 23 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSHUFF64X2-zmmreg-mask-z.zmmreg.zmmrm512-b64.imm8 (make-instance 'x86-asm-instruction
:name "VSHUFF64X2"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64,imm8"
:code-string "[rvmi:fv: evex.nds.512.66.0f3a.w1 23 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSHUFI32X4-ymmreg-mask-z.ymmreg.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VSHUFI32X4"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32,imm8"
:code-string "[rvmi:fv: evex.nds.256.66.0f3a.w0 43 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSHUFI32X4-zmmreg-mask-z.zmmreg.zmmrm512-b32.imm8 (make-instance 'x86-asm-instruction
:name "VSHUFI32X4"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32,imm8"
:code-string "[rvmi:fv: evex.nds.512.66.0f3a.w0 43 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSHUFI64X2-ymmreg-mask-z.ymmreg.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VSHUFI64X2"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64,imm8"
:code-string "[rvmi:fv: evex.nds.256.66.0f3a.w1 43 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSHUFI64X2-zmmreg-mask-z.zmmreg.zmmrm512-b64.imm8 (make-instance 'x86-asm-instruction
:name "VSHUFI64X2"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64,imm8"
:code-string "[rvmi:fv: evex.nds.512.66.0f3a.w1 43 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSHUFPD-xmmreg-mask-z.xmmreg.xmmrm128-b64.imm8 (make-instance 'x86-asm-instruction
:name "VSHUFPD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64,imm8"
:code-string "[rvmi:fv: evex.nds.128.66.0f.w1 c6 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSHUFPD-ymmreg-mask-z.ymmreg.ymmrm256-b64.imm8 (make-instance 'x86-asm-instruction
:name "VSHUFPD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64,imm8"
:code-string "[rvmi:fv: evex.nds.256.66.0f.w1 c6 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSHUFPD-zmmreg-mask-z.zmmreg.zmmrm512-b64.imm8 (make-instance 'x86-asm-instruction
:name "VSHUFPD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64,imm8"
:code-string "[rvmi:fv: evex.nds.512.66.0f.w1 c6 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSHUFPS-xmmreg-mask-z.xmmreg.xmmrm128-b32.imm8 (make-instance 'x86-asm-instruction
:name "VSHUFPS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32,imm8"
:code-string "[rvmi:fv: evex.nds.128.0f.w0 c6 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSHUFPS-ymmreg-mask-z.ymmreg.ymmrm256-b32.imm8 (make-instance 'x86-asm-instruction
:name "VSHUFPS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32,imm8"
:code-string "[rvmi:fv: evex.nds.256.0f.w0 c6 /r ib ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSHUFPS-zmmreg-mask-z.zmmreg.zmmrm512-b32.imm8 (make-instance 'x86-asm-instruction
:name "VSHUFPS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32,imm8"
:code-string "[rvmi:fv: evex.nds.512.0f.w0 c6 /r ib ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSQRTPD-xmmreg-mask-z.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VSQRTPD"
:operands "xmmreg|mask|z,xmmrm128|b64"
:code-string "[rm:fv: evex.128.66.0f.w1 51 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSQRTPD-ymmreg-mask-z.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VSQRTPD"
:operands "ymmreg|mask|z,ymmrm256|b64"
:code-string "[rm:fv: evex.256.66.0f.w1 51 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSQRTPD-zmmreg-mask-z.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VSQRTPD"
:operands "zmmreg|mask|z,zmmrm512|b64|er"
:code-string "[rm:fv: evex.512.66.0f.w1 51 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSQRTPS-xmmreg-mask-z.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VSQRTPS"
:operands "xmmreg|mask|z,xmmrm128|b32"
:code-string "[rm:fv: evex.128.0f.w0 51 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSQRTPS-ymmreg-mask-z.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VSQRTPS"
:operands "ymmreg|mask|z,ymmrm256|b32"
:code-string "[rm:fv: evex.256.0f.w0 51 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSQRTPS-zmmreg-mask-z.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VSQRTPS"
:operands "zmmreg|mask|z,zmmrm512|b32|er"
:code-string "[rm:fv: evex.512.0f.w0 51 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSQRTSD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VSQRTSD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|er"
:code-string "[rvm:t1s: evex.nds.128.f2.0f.w1 51 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSQRTSS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VSQRTSS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|er"
:code-string "[rvm:t1s: evex.nds.128.f3.0f.w0 51 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSUBPD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VSUBPD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 5c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSUBPD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VSUBPD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 5c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSUBPD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x86-asm-instruction
:name "VSUBPD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64|er"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 5c /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSUBPS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VSUBPS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.0f.w0 5c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSUBPS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VSUBPS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.0f.w0 5c /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VSUBPS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x86-asm-instruction
:name "VSUBPS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32|er"
:code-string "[rvm:fv: evex.nds.512.0f.w0 5c /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSUBSD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x86-asm-instruction
:name "VSUBSD"
:operands "xmmreg|mask|z,xmmreg,xmmrm64|er"
:code-string "[rvm:t1s: evex.nds.128.f2.0f.w1 5c /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VSUBSS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x86-asm-instruction
:name "VSUBSS"
:operands "xmmreg|mask|z,xmmreg,xmmrm32|er"
:code-string "[rvm:t1s: evex.nds.128.f3.0f.w0 5c /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VUCOMISD-xmmreg.xmmrm64-sae (make-instance 'x86-asm-instruction
:name "VUCOMISD"
:operands "xmmreg,xmmrm64|sae"
:code-string "[rm:t1s: evex.128.66.0f.w1 2e /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VUCOMISS-xmmreg.xmmrm32-sae (make-instance 'x86-asm-instruction
:name "VUCOMISS"
:operands "xmmreg,xmmrm32|sae"
:code-string "[rm:t1s: evex.128.0f.w0 2e /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VUNPCKHPD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VUNPCKHPD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 15 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VUNPCKHPD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VUNPCKHPD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 15 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VUNPCKHPD-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VUNPCKHPD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 15 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VUNPCKHPS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VUNPCKHPS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.0f.w0 15 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VUNPCKHPS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VUNPCKHPS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.0f.w0 15 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VUNPCKHPS-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VUNPCKHPS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.0f.w0 15 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VUNPCKLPD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VUNPCKLPD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 14 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VUNPCKLPD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VUNPCKLPD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 14 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VUNPCKLPD-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VUNPCKLPD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 14 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VUNPCKLPS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VUNPCKLPS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.0f.w0 14 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VUNPCKLPS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VUNPCKLPS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.0f.w0 14 /r ]"
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(setf VUNPCKLPS-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VUNPCKLPS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.0f.w0 14 /r ]"
:arch-flags (list "AVX512" "FUTURE")))

(setf VXORPD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x86-asm-instruction
:name "VXORPD"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b64"
:code-string "[rvm:fv: evex.nds.128.66.0f.w1 57 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VXORPD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x86-asm-instruction
:name "VXORPD"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b64"
:code-string "[rvm:fv: evex.nds.256.66.0f.w1 57 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VXORPD-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x86-asm-instruction
:name "VXORPD"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b64"
:code-string "[rvm:fv: evex.nds.512.66.0f.w1 57 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf VXORPS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x86-asm-instruction
:name "VXORPS"
:operands "xmmreg|mask|z,xmmreg,xmmrm128|b32"
:code-string "[rvm:fv: evex.nds.128.0f.w0 57 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VXORPS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x86-asm-instruction
:name "VXORPS"
:operands "ymmreg|mask|z,ymmreg,ymmrm256|b32"
:code-string "[rvm:fv: evex.nds.256.0f.w0 57 /r ]"
:arch-flags (list "AVX512VL" "AVX512DQ" "FUTURE")))

(setf VXORPS-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x86-asm-instruction
:name "VXORPS"
:operands "zmmreg|mask|z,zmmreg,zmmrm512|b32"
:code-string "[rvm:fv: evex.nds.512.0f.w0 57 /r ]"
:arch-flags (list "AVX512DQ" "FUTURE")))

(setf CLFLUSHOPT-mem (make-instance 'x86-asm-instruction
:name "CLFLUSHOPT"
:operands "mem"
:code-string "[m: 66 0f ae /7]"
:arch-flags (list "FUTURE")))

(setf HINT_NOP0-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP0"
:operands "rm16"
:code-string "[m: o16 0f 18 /0]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP0-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP0"
:operands "rm32"
:code-string "[m: o32 0f 18 /0]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP0-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP0"
:operands "rm64"
:code-string "[m: o64 0f 18 /0]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP1-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP1"
:operands "rm16"
:code-string "[m: o16 0f 18 /1]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP1-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP1"
:operands "rm32"
:code-string "[m: o32 0f 18 /1]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP1-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP1"
:operands "rm64"
:code-string "[m: o64 0f 18 /1]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP2-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP2"
:operands "rm16"
:code-string "[m: o16 0f 18 /2]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP2-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP2"
:operands "rm32"
:code-string "[m: o32 0f 18 /2]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP2-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP2"
:operands "rm64"
:code-string "[m: o64 0f 18 /2]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP3-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP3"
:operands "rm16"
:code-string "[m: o16 0f 18 /3]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP3-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP3"
:operands "rm32"
:code-string "[m: o32 0f 18 /3]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP3-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP3"
:operands "rm64"
:code-string "[m: o64 0f 18 /3]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP4-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP4"
:operands "rm16"
:code-string "[m: o16 0f 18 /4]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP4-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP4"
:operands "rm32"
:code-string "[m: o32 0f 18 /4]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP4-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP4"
:operands "rm64"
:code-string "[m: o64 0f 18 /4]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP5-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP5"
:operands "rm16"
:code-string "[m: o16 0f 18 /5]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP5-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP5"
:operands "rm32"
:code-string "[m: o32 0f 18 /5]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP5-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP5"
:operands "rm64"
:code-string "[m: o64 0f 18 /5]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP6-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP6"
:operands "rm16"
:code-string "[m: o16 0f 18 /6]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP6-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP6"
:operands "rm32"
:code-string "[m: o32 0f 18 /6]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP6-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP6"
:operands "rm64"
:code-string "[m: o64 0f 18 /6]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP7-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP7"
:operands "rm16"
:code-string "[m: o16 0f 18 /7]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP7-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP7"
:operands "rm32"
:code-string "[m: o32 0f 18 /7]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP7-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP7"
:operands "rm64"
:code-string "[m: o64 0f 18 /7]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP8-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP8"
:operands "rm16"
:code-string "[m: o16 0f 19 /0]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP8-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP8"
:operands "rm32"
:code-string "[m: o32 0f 19 /0]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP8-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP8"
:operands "rm64"
:code-string "[m: o64 0f 19 /0]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP9-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP9"
:operands "rm16"
:code-string "[m: o16 0f 19 /1]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP9-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP9"
:operands "rm32"
:code-string "[m: o32 0f 19 /1]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP9-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP9"
:operands "rm64"
:code-string "[m: o64 0f 19 /1]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP10-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP10"
:operands "rm16"
:code-string "[m: o16 0f 19 /2]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP10-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP10"
:operands "rm32"
:code-string "[m: o32 0f 19 /2]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP10-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP10"
:operands "rm64"
:code-string "[m: o64 0f 19 /2]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP11-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP11"
:operands "rm16"
:code-string "[m: o16 0f 19 /3]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP11-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP11"
:operands "rm32"
:code-string "[m: o32 0f 19 /3]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP11-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP11"
:operands "rm64"
:code-string "[m: o64 0f 19 /3]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP12-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP12"
:operands "rm16"
:code-string "[m: o16 0f 19 /4]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP12-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP12"
:operands "rm32"
:code-string "[m: o32 0f 19 /4]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP12-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP12"
:operands "rm64"
:code-string "[m: o64 0f 19 /4]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP13-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP13"
:operands "rm16"
:code-string "[m: o16 0f 19 /5]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP13-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP13"
:operands "rm32"
:code-string "[m: o32 0f 19 /5]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP13-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP13"
:operands "rm64"
:code-string "[m: o64 0f 19 /5]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP14-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP14"
:operands "rm16"
:code-string "[m: o16 0f 19 /6]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP14-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP14"
:operands "rm32"
:code-string "[m: o32 0f 19 /6]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP14-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP14"
:operands "rm64"
:code-string "[m: o64 0f 19 /6]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP15-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP15"
:operands "rm16"
:code-string "[m: o16 0f 19 /7]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP15-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP15"
:operands "rm32"
:code-string "[m: o32 0f 19 /7]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP15-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP15"
:operands "rm64"
:code-string "[m: o64 0f 19 /7]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP16-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP16"
:operands "rm16"
:code-string "[m: o16 0f 1a /0]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP16-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP16"
:operands "rm32"
:code-string "[m: o32 0f 1a /0]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP16-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP16"
:operands "rm64"
:code-string "[m: o64 0f 1a /0]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP17-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP17"
:operands "rm16"
:code-string "[m: o16 0f 1a /1]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP17-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP17"
:operands "rm32"
:code-string "[m: o32 0f 1a /1]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP17-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP17"
:operands "rm64"
:code-string "[m: o64 0f 1a /1]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP18-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP18"
:operands "rm16"
:code-string "[m: o16 0f 1a /2]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP18-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP18"
:operands "rm32"
:code-string "[m: o32 0f 1a /2]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP18-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP18"
:operands "rm64"
:code-string "[m: o64 0f 1a /2]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP19-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP19"
:operands "rm16"
:code-string "[m: o16 0f 1a /3]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP19-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP19"
:operands "rm32"
:code-string "[m: o32 0f 1a /3]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP19-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP19"
:operands "rm64"
:code-string "[m: o64 0f 1a /3]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP20-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP20"
:operands "rm16"
:code-string "[m: o16 0f 1a /4]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP20-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP20"
:operands "rm32"
:code-string "[m: o32 0f 1a /4]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP20-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP20"
:operands "rm64"
:code-string "[m: o64 0f 1a /4]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP21-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP21"
:operands "rm16"
:code-string "[m: o16 0f 1a /5]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP21-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP21"
:operands "rm32"
:code-string "[m: o32 0f 1a /5]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP21-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP21"
:operands "rm64"
:code-string "[m: o64 0f 1a /5]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP22-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP22"
:operands "rm16"
:code-string "[m: o16 0f 1a /6]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP22-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP22"
:operands "rm32"
:code-string "[m: o32 0f 1a /6]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP22-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP22"
:operands "rm64"
:code-string "[m: o64 0f 1a /6]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP23-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP23"
:operands "rm16"
:code-string "[m: o16 0f 1a /7]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP23-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP23"
:operands "rm32"
:code-string "[m: o32 0f 1a /7]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP23-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP23"
:operands "rm64"
:code-string "[m: o64 0f 1a /7]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP24-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP24"
:operands "rm16"
:code-string "[m: o16 0f 1b /0]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP24-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP24"
:operands "rm32"
:code-string "[m: o32 0f 1b /0]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP24-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP24"
:operands "rm64"
:code-string "[m: o64 0f 1b /0]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP25-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP25"
:operands "rm16"
:code-string "[m: o16 0f 1b /1]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP25-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP25"
:operands "rm32"
:code-string "[m: o32 0f 1b /1]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP25-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP25"
:operands "rm64"
:code-string "[m: o64 0f 1b /1]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP26-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP26"
:operands "rm16"
:code-string "[m: o16 0f 1b /2]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP26-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP26"
:operands "rm32"
:code-string "[m: o32 0f 1b /2]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP26-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP26"
:operands "rm64"
:code-string "[m: o64 0f 1b /2]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP27-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP27"
:operands "rm16"
:code-string "[m: o16 0f 1b /3]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP27-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP27"
:operands "rm32"
:code-string "[m: o32 0f 1b /3]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP27-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP27"
:operands "rm64"
:code-string "[m: o64 0f 1b /3]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP28-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP28"
:operands "rm16"
:code-string "[m: o16 0f 1b /4]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP28-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP28"
:operands "rm32"
:code-string "[m: o32 0f 1b /4]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP28-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP28"
:operands "rm64"
:code-string "[m: o64 0f 1b /4]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP29-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP29"
:operands "rm16"
:code-string "[m: o16 0f 1b /5]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP29-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP29"
:operands "rm32"
:code-string "[m: o32 0f 1b /5]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP29-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP29"
:operands "rm64"
:code-string "[m: o64 0f 1b /5]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP30-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP30"
:operands "rm16"
:code-string "[m: o16 0f 1b /6]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP30-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP30"
:operands "rm32"
:code-string "[m: o32 0f 1b /6]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP30-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP30"
:operands "rm64"
:code-string "[m: o64 0f 1b /6]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP31-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP31"
:operands "rm16"
:code-string "[m: o16 0f 1b /7]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP31-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP31"
:operands "rm32"
:code-string "[m: o32 0f 1b /7]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP31-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP31"
:operands "rm64"
:code-string "[m: o64 0f 1b /7]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP32-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP32"
:operands "rm16"
:code-string "[m: o16 0f 1c /0]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP32-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP32"
:operands "rm32"
:code-string "[m: o32 0f 1c /0]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP32-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP32"
:operands "rm64"
:code-string "[m: o64 0f 1c /0]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP33-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP33"
:operands "rm16"
:code-string "[m: o16 0f 1c /1]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP33-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP33"
:operands "rm32"
:code-string "[m: o32 0f 1c /1]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP33-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP33"
:operands "rm64"
:code-string "[m: o64 0f 1c /1]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP34-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP34"
:operands "rm16"
:code-string "[m: o16 0f 1c /2]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP34-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP34"
:operands "rm32"
:code-string "[m: o32 0f 1c /2]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP34-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP34"
:operands "rm64"
:code-string "[m: o64 0f 1c /2]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP35-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP35"
:operands "rm16"
:code-string "[m: o16 0f 1c /3]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP35-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP35"
:operands "rm32"
:code-string "[m: o32 0f 1c /3]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP35-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP35"
:operands "rm64"
:code-string "[m: o64 0f 1c /3]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP36-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP36"
:operands "rm16"
:code-string "[m: o16 0f 1c /4]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP36-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP36"
:operands "rm32"
:code-string "[m: o32 0f 1c /4]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP36-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP36"
:operands "rm64"
:code-string "[m: o64 0f 1c /4]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP37-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP37"
:operands "rm16"
:code-string "[m: o16 0f 1c /5]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP37-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP37"
:operands "rm32"
:code-string "[m: o32 0f 1c /5]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP37-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP37"
:operands "rm64"
:code-string "[m: o64 0f 1c /5]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP38-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP38"
:operands "rm16"
:code-string "[m: o16 0f 1c /6]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP38-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP38"
:operands "rm32"
:code-string "[m: o32 0f 1c /6]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP38-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP38"
:operands "rm64"
:code-string "[m: o64 0f 1c /6]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP39-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP39"
:operands "rm16"
:code-string "[m: o16 0f 1c /7]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP39-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP39"
:operands "rm32"
:code-string "[m: o32 0f 1c /7]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP39-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP39"
:operands "rm64"
:code-string "[m: o64 0f 1c /7]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP40-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP40"
:operands "rm16"
:code-string "[m: o16 0f 1d /0]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP40-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP40"
:operands "rm32"
:code-string "[m: o32 0f 1d /0]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP40-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP40"
:operands "rm64"
:code-string "[m: o64 0f 1d /0]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP41-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP41"
:operands "rm16"
:code-string "[m: o16 0f 1d /1]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP41-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP41"
:operands "rm32"
:code-string "[m: o32 0f 1d /1]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP41-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP41"
:operands "rm64"
:code-string "[m: o64 0f 1d /1]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP42-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP42"
:operands "rm16"
:code-string "[m: o16 0f 1d /2]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP42-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP42"
:operands "rm32"
:code-string "[m: o32 0f 1d /2]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP42-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP42"
:operands "rm64"
:code-string "[m: o64 0f 1d /2]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP43-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP43"
:operands "rm16"
:code-string "[m: o16 0f 1d /3]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP43-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP43"
:operands "rm32"
:code-string "[m: o32 0f 1d /3]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP43-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP43"
:operands "rm64"
:code-string "[m: o64 0f 1d /3]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP44-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP44"
:operands "rm16"
:code-string "[m: o16 0f 1d /4]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP44-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP44"
:operands "rm32"
:code-string "[m: o32 0f 1d /4]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP44-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP44"
:operands "rm64"
:code-string "[m: o64 0f 1d /4]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP45-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP45"
:operands "rm16"
:code-string "[m: o16 0f 1d /5]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP45-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP45"
:operands "rm32"
:code-string "[m: o32 0f 1d /5]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP45-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP45"
:operands "rm64"
:code-string "[m: o64 0f 1d /5]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP46-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP46"
:operands "rm16"
:code-string "[m: o16 0f 1d /6]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP46-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP46"
:operands "rm32"
:code-string "[m: o32 0f 1d /6]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP46-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP46"
:operands "rm64"
:code-string "[m: o64 0f 1d /6]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP47-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP47"
:operands "rm16"
:code-string "[m: o16 0f 1d /7]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP47-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP47"
:operands "rm32"
:code-string "[m: o32 0f 1d /7]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP47-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP47"
:operands "rm64"
:code-string "[m: o64 0f 1d /7]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP48-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP48"
:operands "rm16"
:code-string "[m: o16 0f 1e /0]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP48-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP48"
:operands "rm32"
:code-string "[m: o32 0f 1e /0]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP48-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP48"
:operands "rm64"
:code-string "[m: o64 0f 1e /0]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP49-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP49"
:operands "rm16"
:code-string "[m: o16 0f 1e /1]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP49-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP49"
:operands "rm32"
:code-string "[m: o32 0f 1e /1]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP49-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP49"
:operands "rm64"
:code-string "[m: o64 0f 1e /1]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP50-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP50"
:operands "rm16"
:code-string "[m: o16 0f 1e /2]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP50-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP50"
:operands "rm32"
:code-string "[m: o32 0f 1e /2]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP50-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP50"
:operands "rm64"
:code-string "[m: o64 0f 1e /2]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP51-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP51"
:operands "rm16"
:code-string "[m: o16 0f 1e /3]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP51-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP51"
:operands "rm32"
:code-string "[m: o32 0f 1e /3]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP51-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP51"
:operands "rm64"
:code-string "[m: o64 0f 1e /3]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP52-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP52"
:operands "rm16"
:code-string "[m: o16 0f 1e /4]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP52-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP52"
:operands "rm32"
:code-string "[m: o32 0f 1e /4]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP52-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP52"
:operands "rm64"
:code-string "[m: o64 0f 1e /4]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP53-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP53"
:operands "rm16"
:code-string "[m: o16 0f 1e /5]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP53-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP53"
:operands "rm32"
:code-string "[m: o32 0f 1e /5]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP53-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP53"
:operands "rm64"
:code-string "[m: o64 0f 1e /5]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP54-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP54"
:operands "rm16"
:code-string "[m: o16 0f 1e /6]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP54-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP54"
:operands "rm32"
:code-string "[m: o32 0f 1e /6]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP54-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP54"
:operands "rm64"
:code-string "[m: o64 0f 1e /6]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP55-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP55"
:operands "rm16"
:code-string "[m: o16 0f 1e /7]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP55-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP55"
:operands "rm32"
:code-string "[m: o32 0f 1e /7]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP55-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP55"
:operands "rm64"
:code-string "[m: o64 0f 1e /7]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP56-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP56"
:operands "rm16"
:code-string "[m: o16 0f 1f /0]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP56-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP56"
:operands "rm32"
:code-string "[m: o32 0f 1f /0]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP56-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP56"
:operands "rm64"
:code-string "[m: o64 0f 1f /0]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP57-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP57"
:operands "rm16"
:code-string "[m: o16 0f 1f /1]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP57-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP57"
:operands "rm32"
:code-string "[m: o32 0f 1f /1]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP57-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP57"
:operands "rm64"
:code-string "[m: o64 0f 1f /1]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP58-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP58"
:operands "rm16"
:code-string "[m: o16 0f 1f /2]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP58-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP58"
:operands "rm32"
:code-string "[m: o32 0f 1f /2]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP58-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP58"
:operands "rm64"
:code-string "[m: o64 0f 1f /2]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP59-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP59"
:operands "rm16"
:code-string "[m: o16 0f 1f /3]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP59-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP59"
:operands "rm32"
:code-string "[m: o32 0f 1f /3]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP59-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP59"
:operands "rm64"
:code-string "[m: o64 0f 1f /3]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP60-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP60"
:operands "rm16"
:code-string "[m: o16 0f 1f /4]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP60-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP60"
:operands "rm32"
:code-string "[m: o32 0f 1f /4]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP60-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP60"
:operands "rm64"
:code-string "[m: o64 0f 1f /4]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP61-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP61"
:operands "rm16"
:code-string "[m: o16 0f 1f /5]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP61-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP61"
:operands "rm32"
:code-string "[m: o32 0f 1f /5]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP61-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP61"
:operands "rm64"
:code-string "[m: o64 0f 1f /5]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP62-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP62"
:operands "rm16"
:code-string "[m: o16 0f 1f /6]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP62-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP62"
:operands "rm32"
:code-string "[m: o32 0f 1f /6]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP62-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP62"
:operands "rm64"
:code-string "[m: o64 0f 1f /6]"
:arch-flags (list "X64" "UNDOC")))

(setf HINT_NOP63-rm16 (make-instance 'x86-asm-instruction
:name "HINT_NOP63"
:operands "rm16"
:code-string "[m: o16 0f 1f /7]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP63-rm32 (make-instance 'x86-asm-instruction
:name "HINT_NOP63"
:operands "rm32"
:code-string "[m: o32 0f 1f /7]"
:arch-flags (list "P6" "UNDOC")))

(setf HINT_NOP63-rm64 (make-instance 'x86-asm-instruction
:name "HINT_NOP63"
:operands "rm64"
:code-string "[m: o64 0f 1f /7]"
:arch-flags (list "X64" "UNDOC"))))