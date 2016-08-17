;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :x64)

(defparameter *x64-instruction-variants-hash-table* (make-hash-table :test #'equalp :size 32768))
(defparameter ADD-mem.reg8 (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "mem" "reg8")
:code-format (list "[mr:" "hle" "00" "/r")
:arch-flags (list "8086" "SM" "LOCK")))

(defparameter ADD-reg8.reg8-mr (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "reg8" "reg8")
:code-format (list "[mr:" "00" "/r")
:arch-flags (list "8086")))

(defparameter ADD-mem.reg16 (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "mem" "reg16")
:code-format (list "[mr:" "hle" "o16" "01" "/r")
:arch-flags (list "8086" "SM" "LOCK")))

(defparameter ADD-reg16.reg16-mr (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "reg16" "reg16")
:code-format (list "[mr:" "o16" "01" "/r")
:arch-flags (list "8086")))

(defparameter ADD-mem.reg32 (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "mem" "reg32")
:code-format (list "[mr:" "hle" "o32" "01" "/r")
:arch-flags (list "386" "SM" "LOCK")))

(defparameter ADD-reg32.reg32-mr (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "reg32" "reg32")
:code-format (list "[mr:" "o32" "01" "/r")
:arch-flags (list "386")))

(defparameter ADD-mem.reg64 (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "mem" "reg64")
:code-format (list "[mr:" "hle" "o64" "01" "/r")
:arch-flags (list "X64" "SM" "LOCK")))

(defparameter ADD-reg64.reg64-mr (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "reg64" "reg64")
:code-format (list "[mr:" "o64" "01" "/r")
:arch-flags (list "X64")))

(defparameter ADD-reg8.mem (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "reg8" "mem")
:code-format (list "[rm:" "02" "/r")
:arch-flags (list "8086" "SM")))

(defparameter ADD-reg8.reg8-rm (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "reg8" "reg8")
:code-format (list "[rm:" "02" "/r")
:arch-flags (list "8086")))

(defparameter ADD-reg16.mem (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "reg16" "mem")
:code-format (list "[rm:" "o16" "03" "/r")
:arch-flags (list "8086" "SM")))

(defparameter ADD-reg16.reg16-rm (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "reg16" "reg16")
:code-format (list "[rm:" "o16" "03" "/r")
:arch-flags (list "8086")))

(defparameter ADD-reg32.mem (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "reg32" "mem")
:code-format (list "[rm:" "o32" "03" "/r")
:arch-flags (list "386" "SM")))

(defparameter ADD-reg32.reg32-rm (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "reg32" "reg32")
:code-format (list "[rm:" "o32" "03" "/r")
:arch-flags (list "386")))

(defparameter ADD-reg64.mem (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "reg64" "mem")
:code-format (list "[rm:" "o64" "03" "/r")
:arch-flags (list "X64" "SM")))

(defparameter ADD-reg64.reg64-rm (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "reg64" "reg64")
:code-format (list "[rm:" "o64" "03" "/r")
:arch-flags (list "X64")))

(defparameter ADD-rm16.imm8 (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "rm16" "imm8")
:code-format (list "[mi:" "hle" "o16" "83" "/0" "ib,s")
:arch-flags (list "8086" "LOCK")))

(defparameter ADD-rm32.imm8 (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "rm32" "imm8")
:code-format (list "[mi:" "hle" "o32" "83" "/0" "ib,s")
:arch-flags (list "386" "LOCK")))

(defparameter ADD-rm64.imm8 (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "rm64" "imm8")
:code-format (list "[mi:" "hle" "o64" "83" "/0" "ib,s")
:arch-flags (list "X64" "LOCK")))

(defparameter ADD-reg_al.imm (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "reg_al" "imm")
:code-format (list "[-i:" "04" "ib")
:arch-flags (list "8086" "SM")))

(defparameter ADD-reg_ax.sbyteword (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "reg_ax" "sbyteword")
:code-format (list "[mi:" "o16" "83" "/0" "ib,s")
:arch-flags (list "8086" "SM" "ND")))

(defparameter ADD-reg_ax.imm (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "reg_ax" "imm")
:code-format (list "[-i:" "o16" "05" "iw")
:arch-flags (list "8086" "SM")))

(defparameter ADD-reg_eax.sbytedword (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "reg_eax" "sbytedword")
:code-format (list "[mi:" "o32" "83" "/0" "ib,s")
:arch-flags (list "386" "SM" "ND")))

(defparameter ADD-reg_eax.imm (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "reg_eax" "imm")
:code-format (list "[-i:" "o32" "05" "id")
:arch-flags (list "386" "SM")))

(defparameter ADD-reg_rax.sbytedword (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "reg_rax" "sbytedword")
:code-format (list "[mi:" "o64" "83" "/0" "ib,s")
:arch-flags (list "X64" "SM" "ND")))

(defparameter ADD-reg_rax.imm (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "reg_rax" "imm")
:code-format (list "[-i:" "o64" "05" "id,s")
:arch-flags (list "X64" "SM")))

(defparameter ADD-rm8.imm (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "rm8" "imm")
:code-format (list "[mi:" "hle" "80" "/0" "ib")
:arch-flags (list "8086" "SM" "LOCK")))

(defparameter ADD-rm16.sbyteword (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "rm16" "sbyteword")
:code-format (list "[mi:" "hle" "o16" "83" "/0" "ib,s")
:arch-flags (list "8086" "SM" "LOCK" "ND")))

(defparameter ADD-rm16.imm (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "rm16" "imm")
:code-format (list "[mi:" "hle" "o16" "81" "/0" "iw")
:arch-flags (list "8086" "SM" "LOCK")))

(defparameter ADD-rm32.sbytedword (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "rm32" "sbytedword")
:code-format (list "[mi:" "hle" "o32" "83" "/0" "ib,s")
:arch-flags (list "386" "SM" "LOCK" "ND")))

(defparameter ADD-rm32.imm (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "rm32" "imm")
:code-format (list "[mi:" "hle" "o32" "81" "/0" "id")
:arch-flags (list "386" "SM" "LOCK")))

(defparameter ADD-rm64.sbytedword (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "rm64" "sbytedword")
:code-format (list "[mi:" "hle" "o64" "83" "/0" "ib,s")
:arch-flags (list "X64" "SM" "LOCK" "ND")))

(defparameter ADD-rm64.imm (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "rm64" "imm")
:code-format (list "[mi:" "hle" "o64" "81" "/0" "id,s")
:arch-flags (list "X64" "SM" "LOCK")))

(defparameter ADD-mem.imm8 (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "mem" "imm8")
:code-format (list "[mi:" "hle" "80" "/0" "ib")
:arch-flags (list "8086" "SM" "LOCK")))

(defparameter ADD-mem.sbyteword16 (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "mem" "sbyteword16")
:code-format (list "[mi:" "hle" "o16" "83" "/0" "ib,s")
:arch-flags (list "8086" "SM" "LOCK" "ND")))

(defparameter ADD-mem.imm16 (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "mem" "imm16")
:code-format (list "[mi:" "hle" "o16" "81" "/0" "iw")
:arch-flags (list "8086" "SM" "LOCK")))

(defparameter ADD-mem.sbytedword32 (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "mem" "sbytedword32")
:code-format (list "[mi:" "hle" "o32" "83" "/0" "ib,s")
:arch-flags (list "386" "SM" "LOCK" "ND")))

(defparameter ADD-mem.imm32 (make-instance 'x64-asm-instruction
:name "ADD"
:req-operands (list "mem" "imm32")
:code-format (list "[mi:" "hle" "o32" "81" "/0" "id")
:arch-flags (list "386" "SM" "LOCK")))

(defparameter FADD-mem32 (make-instance 'x64-asm-instruction
:name "FADD"
:req-operands (list "mem32")
:code-format (list "[m:" "d8" "/0")
:arch-flags (list "8086" "FPU")))

(defparameter FADD-mem64 (make-instance 'x64-asm-instruction
:name "FADD"
:req-operands (list "mem64")
:code-format (list "[m:" "dc" "/0")
:arch-flags (list "8086" "FPU")))

(defparameter FADD-fpureg-to (make-instance 'x64-asm-instruction
:name "FADD"
:req-operands (list "fpureg|to")
:code-format (list "[r:" "dc" "c0+r")
:arch-flags (list "8086" "FPU")))

(defparameter FADD-fpureg (make-instance 'x64-asm-instruction
:name "FADD"
:req-operands (list "fpureg")
:code-format (list "[r:" "d8" "c0+r")
:arch-flags (list "8086" "FPU")))

(defparameter FADD-fpureg.fpu0 (make-instance 'x64-asm-instruction
:name "FADD"
:req-operands (list "fpureg" "fpu0")
:code-format (list "[r-:" "dc" "c0+r")
:arch-flags (list "8086" "FPU")))

(defparameter FADD-fpu0.fpureg (make-instance 'x64-asm-instruction
:name "FADD"
:req-operands (list "fpu0" "fpureg")
:code-format (list "[-r:" "d8" "c0+r")
:arch-flags (list "8086" "FPU")))

(defparameter FADD-void (make-instance 'x64-asm-instruction
:name "FADD"
:req-operands (list "void")
:code-format (list "[" "de" "c1")
:arch-flags (list "8086" "FPU" "ND")))

(defparameter FADDP-fpureg (make-instance 'x64-asm-instruction
:name "FADDP"
:req-operands (list "fpureg")
:code-format (list "[r:" "de" "c0+r")
:arch-flags (list "8086" "FPU")))

(defparameter FADDP-fpureg.fpu0 (make-instance 'x64-asm-instruction
:name "FADDP"
:req-operands (list "fpureg" "fpu0")
:code-format (list "[r-:" "de" "c0+r")
:arch-flags (list "8086" "FPU")))

(defparameter FADDP-void (make-instance 'x64-asm-instruction
:name "FADDP"
:req-operands (list "void")
:code-format (list "[" "de" "c1")
:arch-flags (list "8086" "FPU" "ND")))

(defparameter FIADD-mem32 (make-instance 'x64-asm-instruction
:name "FIADD"
:req-operands (list "mem32")
:code-format (list "[m:" "da" "/0")
:arch-flags (list "8086" "FPU")))

(defparameter FIADD-mem16 (make-instance 'x64-asm-instruction
:name "FIADD"
:req-operands (list "mem16")
:code-format (list "[m:" "de" "/0")
:arch-flags (list "8086" "FPU")))

(defparameter PADDB-mmxreg.mmxrm (make-instance 'x64-asm-instruction
:name "PADDB"
:req-operands (list "mmxreg" "mmxrm")
:code-format (list "[rm:" "np" "o64nw" "0f" "fc" "/r")
:arch-flags (list "PENT" "MMX" "SQ")))

(defparameter PADDD-mmxreg.mmxrm (make-instance 'x64-asm-instruction
:name "PADDD"
:req-operands (list "mmxreg" "mmxrm")
:code-format (list "[rm:" "np" "o64nw" "0f" "fe" "/r")
:arch-flags (list "PENT" "MMX" "SQ")))

(defparameter PADDSB-mmxreg.mmxrm (make-instance 'x64-asm-instruction
:name "PADDSB"
:req-operands (list "mmxreg" "mmxrm")
:code-format (list "[rm:" "np" "o64nw" "0f" "ec" "/r")
:arch-flags (list "PENT" "MMX" "SQ")))

(defparameter PADDSIW-mmxreg.mmxrm (make-instance 'x64-asm-instruction
:name "PADDSIW"
:req-operands (list "mmxreg" "mmxrm")
:code-format (list "[rm:" "o64nw" "0f" "51" "/r")
:arch-flags (list "PENT" "MMX" "SQ" "CYRIX")))

(defparameter PADDSW-mmxreg.mmxrm (make-instance 'x64-asm-instruction
:name "PADDSW"
:req-operands (list "mmxreg" "mmxrm")
:code-format (list "[rm:" "np" "o64nw" "0f" "ed" "/r")
:arch-flags (list "PENT" "MMX" "SQ")))

(defparameter PADDUSB-mmxreg.mmxrm (make-instance 'x64-asm-instruction
:name "PADDUSB"
:req-operands (list "mmxreg" "mmxrm")
:code-format (list "[rm:" "np" "o64nw" "0f" "dc" "/r")
:arch-flags (list "PENT" "MMX" "SQ")))

(defparameter PADDUSW-mmxreg.mmxrm (make-instance 'x64-asm-instruction
:name "PADDUSW"
:req-operands (list "mmxreg" "mmxrm")
:code-format (list "[rm:" "np" "o64nw" "0f" "dd" "/r")
:arch-flags (list "PENT" "MMX" "SQ")))

(defparameter PADDW-mmxreg.mmxrm (make-instance 'x64-asm-instruction
:name "PADDW"
:req-operands (list "mmxreg" "mmxrm")
:code-format (list "[rm:" "np" "o64nw" "0f" "fd" "/r")
:arch-flags (list "PENT" "MMX" "SQ")))

(defparameter PFADD-mmxreg.mmxrm (make-instance 'x64-asm-instruction
:name "PFADD"
:req-operands (list "mmxreg" "mmxrm")
:code-format (list "[rm:" "o64nw" "0f" "0f" "/r" "9e")
:arch-flags (list "PENT" "3DNOW" "SQ")))

(defparameter PMADDWD-mmxreg.mmxrm (make-instance 'x64-asm-instruction
:name "PMADDWD"
:req-operands (list "mmxreg" "mmxrm")
:code-format (list "[rm:" "np" "o64nw" "0f" "f5" "/r")
:arch-flags (list "PENT" "MMX" "SQ")))

(defparameter XADD-mem.reg8 (make-instance 'x64-asm-instruction
:name "XADD"
:req-operands (list "mem" "reg8")
:code-format (list "[mr:" "hle" "0f" "c0" "/r")
:arch-flags (list "486" "SM" "LOCK")))

(defparameter XADD-reg8.reg8-mr (make-instance 'x64-asm-instruction
:name "XADD"
:req-operands (list "reg8" "reg8")
:code-format (list "[mr:" "0f" "c0" "/r")
:arch-flags (list "486")))

(defparameter XADD-mem.reg16 (make-instance 'x64-asm-instruction
:name "XADD"
:req-operands (list "mem" "reg16")
:code-format (list "[mr:" "hle" "o16" "0f" "c1" "/r")
:arch-flags (list "486" "SM" "LOCK")))

(defparameter XADD-reg16.reg16-mr (make-instance 'x64-asm-instruction
:name "XADD"
:req-operands (list "reg16" "reg16")
:code-format (list "[mr:" "o16" "0f" "c1" "/r")
:arch-flags (list "486")))

(defparameter XADD-mem.reg32 (make-instance 'x64-asm-instruction
:name "XADD"
:req-operands (list "mem" "reg32")
:code-format (list "[mr:" "hle" "o32" "0f" "c1" "/r")
:arch-flags (list "486" "SM" "LOCK")))

(defparameter XADD-reg32.reg32-mr (make-instance 'x64-asm-instruction
:name "XADD"
:req-operands (list "reg32" "reg32")
:code-format (list "[mr:" "o32" "0f" "c1" "/r")
:arch-flags (list "486")))

(defparameter XADD-mem.reg64 (make-instance 'x64-asm-instruction
:name "XADD"
:req-operands (list "mem" "reg64")
:code-format (list "[mr:" "hle" "o64" "0f" "c1" "/r")
:arch-flags (list "X64" "SM" "LOCK")))

(defparameter XADD-reg64.reg64-mr (make-instance 'x64-asm-instruction
:name "XADD"
:req-operands (list "reg64" "reg64")
:code-format (list "[mr:" "o64" "0f" "c1" "/r")
:arch-flags (list "X64")))

(defparameter ADDPS-xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "ADDPS"
:req-operands (list "xmmreg" "xmmrm128")
:code-format (list "[rm:" "np" "0f" "58" "/r")
:arch-flags (list "KATMAI" "SSE")))

(defparameter ADDSS-xmmreg.xmmrm32 (make-instance 'x64-asm-instruction
:name "ADDSS"
:req-operands (list "xmmreg" "xmmrm32")
:code-format (list "[rm:" "f3" "0f" "58" "/r")
:arch-flags (list "KATMAI" "SSE")))

(defparameter PADDB-xmmreg.xmmrm (make-instance 'x64-asm-instruction
:name "PADDB"
:req-operands (list "xmmreg" "xmmrm")
:code-format (list "[rm:" "66" "0f" "fc" "/r")
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(defparameter PADDW-xmmreg.xmmrm (make-instance 'x64-asm-instruction
:name "PADDW"
:req-operands (list "xmmreg" "xmmrm")
:code-format (list "[rm:" "66" "0f" "fd" "/r")
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(defparameter PADDD-xmmreg.xmmrm (make-instance 'x64-asm-instruction
:name "PADDD"
:req-operands (list "xmmreg" "xmmrm")
:code-format (list "[rm:" "66" "0f" "fe" "/r")
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(defparameter PADDQ-mmxreg.mmxrm (make-instance 'x64-asm-instruction
:name "PADDQ"
:req-operands (list "mmxreg" "mmxrm")
:code-format (list "[rm:" "np" "0f" "d4" "/r")
:arch-flags (list "WILLAMETTE" "MMX" "SQ")))

(defparameter PADDQ-xmmreg.xmmrm (make-instance 'x64-asm-instruction
:name "PADDQ"
:req-operands (list "xmmreg" "xmmrm")
:code-format (list "[rm:" "66" "0f" "d4" "/r")
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(defparameter PADDSB-xmmreg.xmmrm (make-instance 'x64-asm-instruction
:name "PADDSB"
:req-operands (list "xmmreg" "xmmrm")
:code-format (list "[rm:" "66" "0f" "ec" "/r")
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(defparameter PADDSW-xmmreg.xmmrm (make-instance 'x64-asm-instruction
:name "PADDSW"
:req-operands (list "xmmreg" "xmmrm")
:code-format (list "[rm:" "66" "0f" "ed" "/r")
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(defparameter PADDUSB-xmmreg.xmmrm (make-instance 'x64-asm-instruction
:name "PADDUSB"
:req-operands (list "xmmreg" "xmmrm")
:code-format (list "[rm:" "66" "0f" "dc" "/r")
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(defparameter PADDUSW-xmmreg.xmmrm (make-instance 'x64-asm-instruction
:name "PADDUSW"
:req-operands (list "xmmreg" "xmmrm")
:code-format (list "[rm:" "66" "0f" "dd" "/r")
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(defparameter PMADDWD-xmmreg.xmmrm (make-instance 'x64-asm-instruction
:name "PMADDWD"
:req-operands (list "xmmreg" "xmmrm")
:code-format (list "[rm:" "66" "0f" "f5" "/r")
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(defparameter ADDPD-xmmreg.xmmrm (make-instance 'x64-asm-instruction
:name "ADDPD"
:req-operands (list "xmmreg" "xmmrm")
:code-format (list "[rm:" "66" "0f" "58" "/r")
:arch-flags (list "WILLAMETTE" "SSE2" "SO")))

(defparameter ADDSD-xmmreg.xmmrm (make-instance 'x64-asm-instruction
:name "ADDSD"
:req-operands (list "xmmreg" "xmmrm")
:code-format (list "[rm:" "f2" "0f" "58" "/r")
:arch-flags (list "WILLAMETTE" "SSE2" "SQ")))

(defparameter ADDSUBPD-xmmreg.xmmrm (make-instance 'x64-asm-instruction
:name "ADDSUBPD"
:req-operands (list "xmmreg" "xmmrm")
:code-format (list "[rm:" "66" "0f" "d0" "/r")
:arch-flags (list "PRESCOTT" "SSE3" "SO")))

(defparameter ADDSUBPS-xmmreg.xmmrm (make-instance 'x64-asm-instruction
:name "ADDSUBPS"
:req-operands (list "xmmreg" "xmmrm")
:code-format (list "[rm:" "f2" "0f" "d0" "/r")
:arch-flags (list "PRESCOTT" "SSE3" "SO")))

(defparameter HADDPD-xmmreg.xmmrm (make-instance 'x64-asm-instruction
:name "HADDPD"
:req-operands (list "xmmreg" "xmmrm")
:code-format (list "[rm:" "66" "0f" "7c" "/r")
:arch-flags (list "PRESCOTT" "SSE3" "SO")))

(defparameter HADDPS-xmmreg.xmmrm (make-instance 'x64-asm-instruction
:name "HADDPS"
:req-operands (list "xmmreg" "xmmrm")
:code-format (list "[rm:" "f2" "0f" "7c" "/r")
:arch-flags (list "PRESCOTT" "SSE3" "SO")))

(defparameter PHADDW-mmxreg.mmxrm (make-instance 'x64-asm-instruction
:name "PHADDW"
:req-operands (list "mmxreg" "mmxrm")
:code-format (list "[rm:" "np" "0f" "38" "01" "/r")
:arch-flags (list "SSSE3" "MMX" "SQ")))

(defparameter PHADDW-xmmreg.xmmrm (make-instance 'x64-asm-instruction
:name "PHADDW"
:req-operands (list "xmmreg" "xmmrm")
:code-format (list "[rm:" "66" "0f" "38" "01" "/r")
:arch-flags (list "SSSE3")))

(defparameter PHADDD-mmxreg.mmxrm (make-instance 'x64-asm-instruction
:name "PHADDD"
:req-operands (list "mmxreg" "mmxrm")
:code-format (list "[rm:" "np" "0f" "38" "02" "/r")
:arch-flags (list "SSSE3" "MMX" "SQ")))

(defparameter PHADDD-xmmreg.xmmrm (make-instance 'x64-asm-instruction
:name "PHADDD"
:req-operands (list "xmmreg" "xmmrm")
:code-format (list "[rm:" "66" "0f" "38" "02" "/r")
:arch-flags (list "SSSE3")))

(defparameter PHADDSW-mmxreg.mmxrm (make-instance 'x64-asm-instruction
:name "PHADDSW"
:req-operands (list "mmxreg" "mmxrm")
:code-format (list "[rm:" "np" "0f" "38" "03" "/r")
:arch-flags (list "SSSE3" "MMX" "SQ")))

(defparameter PHADDSW-xmmreg.xmmrm (make-instance 'x64-asm-instruction
:name "PHADDSW"
:req-operands (list "xmmreg" "xmmrm")
:code-format (list "[rm:" "66" "0f" "38" "03" "/r")
:arch-flags (list "SSSE3")))

(defparameter PMADDUBSW-mmxreg.mmxrm (make-instance 'x64-asm-instruction
:name "PMADDUBSW"
:req-operands (list "mmxreg" "mmxrm")
:code-format (list "[rm:" "np" "0f" "38" "04" "/r")
:arch-flags (list "SSSE3" "MMX" "SQ")))

(defparameter PMADDUBSW-xmmreg.xmmrm (make-instance 'x64-asm-instruction
:name "PMADDUBSW"
:req-operands (list "xmmreg" "xmmrm")
:code-format (list "[rm:" "66" "0f" "38" "04" "/r")
:arch-flags (list "SSSE3")))

(defparameter VADDPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x64-asm-instruction
:name "VADDPD"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128")
:code-format (list "[rvm:" "vex.nds.128.66.0f" "58" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VADDPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x64-asm-instruction
:name "VADDPD"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256")
:code-format (list "[rvm:" "vex.nds.256.66.0f" "58" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VADDPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x64-asm-instruction
:name "VADDPS"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128")
:code-format (list "[rvm:" "vex.nds.128.0f" "58" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VADDPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x64-asm-instruction
:name "VADDPS"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256")
:code-format (list "[rvm:" "vex.nds.256.0f" "58" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VADDSD-xmmreg.xmmreg*.xmmrm64 (make-instance 'x64-asm-instruction
:name "VADDSD"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm64")
:code-format (list "[rvm:" "vex.nds.lig.f2.0f" "58" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VADDSS-xmmreg.xmmreg*.xmmrm32 (make-instance 'x64-asm-instruction
:name "VADDSS"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm32")
:code-format (list "[rvm:" "vex.nds.lig.f3.0f" "58" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VADDSUBPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x64-asm-instruction
:name "VADDSUBPD"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128")
:code-format (list "[rvm:" "vex.nds.128.66.0f" "d0" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VADDSUBPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x64-asm-instruction
:name "VADDSUBPD"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256")
:code-format (list "[rvm:" "vex.nds.256.66.0f" "d0" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VADDSUBPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x64-asm-instruction
:name "VADDSUBPS"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128")
:code-format (list "[rvm:" "vex.nds.128.f2.0f" "d0" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VADDSUBPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x64-asm-instruction
:name "VADDSUBPS"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256")
:code-format (list "[rvm:" "vex.nds.256.f2.0f" "d0" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VHADDPD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x64-asm-instruction
:name "VHADDPD"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128")
:code-format (list "[rvm:" "vex.nds.128.66.0f" "7c" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VHADDPD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x64-asm-instruction
:name "VHADDPD"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256")
:code-format (list "[rvm:" "vex.nds.256.66.0f" "7c" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VHADDPS-xmmreg.xmmreg*.xmmrm128 (make-instance 'x64-asm-instruction
:name "VHADDPS"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128")
:code-format (list "[rvm:" "vex.nds.128.f2.0f" "7c" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VHADDPS-ymmreg.ymmreg*.ymmrm256 (make-instance 'x64-asm-instruction
:name "VHADDPS"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256")
:code-format (list "[rvm:" "vex.nds.256.f2.0f" "7c" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VPADDB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPADDB"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128")
:code-format (list "[rvm:" "vex.nds.128.66.0f" "fc" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VPADDW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPADDW"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128")
:code-format (list "[rvm:" "vex.nds.128.66.0f" "fd" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VPADDD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPADDD"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128")
:code-format (list "[rvm:" "vex.nds.128.66.0f" "fe" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VPADDQ-xmmreg.xmmreg*.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPADDQ"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128")
:code-format (list "[rvm:" "vex.nds.128.66.0f" "d4" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VPADDSB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPADDSB"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128")
:code-format (list "[rvm:" "vex.nds.128.66.0f" "ec" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VPADDSW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPADDSW"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128")
:code-format (list "[rvm:" "vex.nds.128.66.0f" "ed" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VPADDUSB-xmmreg.xmmreg*.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPADDUSB"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128")
:code-format (list "[rvm:" "vex.nds.128.66.0f" "dc" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VPADDUSW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPADDUSW"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128")
:code-format (list "[rvm:" "vex.nds.128.66.0f" "dd" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VPHADDW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPHADDW"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128")
:code-format (list "[rvm:" "vex.nds.128.66.0f38" "01" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VPHADDD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPHADDD"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128")
:code-format (list "[rvm:" "vex.nds.128.66.0f38" "02" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VPHADDSW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPHADDSW"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128")
:code-format (list "[rvm:" "vex.nds.128.66.0f38" "03" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VPMADDWD-xmmreg.xmmreg*.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPMADDWD"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128")
:code-format (list "[rvm:" "vex.nds.128.66.0f" "f5" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VPMADDUBSW-xmmreg.xmmreg*.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPMADDUBSW"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128")
:code-format (list "[rvm:" "vex.nds.128.66.0f38" "04" "/r")
:arch-flags (list "AVX" "SANDYBRIDGE")))

(defparameter VFMADD132PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADD132PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "98" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD132PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADD132PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "98" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD132PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADD132PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "98" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD132PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADD132PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "98" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD312PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADD312PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "98" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD312PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADD312PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "98" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD312PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADD312PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "98" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD312PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADD312PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "98" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD213PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADD213PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "a8" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD213PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADD213PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "a8" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD213PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADD213PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "a8" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD213PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADD213PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "a8" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD123PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADD123PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "a8" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD123PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADD123PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "a8" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD123PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADD123PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "a8" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD123PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADD123PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "a8" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD231PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADD231PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "b8" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD231PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADD231PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "b8" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD231PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADD231PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "b8" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD231PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADD231PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "b8" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD321PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADD321PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "b8" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD321PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADD321PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "b8" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD321PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADD321PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "b8" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD321PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADD321PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "b8" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB132PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADDSUB132PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "96" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB132PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADDSUB132PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "96" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB132PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADDSUB132PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "96" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB132PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADDSUB132PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "96" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB312PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADDSUB312PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "96" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB312PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADDSUB312PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "96" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB312PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADDSUB312PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "96" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB312PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADDSUB312PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "96" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB213PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADDSUB213PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "a6" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB213PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADDSUB213PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "a6" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB213PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADDSUB213PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "a6" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB213PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADDSUB213PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "a6" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB123PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADDSUB123PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "a6" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB123PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADDSUB123PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "a6" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB123PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADDSUB123PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "a6" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB123PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADDSUB123PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "a6" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB231PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADDSUB231PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "b6" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB231PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADDSUB231PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "b6" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB231PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADDSUB231PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "b6" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB231PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADDSUB231PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "b6" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB321PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADDSUB321PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "b6" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB321PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADDSUB321PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "b6" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB321PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADDSUB321PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "b6" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDSUB321PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADDSUB321PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "b6" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD132PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMSUBADD132PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "97" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD132PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMSUBADD132PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "97" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD132PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMSUBADD132PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "97" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD132PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMSUBADD132PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "97" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD312PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMSUBADD312PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "97" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD312PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMSUBADD312PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "97" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD312PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMSUBADD312PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "97" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD312PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMSUBADD312PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "97" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD213PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMSUBADD213PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "a7" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD213PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMSUBADD213PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "a7" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD213PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMSUBADD213PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "a7" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD213PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMSUBADD213PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "a7" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD123PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMSUBADD123PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "a7" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD123PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMSUBADD123PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "a7" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD123PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMSUBADD123PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "a7" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD123PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMSUBADD123PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "a7" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD231PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMSUBADD231PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "b7" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD231PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMSUBADD231PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "b7" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD231PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMSUBADD231PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "b7" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD231PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMSUBADD231PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "b7" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD321PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMSUBADD321PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "b7" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD321PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMSUBADD321PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "b7" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD321PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMSUBADD321PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "b7" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMSUBADD321PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMSUBADD321PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "b7" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD132PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFNMADD132PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "9c" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD132PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFNMADD132PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "9c" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD132PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFNMADD132PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "9c" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD132PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFNMADD132PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "9c" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD312PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFNMADD312PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "9c" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD312PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFNMADD312PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "9c" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD312PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFNMADD312PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "9c" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD312PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFNMADD312PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "9c" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD213PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFNMADD213PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "ac" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD213PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFNMADD213PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "ac" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD213PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFNMADD213PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "ac" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD213PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFNMADD213PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "ac" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD123PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFNMADD123PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "ac" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD123PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFNMADD123PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "ac" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD123PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFNMADD123PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "ac" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD123PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFNMADD123PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "ac" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD231PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFNMADD231PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "bc" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD231PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFNMADD231PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "bc" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD231PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFNMADD231PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "bc" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD231PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFNMADD231PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "bc" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD321PS-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFNMADD321PS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "bc" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD321PS-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFNMADD321PS"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w0" "bc" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD321PD-xmmreg.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFNMADD321PD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm128")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "bc" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD321PD-ymmreg.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFNMADD321PD"
:req-operands (list "ymmreg" "ymmreg" "ymmrm256")
:code-format (list "[rvm:" "vex.dds.256.66.0f38.w1" "bc" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD132SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x64-asm-instruction
:name "VFMADD132SS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm32")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "99" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD132SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x64-asm-instruction
:name "VFMADD132SD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm64")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "99" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD312SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x64-asm-instruction
:name "VFMADD312SS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm32")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "99" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD312SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x64-asm-instruction
:name "VFMADD312SD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm64")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "99" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD213SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x64-asm-instruction
:name "VFMADD213SS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm32")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "a9" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD213SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x64-asm-instruction
:name "VFMADD213SD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm64")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "a9" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD123SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x64-asm-instruction
:name "VFMADD123SS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm32")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "a9" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD123SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x64-asm-instruction
:name "VFMADD123SD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm64")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "a9" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD231SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x64-asm-instruction
:name "VFMADD231SS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm32")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "b9" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD231SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x64-asm-instruction
:name "VFMADD231SD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm64")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "b9" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD321SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x64-asm-instruction
:name "VFMADD321SS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm32")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "b9" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADD321SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x64-asm-instruction
:name "VFMADD321SD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm64")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "b9" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD132SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x64-asm-instruction
:name "VFNMADD132SS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm32")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "9d" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD132SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x64-asm-instruction
:name "VFNMADD132SD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm64")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "9d" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD312SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x64-asm-instruction
:name "VFNMADD312SS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm32")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "9d" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD312SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x64-asm-instruction
:name "VFNMADD312SD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm64")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "9d" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD213SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x64-asm-instruction
:name "VFNMADD213SS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm32")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "ad" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD213SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x64-asm-instruction
:name "VFNMADD213SD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm64")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "ad" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD123SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x64-asm-instruction
:name "VFNMADD123SS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm32")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "ad" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD123SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x64-asm-instruction
:name "VFNMADD123SD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm64")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "ad" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD231SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x64-asm-instruction
:name "VFNMADD231SS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm32")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "bd" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD231SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x64-asm-instruction
:name "VFNMADD231SD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm64")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "bd" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD321SS-xmmreg.xmmreg.xmmrm32 (make-instance 'x64-asm-instruction
:name "VFNMADD321SS"
:req-operands (list "xmmreg" "xmmreg" "xmmrm32")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w0" "bd" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFNMADD321SD-xmmreg.xmmreg.xmmrm64 (make-instance 'x64-asm-instruction
:name "VFNMADD321SD"
:req-operands (list "xmmreg" "xmmreg" "xmmrm64")
:code-format (list "[rvm:" "vex.dds.128.66.0f38.w1" "bd" "/r")
:arch-flags (list "FMA" "FUTURE")))

(defparameter VFMADDPD-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x64-asm-instruction
:name "VFMADDPD"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128" "xmmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l0.p1" "69" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMADDPD-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x64-asm-instruction
:name "VFMADDPD"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256" "ymmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l1.p1" "69" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMADDPD-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADDPD"
:req-operands (list "xmmreg" "xmmreg*" "xmmreg" "xmmrm128")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l0.p1" "69" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMADDPD-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADDPD"
:req-operands (list "ymmreg" "ymmreg*" "ymmreg" "ymmrm256")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l1.p1" "69" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMADDPS-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x64-asm-instruction
:name "VFMADDPS"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128" "xmmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l0.p1" "68" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMADDPS-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x64-asm-instruction
:name "VFMADDPS"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256" "ymmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l1.p1" "68" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMADDPS-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADDPS"
:req-operands (list "xmmreg" "xmmreg*" "xmmreg" "xmmrm128")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l0.p1" "68" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMADDPS-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADDPS"
:req-operands (list "ymmreg" "ymmreg*" "ymmreg" "ymmrm256")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l1.p1" "68" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMADDSD-xmmreg.xmmreg*.xmmrm64.xmmreg (make-instance 'x64-asm-instruction
:name "VFMADDSD"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm64" "xmmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l0.p1" "6b" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMADDSD-xmmreg.xmmreg*.xmmreg.xmmrm64 (make-instance 'x64-asm-instruction
:name "VFMADDSD"
:req-operands (list "xmmreg" "xmmreg*" "xmmreg" "xmmrm64")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l0.p1" "6b" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMADDSS-xmmreg.xmmreg*.xmmrm32.xmmreg (make-instance 'x64-asm-instruction
:name "VFMADDSS"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm32" "xmmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l0.p1" "6a" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMADDSS-xmmreg.xmmreg*.xmmreg.xmmrm32 (make-instance 'x64-asm-instruction
:name "VFMADDSS"
:req-operands (list "xmmreg" "xmmreg*" "xmmreg" "xmmrm32")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l0.p1" "6a" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMADDSUBPD-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x64-asm-instruction
:name "VFMADDSUBPD"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128" "xmmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l0.p1" "5d" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMADDSUBPD-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x64-asm-instruction
:name "VFMADDSUBPD"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256" "ymmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l1.p1" "5d" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMADDSUBPD-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADDSUBPD"
:req-operands (list "xmmreg" "xmmreg*" "xmmreg" "xmmrm128")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l0.p1" "5d" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMADDSUBPD-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADDSUBPD"
:req-operands (list "ymmreg" "ymmreg*" "ymmreg" "ymmrm256")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l1.p1" "5d" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMADDSUBPS-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x64-asm-instruction
:name "VFMADDSUBPS"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128" "xmmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l0.p1" "5c" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMADDSUBPS-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x64-asm-instruction
:name "VFMADDSUBPS"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256" "ymmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l1.p1" "5c" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMADDSUBPS-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMADDSUBPS"
:req-operands (list "xmmreg" "xmmreg*" "xmmreg" "xmmrm128")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l0.p1" "5c" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMADDSUBPS-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMADDSUBPS"
:req-operands (list "ymmreg" "ymmreg*" "ymmreg" "ymmrm256")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l1.p1" "5c" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMSUBADDPD-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x64-asm-instruction
:name "VFMSUBADDPD"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128" "xmmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l0.p1" "5f" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMSUBADDPD-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x64-asm-instruction
:name "VFMSUBADDPD"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256" "ymmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l1.p1" "5f" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMSUBADDPD-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMSUBADDPD"
:req-operands (list "xmmreg" "xmmreg*" "xmmreg" "xmmrm128")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l0.p1" "5f" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMSUBADDPD-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMSUBADDPD"
:req-operands (list "ymmreg" "ymmreg*" "ymmreg" "ymmrm256")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l1.p1" "5f" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMSUBADDPS-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x64-asm-instruction
:name "VFMSUBADDPS"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128" "xmmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l0.p1" "5e" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMSUBADDPS-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x64-asm-instruction
:name "VFMSUBADDPS"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256" "ymmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l1.p1" "5e" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMSUBADDPS-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFMSUBADDPS"
:req-operands (list "xmmreg" "xmmreg*" "xmmreg" "xmmrm128")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l0.p1" "5e" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFMSUBADDPS-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFMSUBADDPS"
:req-operands (list "ymmreg" "ymmreg*" "ymmreg" "ymmrm256")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l1.p1" "5e" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFNMADDPD-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x64-asm-instruction
:name "VFNMADDPD"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128" "xmmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l0.p1" "79" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFNMADDPD-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x64-asm-instruction
:name "VFNMADDPD"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256" "ymmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l1.p1" "79" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFNMADDPD-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFNMADDPD"
:req-operands (list "xmmreg" "xmmreg*" "xmmreg" "xmmrm128")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l0.p1" "79" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFNMADDPD-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFNMADDPD"
:req-operands (list "ymmreg" "ymmreg*" "ymmreg" "ymmrm256")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l1.p1" "79" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFNMADDPS-xmmreg.xmmreg*.xmmrm128.xmmreg (make-instance 'x64-asm-instruction
:name "VFNMADDPS"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm128" "xmmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l0.p1" "78" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFNMADDPS-ymmreg.ymmreg*.ymmrm256.ymmreg (make-instance 'x64-asm-instruction
:name "VFNMADDPS"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256" "ymmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l1.p1" "78" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFNMADDPS-xmmreg.xmmreg*.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VFNMADDPS"
:req-operands (list "xmmreg" "xmmreg*" "xmmreg" "xmmrm128")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l0.p1" "78" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFNMADDPS-ymmreg.ymmreg*.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VFNMADDPS"
:req-operands (list "ymmreg" "ymmreg*" "ymmreg" "ymmrm256")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l1.p1" "78" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFNMADDSD-xmmreg.xmmreg*.xmmrm64.xmmreg (make-instance 'x64-asm-instruction
:name "VFNMADDSD"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm64" "xmmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l0.p1" "7b" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFNMADDSD-xmmreg.xmmreg*.xmmreg.xmmrm64 (make-instance 'x64-asm-instruction
:name "VFNMADDSD"
:req-operands (list "xmmreg" "xmmreg*" "xmmreg" "xmmrm64")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l0.p1" "7b" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFNMADDSS-xmmreg.xmmreg*.xmmrm32.xmmreg (make-instance 'x64-asm-instruction
:name "VFNMADDSS"
:req-operands (list "xmmreg" "xmmreg*" "xmmrm32" "xmmreg")
:code-format (list "[rvms:" "vex.m3.w0.nds.l0.p1" "7a" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VFNMADDSS-xmmreg.xmmreg*.xmmreg.xmmrm32 (make-instance 'x64-asm-instruction
:name "VFNMADDSS"
:req-operands (list "xmmreg" "xmmreg*" "xmmreg" "xmmrm32")
:code-format (list "[rvsm:" "vex.m3.w1.nds.l0.p1" "7a" "/r" "/is4")
:arch-flags (list "AMD" "SSE5")))

(defparameter VPHADDBD-xmmreg.xmmrm128* (make-instance 'x64-asm-instruction
:name "VPHADDBD"
:req-operands (list "xmmreg" "xmmrm128*")
:code-format (list "[rm:" "xop.m9.w0.l0.p0" "c2" "/r")
:arch-flags (list "AMD" "SSE5")))

(defparameter VPHADDBQ-xmmreg.xmmrm128* (make-instance 'x64-asm-instruction
:name "VPHADDBQ"
:req-operands (list "xmmreg" "xmmrm128*")
:code-format (list "[rm:" "xop.m9.w0.l0.p0" "c3" "/r")
:arch-flags (list "AMD" "SSE5")))

(defparameter VPHADDBW-xmmreg.xmmrm128* (make-instance 'x64-asm-instruction
:name "VPHADDBW"
:req-operands (list "xmmreg" "xmmrm128*")
:code-format (list "[rm:" "xop.m9.w0.l0.p0" "c1" "/r")
:arch-flags (list "AMD" "SSE5")))

(defparameter VPHADDDQ-xmmreg.xmmrm128* (make-instance 'x64-asm-instruction
:name "VPHADDDQ"
:req-operands (list "xmmreg" "xmmrm128*")
:code-format (list "[rm:" "xop.m9.w0.l0.p0" "cb" "/r")
:arch-flags (list "AMD" "SSE5")))

(defparameter VPHADDUBD-xmmreg.xmmrm128* (make-instance 'x64-asm-instruction
:name "VPHADDUBD"
:req-operands (list "xmmreg" "xmmrm128*")
:code-format (list "[rm:" "xop.m9.w0.l0.p0" "d2" "/r")
:arch-flags (list "AMD" "SSE5")))

(defparameter VPHADDUBQ-xmmreg.xmmrm128* (make-instance 'x64-asm-instruction
:name "VPHADDUBQ"
:req-operands (list "xmmreg" "xmmrm128*")
:code-format (list "[rm:" "xop.m9.w0.l0.p0" "d3" "/r")
:arch-flags (list "AMD" "SSE5")))

(defparameter VPHADDUBW-xmmreg.xmmrm128* (make-instance 'x64-asm-instruction
:name "VPHADDUBW"
:req-operands (list "xmmreg" "xmmrm128*")
:code-format (list "[rm:" "xop.m9.w0.l0.p0" "d1" "/r")
:arch-flags (list "AMD" "SSE5")))

(defparameter VPHADDUDQ-xmmreg.xmmrm128* (make-instance 'x64-asm-instruction
:name "VPHADDUDQ"
:req-operands (list "xmmreg" "xmmrm128*")
:code-format (list "[rm:" "xop.m9.w0.l0.p0" "db" "/r")
:arch-flags (list "AMD" "SSE5")))

(defparameter VPHADDUWD-xmmreg.xmmrm128* (make-instance 'x64-asm-instruction
:name "VPHADDUWD"
:req-operands (list "xmmreg" "xmmrm128*")
:code-format (list "[rm:" "xop.m9.w0.l0.p0" "d6" "/r")
:arch-flags (list "AMD" "SSE5")))

(defparameter VPHADDUWQ-xmmreg.xmmrm128* (make-instance 'x64-asm-instruction
:name "VPHADDUWQ"
:req-operands (list "xmmreg" "xmmrm128*")
:code-format (list "[rm:" "xop.m9.w0.l0.p0" "d7" "/r")
:arch-flags (list "AMD" "SSE5")))

(defparameter VPHADDWD-xmmreg.xmmrm128* (make-instance 'x64-asm-instruction
:name "VPHADDWD"
:req-operands (list "xmmreg" "xmmrm128*")
:code-format (list "[rm:" "xop.m9.w0.l0.p0" "c6" "/r")
:arch-flags (list "AMD" "SSE5")))

(defparameter VPHADDWQ-xmmreg.xmmrm128* (make-instance 'x64-asm-instruction
:name "VPHADDWQ"
:req-operands (list "xmmreg" "xmmrm128*")
:code-format (list "[rm:" "xop.m9.w0.l0.p0" "c7" "/r")
:arch-flags (list "AMD" "SSE5")))

(defparameter VPADDB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPADDB"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256")
:code-format (list "[rvm:" "vex.nds.256.66.0f" "fc" "/r")
:arch-flags (list "FUTURE" "AVX2")))

(defparameter VPADDW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPADDW"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256")
:code-format (list "[rvm:" "vex.nds.256.66.0f" "fd" "/r")
:arch-flags (list "FUTURE" "AVX2")))

(defparameter VPADDD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPADDD"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256")
:code-format (list "[rvm:" "vex.nds.256.66.0f" "fe" "/r")
:arch-flags (list "FUTURE" "AVX2")))

(defparameter VPADDQ-ymmreg.ymmreg*.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPADDQ"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256")
:code-format (list "[rvm:" "vex.nds.256.66.0f" "d4" "/r")
:arch-flags (list "FUTURE" "AVX2")))

(defparameter VPADDSB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPADDSB"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256")
:code-format (list "[rvm:" "vex.nds.256.66.0f" "ec" "/r")
:arch-flags (list "FUTURE" "AVX2")))

(defparameter VPADDSW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPADDSW"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256")
:code-format (list "[rvm:" "vex.nds.256.66.0f" "ed" "/r")
:arch-flags (list "FUTURE" "AVX2")))

(defparameter VPADDUSB-ymmreg.ymmreg*.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPADDUSB"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256")
:code-format (list "[rvm:" "vex.nds.256.66.0f" "dc" "/r")
:arch-flags (list "FUTURE" "AVX2")))

(defparameter VPADDUSW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPADDUSW"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256")
:code-format (list "[rvm:" "vex.nds.256.66.0f" "dd" "/r")
:arch-flags (list "FUTURE" "AVX2")))

(defparameter VPHADDW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPHADDW"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256")
:code-format (list "[rvm:" "vex.nds.256.66.0f38" "01" "/r")
:arch-flags (list "FUTURE" "AVX2")))

(defparameter VPHADDD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPHADDD"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256")
:code-format (list "[rvm:" "vex.nds.256.66.0f38" "02" "/r")
:arch-flags (list "FUTURE" "AVX2")))

(defparameter VPHADDSW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPHADDSW"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256")
:code-format (list "[rvm:" "vex.nds.256.66.0f38" "03" "/r")
:arch-flags (list "FUTURE" "AVX2")))

(defparameter VPMADDUBSW-ymmreg.ymmreg*.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPMADDUBSW"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256")
:code-format (list "[rvm:" "vex.nds.256.66.0f38" "04" "/r")
:arch-flags (list "FUTURE" "AVX2")))

(defparameter VPMADDWD-ymmreg.ymmreg*.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPMADDWD"
:req-operands (list "ymmreg" "ymmreg*" "ymmrm256")
:code-format (list "[rvm:" "vex.nds.256.66.0f" "f5" "/r")
:arch-flags (list "FUTURE" "AVX2")))

(defparameter KADDB-kreg.kreg.kreg (make-instance 'x64-asm-instruction
:name "KADDB"
:req-operands (list "kreg" "kreg" "kreg")
:code-format (list "[rvm:" "vex.nds.l1.66.0f.w0" "4a" "/r" "")
:arch-flags (list "FUTURE")))

(defparameter KADDD-kreg.kreg.kreg (make-instance 'x64-asm-instruction
:name "KADDD"
:req-operands (list "kreg" "kreg" "kreg")
:code-format (list "[rvm:" "vex.nds.l1.66.0f.w1" "4a" "/r" "")
:arch-flags (list "FUTURE")))

(defparameter KADDQ-kreg.kreg.kreg (make-instance 'x64-asm-instruction
:name "KADDQ"
:req-operands (list "kreg" "kreg" "kreg")
:code-format (list "[rvm:" "vex.nds.l1.0f.w1" "4a" "/r" "")
:arch-flags (list "FUTURE")))

(defparameter KADDW-kreg.kreg.kreg (make-instance 'x64-asm-instruction
:name "KADDW"
:req-operands (list "kreg" "kreg" "kreg")
:code-format (list "[rvm:" "vex.nds.l1.0f.w0" "4a" "/r" "")
:arch-flags (list "FUTURE")))

(defparameter VADDPD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x64-asm-instruction
:name "VADDPD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b64")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f.w1" "58" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VADDPD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x64-asm-instruction
:name "VADDPD"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b64")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f.w1" "58" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VADDPD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x64-asm-instruction
:name "VADDPD"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b64|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f.w1" "58" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VADDPS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x64-asm-instruction
:name "VADDPS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b32")
:code-format (list "[rvm:fv:" "evex.nds.128.0f.w0" "58" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VADDPS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x64-asm-instruction
:name "VADDPS"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b32")
:code-format (list "[rvm:fv:" "evex.nds.256.0f.w0" "58" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VADDPS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x64-asm-instruction
:name "VADDPS"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b32|er")
:code-format (list "[rvm:fv:" "evex.nds.512.0f.w0" "58" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VADDSD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x64-asm-instruction
:name "VADDSD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm64|er")
:code-format (list "[rvm:t1s:" "evex.nds.128.f2.0f.w1" "58" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VADDSS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x64-asm-instruction
:name "VADDSS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm32|er")
:code-format (list "[rvm:t1s:" "evex.nds.128.f3.0f.w0" "58" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMADD132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x64-asm-instruction
:name "VFMADD132PD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b64")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w1" "98" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADD132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x64-asm-instruction
:name "VFMADD132PD"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b64")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w1" "98" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADD132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x64-asm-instruction
:name "VFMADD132PD"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b64|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w1" "98" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMADD132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x64-asm-instruction
:name "VFMADD132PS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b32")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w0" "98" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADD132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x64-asm-instruction
:name "VFMADD132PS"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b32")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w0" "98" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADD132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x64-asm-instruction
:name "VFMADD132PS"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b32|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w0" "98" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMADD132SD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x64-asm-instruction
:name "VFMADD132SD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm64|er")
:code-format (list "[rvm:t1s:" "evex.nds.128.66.0f38.w1" "99" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMADD132SS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x64-asm-instruction
:name "VFMADD132SS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm32|er")
:code-format (list "[rvm:t1s:" "evex.nds.128.66.0f38.w0" "99" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMADD213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x64-asm-instruction
:name "VFMADD213PD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b64")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w1" "a8" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADD213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x64-asm-instruction
:name "VFMADD213PD"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b64")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w1" "a8" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADD213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x64-asm-instruction
:name "VFMADD213PD"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b64|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w1" "a8" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMADD213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x64-asm-instruction
:name "VFMADD213PS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b32")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w0" "a8" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADD213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x64-asm-instruction
:name "VFMADD213PS"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b32")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w0" "a8" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADD213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x64-asm-instruction
:name "VFMADD213PS"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b32|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w0" "a8" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMADD213SD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x64-asm-instruction
:name "VFMADD213SD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm64|er")
:code-format (list "[rvm:t1s:" "evex.nds.128.66.0f38.w1" "a9" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMADD213SS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x64-asm-instruction
:name "VFMADD213SS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm32|er")
:code-format (list "[rvm:t1s:" "evex.nds.128.66.0f38.w0" "a9" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMADD231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x64-asm-instruction
:name "VFMADD231PD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b64")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w1" "b8" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADD231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x64-asm-instruction
:name "VFMADD231PD"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b64")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w1" "b8" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADD231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x64-asm-instruction
:name "VFMADD231PD"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b64|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w1" "b8" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMADD231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x64-asm-instruction
:name "VFMADD231PS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b32")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w0" "b8" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADD231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x64-asm-instruction
:name "VFMADD231PS"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b32")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w0" "b8" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADD231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x64-asm-instruction
:name "VFMADD231PS"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b32|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w0" "b8" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMADD231SD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x64-asm-instruction
:name "VFMADD231SD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm64|er")
:code-format (list "[rvm:t1s:" "evex.nds.128.66.0f38.w1" "b9" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMADD231SS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x64-asm-instruction
:name "VFMADD231SS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm32|er")
:code-format (list "[rvm:t1s:" "evex.nds.128.66.0f38.w0" "b9" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMADDSUB132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x64-asm-instruction
:name "VFMADDSUB132PD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b64")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w1" "96" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADDSUB132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x64-asm-instruction
:name "VFMADDSUB132PD"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b64")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w1" "96" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADDSUB132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x64-asm-instruction
:name "VFMADDSUB132PD"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b64|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w1" "96" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMADDSUB132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x64-asm-instruction
:name "VFMADDSUB132PS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b32")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w0" "96" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADDSUB132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x64-asm-instruction
:name "VFMADDSUB132PS"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b32")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w0" "96" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADDSUB132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x64-asm-instruction
:name "VFMADDSUB132PS"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b32|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w0" "96" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMADDSUB213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x64-asm-instruction
:name "VFMADDSUB213PD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b64")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w1" "a6" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADDSUB213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x64-asm-instruction
:name "VFMADDSUB213PD"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b64")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w1" "a6" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADDSUB213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x64-asm-instruction
:name "VFMADDSUB213PD"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b64|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w1" "a6" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMADDSUB213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x64-asm-instruction
:name "VFMADDSUB213PS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b32")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w0" "a6" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADDSUB213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x64-asm-instruction
:name "VFMADDSUB213PS"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b32")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w0" "a6" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADDSUB213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x64-asm-instruction
:name "VFMADDSUB213PS"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b32|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w0" "a6" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMADDSUB231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x64-asm-instruction
:name "VFMADDSUB231PD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b64")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w1" "b6" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADDSUB231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x64-asm-instruction
:name "VFMADDSUB231PD"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b64")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w1" "b6" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADDSUB231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x64-asm-instruction
:name "VFMADDSUB231PD"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b64|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w1" "b6" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMADDSUB231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x64-asm-instruction
:name "VFMADDSUB231PS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b32")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w0" "b6" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADDSUB231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x64-asm-instruction
:name "VFMADDSUB231PS"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b32")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w0" "b6" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMADDSUB231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x64-asm-instruction
:name "VFMADDSUB231PS"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b32|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w0" "b6" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMSUBADD132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x64-asm-instruction
:name "VFMSUBADD132PD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b64")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w1" "97" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMSUBADD132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x64-asm-instruction
:name "VFMSUBADD132PD"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b64")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w1" "97" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMSUBADD132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x64-asm-instruction
:name "VFMSUBADD132PD"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b64|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w1" "97" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMSUBADD132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x64-asm-instruction
:name "VFMSUBADD132PS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b32")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w0" "97" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMSUBADD132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x64-asm-instruction
:name "VFMSUBADD132PS"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b32")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w0" "97" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMSUBADD132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x64-asm-instruction
:name "VFMSUBADD132PS"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b32|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w0" "97" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMSUBADD213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x64-asm-instruction
:name "VFMSUBADD213PD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b64")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w1" "a7" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMSUBADD213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x64-asm-instruction
:name "VFMSUBADD213PD"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b64")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w1" "a7" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMSUBADD213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x64-asm-instruction
:name "VFMSUBADD213PD"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b64|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w1" "a7" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMSUBADD213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x64-asm-instruction
:name "VFMSUBADD213PS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b32")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w0" "a7" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMSUBADD213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x64-asm-instruction
:name "VFMSUBADD213PS"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b32")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w0" "a7" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMSUBADD213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x64-asm-instruction
:name "VFMSUBADD213PS"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b32|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w0" "a7" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMSUBADD231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x64-asm-instruction
:name "VFMSUBADD231PD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b64")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w1" "b7" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMSUBADD231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x64-asm-instruction
:name "VFMSUBADD231PD"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b64")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w1" "b7" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMSUBADD231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x64-asm-instruction
:name "VFMSUBADD231PD"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b64|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w1" "b7" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFMSUBADD231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x64-asm-instruction
:name "VFMSUBADD231PS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b32")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w0" "b7" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMSUBADD231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x64-asm-instruction
:name "VFMSUBADD231PS"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b32")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w0" "b7" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFMSUBADD231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x64-asm-instruction
:name "VFMSUBADD231PS"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b32|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w0" "b7" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFNMADD132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x64-asm-instruction
:name "VFNMADD132PD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b64")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w1" "9c" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFNMADD132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x64-asm-instruction
:name "VFNMADD132PD"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b64")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w1" "9c" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFNMADD132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x64-asm-instruction
:name "VFNMADD132PD"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b64|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w1" "9c" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFNMADD132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x64-asm-instruction
:name "VFNMADD132PS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b32")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w0" "9c" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFNMADD132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x64-asm-instruction
:name "VFNMADD132PS"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b32")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w0" "9c" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFNMADD132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x64-asm-instruction
:name "VFNMADD132PS"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b32|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w0" "9c" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFNMADD132SD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x64-asm-instruction
:name "VFNMADD132SD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm64|er")
:code-format (list "[rvm:t1s:" "evex.nds.128.66.0f38.w1" "9d" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFNMADD132SS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x64-asm-instruction
:name "VFNMADD132SS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm32|er")
:code-format (list "[rvm:t1s:" "evex.nds.128.66.0f38.w0" "9d" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFNMADD213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x64-asm-instruction
:name "VFNMADD213PD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b64")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w1" "ac" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFNMADD213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x64-asm-instruction
:name "VFNMADD213PD"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b64")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w1" "ac" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFNMADD213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x64-asm-instruction
:name "VFNMADD213PD"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b64|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w1" "ac" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFNMADD213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x64-asm-instruction
:name "VFNMADD213PS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b32")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w0" "ac" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFNMADD213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x64-asm-instruction
:name "VFNMADD213PS"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b32")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w0" "ac" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFNMADD213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x64-asm-instruction
:name "VFNMADD213PS"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b32|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w0" "ac" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFNMADD213SD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x64-asm-instruction
:name "VFNMADD213SD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm64|er")
:code-format (list "[rvm:t1s:" "evex.nds.128.66.0f38.w1" "ad" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFNMADD213SS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x64-asm-instruction
:name "VFNMADD213SS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm32|er")
:code-format (list "[rvm:t1s:" "evex.nds.128.66.0f38.w0" "ad" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFNMADD231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x64-asm-instruction
:name "VFNMADD231PD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b64")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w1" "bc" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFNMADD231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x64-asm-instruction
:name "VFNMADD231PD"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b64")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w1" "bc" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFNMADD231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er (make-instance 'x64-asm-instruction
:name "VFNMADD231PD"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b64|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w1" "bc" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFNMADD231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x64-asm-instruction
:name "VFNMADD231PS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b32")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w0" "bc" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFNMADD231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x64-asm-instruction
:name "VFNMADD231PS"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b32")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w0" "bc" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VFNMADD231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er (make-instance 'x64-asm-instruction
:name "VFNMADD231PS"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b32|er")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w0" "bc" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFNMADD231SD-xmmreg-mask-z.xmmreg.xmmrm64-er (make-instance 'x64-asm-instruction
:name "VFNMADD231SD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm64|er")
:code-format (list "[rvm:t1s:" "evex.nds.128.66.0f38.w1" "bd" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VFNMADD231SS-xmmreg-mask-z.xmmreg.xmmrm32-er (make-instance 'x64-asm-instruction
:name "VFNMADD231SS"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm32|er")
:code-format (list "[rvm:t1s:" "evex.nds.128.66.0f38.w0" "bd" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VPADDB-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPADDB"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128")
:code-format (list "[rvm:fvm:" "evex.nds.128.66.0f.wig" "fc" "/r" "")
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(defparameter VPADDB-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPADDB"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256")
:code-format (list "[rvm:fvm:" "evex.nds.256.66.0f.wig" "fc" "/r" "")
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(defparameter VPADDB-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x64-asm-instruction
:name "VPADDB"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512")
:code-format (list "[rvm:fvm:" "evex.nds.512.66.0f.wig" "fc" "/r" "")
:arch-flags (list "AVX512BW" "FUTURE")))

(defparameter VPADDD-xmmreg-mask-z.xmmreg.xmmrm128-b32 (make-instance 'x64-asm-instruction
:name "VPADDD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b32")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f.w0" "fe" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VPADDD-ymmreg-mask-z.ymmreg.ymmrm256-b32 (make-instance 'x64-asm-instruction
:name "VPADDD"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b32")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f.w0" "fe" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VPADDD-zmmreg-mask-z.zmmreg.zmmrm512-b32 (make-instance 'x64-asm-instruction
:name "VPADDD"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b32")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f.w0" "fe" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VPADDQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x64-asm-instruction
:name "VPADDQ"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b64")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f.w1" "d4" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VPADDQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x64-asm-instruction
:name "VPADDQ"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b64")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f.w1" "d4" "/r" "")
:arch-flags (list "AVX512VL" "AVX512" "FUTURE")))

(defparameter VPADDQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x64-asm-instruction
:name "VPADDQ"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b64")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f.w1" "d4" "/r" "")
:arch-flags (list "AVX512" "FUTURE")))

(defparameter VPADDSB-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPADDSB"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128")
:code-format (list "[rvm:fvm:" "evex.nds.128.66.0f.wig" "ec" "/r" "")
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(defparameter VPADDSB-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPADDSB"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256")
:code-format (list "[rvm:fvm:" "evex.nds.256.66.0f.wig" "ec" "/r" "")
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(defparameter VPADDSB-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x64-asm-instruction
:name "VPADDSB"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512")
:code-format (list "[rvm:fvm:" "evex.nds.512.66.0f.wig" "ec" "/r" "")
:arch-flags (list "AVX512BW" "FUTURE")))

(defparameter VPADDSW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPADDSW"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128")
:code-format (list "[rvm:fvm:" "evex.nds.128.66.0f.wig" "ed" "/r" "")
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(defparameter VPADDSW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPADDSW"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256")
:code-format (list "[rvm:fvm:" "evex.nds.256.66.0f.wig" "ed" "/r" "")
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(defparameter VPADDSW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x64-asm-instruction
:name "VPADDSW"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512")
:code-format (list "[rvm:fvm:" "evex.nds.512.66.0f.wig" "ed" "/r" "")
:arch-flags (list "AVX512BW" "FUTURE")))

(defparameter VPADDUSB-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPADDUSB"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128")
:code-format (list "[rvm:fvm:" "evex.nds.128.66.0f.wig" "dc" "/r" "")
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(defparameter VPADDUSB-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPADDUSB"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256")
:code-format (list "[rvm:fvm:" "evex.nds.256.66.0f.wig" "dc" "/r" "")
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(defparameter VPADDUSB-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x64-asm-instruction
:name "VPADDUSB"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512")
:code-format (list "[rvm:fvm:" "evex.nds.512.66.0f.wig" "dc" "/r" "")
:arch-flags (list "AVX512BW" "FUTURE")))

(defparameter VPADDUSW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPADDUSW"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128")
:code-format (list "[rvm:fvm:" "evex.nds.128.66.0f.wig" "dd" "/r" "")
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(defparameter VPADDUSW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPADDUSW"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256")
:code-format (list "[rvm:fvm:" "evex.nds.256.66.0f.wig" "dd" "/r" "")
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(defparameter VPADDUSW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x64-asm-instruction
:name "VPADDUSW"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512")
:code-format (list "[rvm:fvm:" "evex.nds.512.66.0f.wig" "dd" "/r" "")
:arch-flags (list "AVX512BW" "FUTURE")))

(defparameter VPADDW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPADDW"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128")
:code-format (list "[rvm:fvm:" "evex.nds.128.66.0f.wig" "fd" "/r" "")
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(defparameter VPADDW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPADDW"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256")
:code-format (list "[rvm:fvm:" "evex.nds.256.66.0f.wig" "fd" "/r" "")
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(defparameter VPADDW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x64-asm-instruction
:name "VPADDW"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512")
:code-format (list "[rvm:fvm:" "evex.nds.512.66.0f.wig" "fd" "/r" "")
:arch-flags (list "AVX512BW" "FUTURE")))

(defparameter VPMADD52HUQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x64-asm-instruction
:name "VPMADD52HUQ"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b64")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w1" "b5" "/r" "")
:arch-flags (list "AVX512VL" "AVX512IFMA" "FUTURE")))

(defparameter VPMADD52HUQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x64-asm-instruction
:name "VPMADD52HUQ"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b64")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w1" "b5" "/r" "")
:arch-flags (list "AVX512VL" "AVX512IFMA" "FUTURE")))

(defparameter VPMADD52HUQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x64-asm-instruction
:name "VPMADD52HUQ"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b64")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w1" "b5" "/r" "")
:arch-flags (list "AVX512IFMA" "FUTURE")))

(defparameter VPMADD52LUQ-xmmreg-mask-z.xmmreg.xmmrm128-b64 (make-instance 'x64-asm-instruction
:name "VPMADD52LUQ"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128|b64")
:code-format (list "[rvm:fv:" "evex.nds.128.66.0f38.w1" "b4" "/r" "")
:arch-flags (list "AVX512VL" "AVX512IFMA" "FUTURE")))

(defparameter VPMADD52LUQ-ymmreg-mask-z.ymmreg.ymmrm256-b64 (make-instance 'x64-asm-instruction
:name "VPMADD52LUQ"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256|b64")
:code-format (list "[rvm:fv:" "evex.nds.256.66.0f38.w1" "b4" "/r" "")
:arch-flags (list "AVX512VL" "AVX512IFMA" "FUTURE")))

(defparameter VPMADD52LUQ-zmmreg-mask-z.zmmreg.zmmrm512-b64 (make-instance 'x64-asm-instruction
:name "VPMADD52LUQ"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512|b64")
:code-format (list "[rvm:fv:" "evex.nds.512.66.0f38.w1" "b4" "/r" "")
:arch-flags (list "AVX512IFMA" "FUTURE")))

(defparameter VPMADDUBSW-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPMADDUBSW"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128")
:code-format (list "[rvm:fvm:" "evex.nds.128.66.0f38.wig" "04" "/r" "")
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(defparameter VPMADDUBSW-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPMADDUBSW"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256")
:code-format (list "[rvm:fvm:" "evex.nds.256.66.0f38.wig" "04" "/r" "")
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(defparameter VPMADDUBSW-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x64-asm-instruction
:name "VPMADDUBSW"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512")
:code-format (list "[rvm:fvm:" "evex.nds.512.66.0f38.wig" "04" "/r" "")
:arch-flags (list "AVX512BW" "FUTURE")))

(defparameter VPMADDWD-xmmreg-mask-z.xmmreg.xmmrm128 (make-instance 'x64-asm-instruction
:name "VPMADDWD"
:req-operands (list "xmmreg|mask|z" "xmmreg" "xmmrm128")
:code-format (list "[rvm:fvm:" "evex.nds.128.66.0f.wig" "f5" "/r" "")
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(defparameter VPMADDWD-ymmreg-mask-z.ymmreg.ymmrm256 (make-instance 'x64-asm-instruction
:name "VPMADDWD"
:req-operands (list "ymmreg|mask|z" "ymmreg" "ymmrm256")
:code-format (list "[rvm:fvm:" "evex.nds.256.66.0f.wig" "f5" "/r" "")
:arch-flags (list "AVX512VL" "AVX512BW" "FUTURE")))

(defparameter VPMADDWD-zmmreg-mask-z.zmmreg.zmmrm512 (make-instance 'x64-asm-instruction
:name "VPMADDWD"
:req-operands (list "zmmreg|mask|z" "zmmreg" "zmmrm512")
:code-format (list "[rvm:fvm:" "evex.nds.512.66.0f.wig" "f5" "/r" "")
:arch-flags (list "AVX512BW" "FUTURE")))

(setf (gethash "ADD" *x64-instruction-variants-hash-table*) (list
ADD-mem.reg8
ADD-reg8.reg8-mr
ADD-mem.reg16
ADD-reg16.reg16-mr
ADD-mem.reg32
ADD-reg32.reg32-mr
ADD-mem.reg64
ADD-reg64.reg64-mr
ADD-reg8.mem
ADD-reg8.reg8-rm
ADD-reg16.mem
ADD-reg16.reg16-rm
ADD-reg32.mem
ADD-reg32.reg32-rm
ADD-reg64.mem
ADD-reg64.reg64-rm
ADD-rm16.imm8
ADD-rm32.imm8
ADD-rm64.imm8
ADD-reg_al.imm
ADD-reg_ax.sbyteword
ADD-reg_ax.imm
ADD-reg_eax.sbytedword
ADD-reg_eax.imm
ADD-reg_rax.sbytedword
ADD-reg_rax.imm
ADD-rm8.imm
ADD-rm16.sbyteword
ADD-rm16.imm
ADD-rm32.sbytedword
ADD-rm32.imm
ADD-rm64.sbytedword
ADD-rm64.imm
ADD-mem.imm8
ADD-mem.sbyteword16
ADD-mem.imm16
ADD-mem.sbytedword32
ADD-mem.imm32))

(setf (gethash "ADD-mem.imm16" *x64-instruction-variants-hash-table*) (list
ADD-mem.imm16))

(setf (gethash "ADD-mem.imm32" *x64-instruction-variants-hash-table*) (list
ADD-mem.imm32))

(setf (gethash "ADD-mem.imm8" *x64-instruction-variants-hash-table*) (list
ADD-mem.imm8))

(setf (gethash "ADD-mem.reg16" *x64-instruction-variants-hash-table*) (list
ADD-mem.reg16))

(setf (gethash "ADD-mem.reg32" *x64-instruction-variants-hash-table*) (list
ADD-mem.reg32))

(setf (gethash "ADD-mem.reg64" *x64-instruction-variants-hash-table*) (list
ADD-mem.reg64))

(setf (gethash "ADD-mem.reg8" *x64-instruction-variants-hash-table*) (list
ADD-mem.reg8))

(setf (gethash "ADD-mem.sbytedword32" *x64-instruction-variants-hash-table*) (list
ADD-mem.sbytedword32))

(setf (gethash "ADD-mem.sbyteword16" *x64-instruction-variants-hash-table*) (list
ADD-mem.sbyteword16))

(setf (gethash "ADD-reg16.mem" *x64-instruction-variants-hash-table*) (list
ADD-reg16.mem))

(setf (gethash "ADD-reg16.reg16-mr" *x64-instruction-variants-hash-table*) (list
ADD-reg16.reg16-mr))

(setf (gethash "ADD-reg16.reg16-rm" *x64-instruction-variants-hash-table*) (list
ADD-reg16.reg16-rm))

(setf (gethash "ADD-reg32.mem" *x64-instruction-variants-hash-table*) (list
ADD-reg32.mem))

(setf (gethash "ADD-reg32.reg32-mr" *x64-instruction-variants-hash-table*) (list
ADD-reg32.reg32-mr))

(setf (gethash "ADD-reg32.reg32-rm" *x64-instruction-variants-hash-table*) (list
ADD-reg32.reg32-rm))

(setf (gethash "ADD-reg64.mem" *x64-instruction-variants-hash-table*) (list
ADD-reg64.mem))

(setf (gethash "ADD-reg64.reg64-mr" *x64-instruction-variants-hash-table*) (list
ADD-reg64.reg64-mr))

(setf (gethash "ADD-reg64.reg64-rm" *x64-instruction-variants-hash-table*) (list
ADD-reg64.reg64-rm))

(setf (gethash "ADD-reg8.mem" *x64-instruction-variants-hash-table*) (list
ADD-reg8.mem))

(setf (gethash "ADD-reg8.reg8-mr" *x64-instruction-variants-hash-table*) (list
ADD-reg8.reg8-mr))

(setf (gethash "ADD-reg8.reg8-rm" *x64-instruction-variants-hash-table*) (list
ADD-reg8.reg8-rm))

(setf (gethash "ADD-reg_al.imm" *x64-instruction-variants-hash-table*) (list
ADD-reg_al.imm))

(setf (gethash "ADD-reg_ax.imm" *x64-instruction-variants-hash-table*) (list
ADD-reg_ax.imm))

(setf (gethash "ADD-reg_ax.sbyteword" *x64-instruction-variants-hash-table*) (list
ADD-reg_ax.sbyteword))

(setf (gethash "ADD-reg_eax.imm" *x64-instruction-variants-hash-table*) (list
ADD-reg_eax.imm))

(setf (gethash "ADD-reg_eax.sbytedword" *x64-instruction-variants-hash-table*) (list
ADD-reg_eax.sbytedword))

(setf (gethash "ADD-reg_rax.imm" *x64-instruction-variants-hash-table*) (list
ADD-reg_rax.imm))

(setf (gethash "ADD-reg_rax.sbytedword" *x64-instruction-variants-hash-table*) (list
ADD-reg_rax.sbytedword))

(setf (gethash "ADD-rm16.imm" *x64-instruction-variants-hash-table*) (list
ADD-rm16.imm))

(setf (gethash "ADD-rm16.imm8" *x64-instruction-variants-hash-table*) (list
ADD-rm16.imm8))

(setf (gethash "ADD-rm16.sbyteword" *x64-instruction-variants-hash-table*) (list
ADD-rm16.sbyteword))

(setf (gethash "ADD-rm32.imm" *x64-instruction-variants-hash-table*) (list
ADD-rm32.imm))

(setf (gethash "ADD-rm32.imm8" *x64-instruction-variants-hash-table*) (list
ADD-rm32.imm8))

(setf (gethash "ADD-rm32.sbytedword" *x64-instruction-variants-hash-table*) (list
ADD-rm32.sbytedword))

(setf (gethash "ADD-rm64.imm" *x64-instruction-variants-hash-table*) (list
ADD-rm64.imm))

(setf (gethash "ADD-rm64.imm8" *x64-instruction-variants-hash-table*) (list
ADD-rm64.imm8))

(setf (gethash "ADD-rm64.sbytedword" *x64-instruction-variants-hash-table*) (list
ADD-rm64.sbytedword))

(setf (gethash "ADD-rm8.imm" *x64-instruction-variants-hash-table*) (list
ADD-rm8.imm))

(setf (gethash "ADDPD" *x64-instruction-variants-hash-table*) (list
ADDPD-xmmreg.xmmrm))

(setf (gethash "ADDPD-xmmreg.xmmrm" *x64-instruction-variants-hash-table*) (list
ADDPD-xmmreg.xmmrm))

(setf (gethash "ADDPS" *x64-instruction-variants-hash-table*) (list
ADDPS-xmmreg.xmmrm128))

(setf (gethash "ADDPS-xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
ADDPS-xmmreg.xmmrm128))

(setf (gethash "ADDSD" *x64-instruction-variants-hash-table*) (list
ADDSD-xmmreg.xmmrm))

(setf (gethash "ADDSD-xmmreg.xmmrm" *x64-instruction-variants-hash-table*) (list
ADDSD-xmmreg.xmmrm))

(setf (gethash "ADDSS" *x64-instruction-variants-hash-table*) (list
ADDSS-xmmreg.xmmrm32))

(setf (gethash "ADDSS-xmmreg.xmmrm32" *x64-instruction-variants-hash-table*) (list
ADDSS-xmmreg.xmmrm32))

(setf (gethash "ADDSUBPD" *x64-instruction-variants-hash-table*) (list
ADDSUBPD-xmmreg.xmmrm))

(setf (gethash "ADDSUBPD-xmmreg.xmmrm" *x64-instruction-variants-hash-table*) (list
ADDSUBPD-xmmreg.xmmrm))

(setf (gethash "ADDSUBPS" *x64-instruction-variants-hash-table*) (list
ADDSUBPS-xmmreg.xmmrm))

(setf (gethash "ADDSUBPS-xmmreg.xmmrm" *x64-instruction-variants-hash-table*) (list
ADDSUBPS-xmmreg.xmmrm))

(setf (gethash "FADD" *x64-instruction-variants-hash-table*) (list
FADD-mem32
FADD-mem64
FADD-fpureg-to
FADD-fpureg
FADD-fpureg.fpu0
FADD-fpu0.fpureg
FADD-void))

(setf (gethash "FADD-fpu0.fpureg" *x64-instruction-variants-hash-table*) (list
FADD-fpu0.fpureg))

(setf (gethash "FADD-fpureg" *x64-instruction-variants-hash-table*) (list
FADD-fpureg))

(setf (gethash "FADD-fpureg-to" *x64-instruction-variants-hash-table*) (list
FADD-fpureg-to))

(setf (gethash "FADD-fpureg.fpu0" *x64-instruction-variants-hash-table*) (list
FADD-fpureg.fpu0))

(setf (gethash "FADD-mem32" *x64-instruction-variants-hash-table*) (list
FADD-mem32))

(setf (gethash "FADD-mem64" *x64-instruction-variants-hash-table*) (list
FADD-mem64))

(setf (gethash "FADD-void" *x64-instruction-variants-hash-table*) (list
FADD-void))

(setf (gethash "FADDP" *x64-instruction-variants-hash-table*) (list
FADDP-fpureg
FADDP-fpureg.fpu0
FADDP-void))

(setf (gethash "FADDP-fpureg" *x64-instruction-variants-hash-table*) (list
FADDP-fpureg))

(setf (gethash "FADDP-fpureg.fpu0" *x64-instruction-variants-hash-table*) (list
FADDP-fpureg.fpu0))

(setf (gethash "FADDP-void" *x64-instruction-variants-hash-table*) (list
FADDP-void))

(setf (gethash "FIADD" *x64-instruction-variants-hash-table*) (list
FIADD-mem32
FIADD-mem16))

(setf (gethash "FIADD-mem16" *x64-instruction-variants-hash-table*) (list
FIADD-mem16))

(setf (gethash "FIADD-mem32" *x64-instruction-variants-hash-table*) (list
FIADD-mem32))

(setf (gethash "HADDPD" *x64-instruction-variants-hash-table*) (list
HADDPD-xmmreg.xmmrm))

(setf (gethash "HADDPD-xmmreg.xmmrm" *x64-instruction-variants-hash-table*) (list
HADDPD-xmmreg.xmmrm))

(setf (gethash "HADDPS" *x64-instruction-variants-hash-table*) (list
HADDPS-xmmreg.xmmrm))

(setf (gethash "HADDPS-xmmreg.xmmrm" *x64-instruction-variants-hash-table*) (list
HADDPS-xmmreg.xmmrm))

(setf (gethash "KADDB" *x64-instruction-variants-hash-table*) (list
KADDB-kreg.kreg.kreg))

(setf (gethash "KADDB-kreg.kreg.kreg" *x64-instruction-variants-hash-table*) (list
KADDB-kreg.kreg.kreg))

(setf (gethash "KADDD" *x64-instruction-variants-hash-table*) (list
KADDD-kreg.kreg.kreg))

(setf (gethash "KADDD-kreg.kreg.kreg" *x64-instruction-variants-hash-table*) (list
KADDD-kreg.kreg.kreg))

(setf (gethash "KADDQ" *x64-instruction-variants-hash-table*) (list
KADDQ-kreg.kreg.kreg))

(setf (gethash "KADDQ-kreg.kreg.kreg" *x64-instruction-variants-hash-table*) (list
KADDQ-kreg.kreg.kreg))

(setf (gethash "KADDW" *x64-instruction-variants-hash-table*) (list
KADDW-kreg.kreg.kreg))

(setf (gethash "KADDW-kreg.kreg.kreg" *x64-instruction-variants-hash-table*) (list
KADDW-kreg.kreg.kreg))

(setf (gethash "PADDB" *x64-instruction-variants-hash-table*) (list
PADDB-mmxreg.mmxrm
PADDB-xmmreg.xmmrm))

(setf (gethash "PADDB-mmxreg.mmxrm" *x64-instruction-variants-hash-table*) (list
PADDB-mmxreg.mmxrm))

(setf (gethash "PADDB-xmmreg.xmmrm" *x64-instruction-variants-hash-table*) (list
PADDB-xmmreg.xmmrm))

(setf (gethash "PADDD" *x64-instruction-variants-hash-table*) (list
PADDD-mmxreg.mmxrm
PADDD-xmmreg.xmmrm))

(setf (gethash "PADDD-mmxreg.mmxrm" *x64-instruction-variants-hash-table*) (list
PADDD-mmxreg.mmxrm))

(setf (gethash "PADDD-xmmreg.xmmrm" *x64-instruction-variants-hash-table*) (list
PADDD-xmmreg.xmmrm))

(setf (gethash "PADDQ" *x64-instruction-variants-hash-table*) (list
PADDQ-mmxreg.mmxrm
PADDQ-xmmreg.xmmrm))

(setf (gethash "PADDQ-mmxreg.mmxrm" *x64-instruction-variants-hash-table*) (list
PADDQ-mmxreg.mmxrm))

(setf (gethash "PADDQ-xmmreg.xmmrm" *x64-instruction-variants-hash-table*) (list
PADDQ-xmmreg.xmmrm))

(setf (gethash "PADDSB" *x64-instruction-variants-hash-table*) (list
PADDSB-mmxreg.mmxrm
PADDSB-xmmreg.xmmrm))

(setf (gethash "PADDSB-mmxreg.mmxrm" *x64-instruction-variants-hash-table*) (list
PADDSB-mmxreg.mmxrm))

(setf (gethash "PADDSB-xmmreg.xmmrm" *x64-instruction-variants-hash-table*) (list
PADDSB-xmmreg.xmmrm))

(setf (gethash "PADDSIW" *x64-instruction-variants-hash-table*) (list
PADDSIW-mmxreg.mmxrm))

(setf (gethash "PADDSIW-mmxreg.mmxrm" *x64-instruction-variants-hash-table*) (list
PADDSIW-mmxreg.mmxrm))

(setf (gethash "PADDSW" *x64-instruction-variants-hash-table*) (list
PADDSW-mmxreg.mmxrm
PADDSW-xmmreg.xmmrm))

(setf (gethash "PADDSW-mmxreg.mmxrm" *x64-instruction-variants-hash-table*) (list
PADDSW-mmxreg.mmxrm))

(setf (gethash "PADDSW-xmmreg.xmmrm" *x64-instruction-variants-hash-table*) (list
PADDSW-xmmreg.xmmrm))

(setf (gethash "PADDUSB" *x64-instruction-variants-hash-table*) (list
PADDUSB-mmxreg.mmxrm
PADDUSB-xmmreg.xmmrm))

(setf (gethash "PADDUSB-mmxreg.mmxrm" *x64-instruction-variants-hash-table*) (list
PADDUSB-mmxreg.mmxrm))

(setf (gethash "PADDUSB-xmmreg.xmmrm" *x64-instruction-variants-hash-table*) (list
PADDUSB-xmmreg.xmmrm))

(setf (gethash "PADDUSW" *x64-instruction-variants-hash-table*) (list
PADDUSW-mmxreg.mmxrm
PADDUSW-xmmreg.xmmrm))

(setf (gethash "PADDUSW-mmxreg.mmxrm" *x64-instruction-variants-hash-table*) (list
PADDUSW-mmxreg.mmxrm))

(setf (gethash "PADDUSW-xmmreg.xmmrm" *x64-instruction-variants-hash-table*) (list
PADDUSW-xmmreg.xmmrm))

(setf (gethash "PADDW" *x64-instruction-variants-hash-table*) (list
PADDW-mmxreg.mmxrm
PADDW-xmmreg.xmmrm))

(setf (gethash "PADDW-mmxreg.mmxrm" *x64-instruction-variants-hash-table*) (list
PADDW-mmxreg.mmxrm))

(setf (gethash "PADDW-xmmreg.xmmrm" *x64-instruction-variants-hash-table*) (list
PADDW-xmmreg.xmmrm))

(setf (gethash "PFADD" *x64-instruction-variants-hash-table*) (list
PFADD-mmxreg.mmxrm))

(setf (gethash "PFADD-mmxreg.mmxrm" *x64-instruction-variants-hash-table*) (list
PFADD-mmxreg.mmxrm))

(setf (gethash "PHADDD" *x64-instruction-variants-hash-table*) (list
PHADDD-mmxreg.mmxrm
PHADDD-xmmreg.xmmrm))

(setf (gethash "PHADDD-mmxreg.mmxrm" *x64-instruction-variants-hash-table*) (list
PHADDD-mmxreg.mmxrm))

(setf (gethash "PHADDD-xmmreg.xmmrm" *x64-instruction-variants-hash-table*) (list
PHADDD-xmmreg.xmmrm))

(setf (gethash "PHADDSW" *x64-instruction-variants-hash-table*) (list
PHADDSW-mmxreg.mmxrm
PHADDSW-xmmreg.xmmrm))

(setf (gethash "PHADDSW-mmxreg.mmxrm" *x64-instruction-variants-hash-table*) (list
PHADDSW-mmxreg.mmxrm))

(setf (gethash "PHADDSW-xmmreg.xmmrm" *x64-instruction-variants-hash-table*) (list
PHADDSW-xmmreg.xmmrm))

(setf (gethash "PHADDW" *x64-instruction-variants-hash-table*) (list
PHADDW-mmxreg.mmxrm
PHADDW-xmmreg.xmmrm))

(setf (gethash "PHADDW-mmxreg.mmxrm" *x64-instruction-variants-hash-table*) (list
PHADDW-mmxreg.mmxrm))

(setf (gethash "PHADDW-xmmreg.xmmrm" *x64-instruction-variants-hash-table*) (list
PHADDW-xmmreg.xmmrm))

(setf (gethash "PMADDUBSW" *x64-instruction-variants-hash-table*) (list
PMADDUBSW-mmxreg.mmxrm
PMADDUBSW-xmmreg.xmmrm))

(setf (gethash "PMADDUBSW-mmxreg.mmxrm" *x64-instruction-variants-hash-table*) (list
PMADDUBSW-mmxreg.mmxrm))

(setf (gethash "PMADDUBSW-xmmreg.xmmrm" *x64-instruction-variants-hash-table*) (list
PMADDUBSW-xmmreg.xmmrm))

(setf (gethash "PMADDWD" *x64-instruction-variants-hash-table*) (list
PMADDWD-mmxreg.mmxrm
PMADDWD-xmmreg.xmmrm))

(setf (gethash "PMADDWD-mmxreg.mmxrm" *x64-instruction-variants-hash-table*) (list
PMADDWD-mmxreg.mmxrm))

(setf (gethash "PMADDWD-xmmreg.xmmrm" *x64-instruction-variants-hash-table*) (list
PMADDWD-xmmreg.xmmrm))

(setf (gethash "VADDPD" *x64-instruction-variants-hash-table*) (list
VADDPD-xmmreg.xmmreg*.xmmrm128
VADDPD-ymmreg.ymmreg*.ymmrm256
VADDPD-xmmreg-mask-z.xmmreg.xmmrm128-b64
VADDPD-ymmreg-mask-z.ymmreg.ymmrm256-b64
VADDPD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VADDPD-xmmreg-mask-z.xmmreg.xmmrm128-b64" *x64-instruction-variants-hash-table*) (list
VADDPD-xmmreg-mask-z.xmmreg.xmmrm128-b64))

(setf (gethash "VADDPD-xmmreg.xmmreg*.xmmrm128" *x64-instruction-variants-hash-table*) (list
VADDPD-xmmreg.xmmreg*.xmmrm128))

(setf (gethash "VADDPD-ymmreg-mask-z.ymmreg.ymmrm256-b64" *x64-instruction-variants-hash-table*) (list
VADDPD-ymmreg-mask-z.ymmreg.ymmrm256-b64))

(setf (gethash "VADDPD-ymmreg.ymmreg*.ymmrm256" *x64-instruction-variants-hash-table*) (list
VADDPD-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VADDPD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er" *x64-instruction-variants-hash-table*) (list
VADDPD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VADDPS" *x64-instruction-variants-hash-table*) (list
VADDPS-xmmreg.xmmreg*.xmmrm128
VADDPS-ymmreg.ymmreg*.ymmrm256
VADDPS-xmmreg-mask-z.xmmreg.xmmrm128-b32
VADDPS-ymmreg-mask-z.ymmreg.ymmrm256-b32
VADDPS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VADDPS-xmmreg-mask-z.xmmreg.xmmrm128-b32" *x64-instruction-variants-hash-table*) (list
VADDPS-xmmreg-mask-z.xmmreg.xmmrm128-b32))

(setf (gethash "VADDPS-xmmreg.xmmreg*.xmmrm128" *x64-instruction-variants-hash-table*) (list
VADDPS-xmmreg.xmmreg*.xmmrm128))

(setf (gethash "VADDPS-ymmreg-mask-z.ymmreg.ymmrm256-b32" *x64-instruction-variants-hash-table*) (list
VADDPS-ymmreg-mask-z.ymmreg.ymmrm256-b32))

(setf (gethash "VADDPS-ymmreg.ymmreg*.ymmrm256" *x64-instruction-variants-hash-table*) (list
VADDPS-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VADDPS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er" *x64-instruction-variants-hash-table*) (list
VADDPS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VADDSD" *x64-instruction-variants-hash-table*) (list
VADDSD-xmmreg.xmmreg*.xmmrm64
VADDSD-xmmreg-mask-z.xmmreg.xmmrm64-er))

(setf (gethash "VADDSD-xmmreg-mask-z.xmmreg.xmmrm64-er" *x64-instruction-variants-hash-table*) (list
VADDSD-xmmreg-mask-z.xmmreg.xmmrm64-er))

(setf (gethash "VADDSD-xmmreg.xmmreg*.xmmrm64" *x64-instruction-variants-hash-table*) (list
VADDSD-xmmreg.xmmreg*.xmmrm64))

(setf (gethash "VADDSS" *x64-instruction-variants-hash-table*) (list
VADDSS-xmmreg.xmmreg*.xmmrm32
VADDSS-xmmreg-mask-z.xmmreg.xmmrm32-er))

(setf (gethash "VADDSS-xmmreg-mask-z.xmmreg.xmmrm32-er" *x64-instruction-variants-hash-table*) (list
VADDSS-xmmreg-mask-z.xmmreg.xmmrm32-er))

(setf (gethash "VADDSS-xmmreg.xmmreg*.xmmrm32" *x64-instruction-variants-hash-table*) (list
VADDSS-xmmreg.xmmreg*.xmmrm32))

(setf (gethash "VADDSUBPD" *x64-instruction-variants-hash-table*) (list
VADDSUBPD-xmmreg.xmmreg*.xmmrm128
VADDSUBPD-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VADDSUBPD-xmmreg.xmmreg*.xmmrm128" *x64-instruction-variants-hash-table*) (list
VADDSUBPD-xmmreg.xmmreg*.xmmrm128))

(setf (gethash "VADDSUBPD-ymmreg.ymmreg*.ymmrm256" *x64-instruction-variants-hash-table*) (list
VADDSUBPD-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VADDSUBPS" *x64-instruction-variants-hash-table*) (list
VADDSUBPS-xmmreg.xmmreg*.xmmrm128
VADDSUBPS-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VADDSUBPS-xmmreg.xmmreg*.xmmrm128" *x64-instruction-variants-hash-table*) (list
VADDSUBPS-xmmreg.xmmreg*.xmmrm128))

(setf (gethash "VADDSUBPS-ymmreg.ymmreg*.ymmrm256" *x64-instruction-variants-hash-table*) (list
VADDSUBPS-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VFMADD123PD" *x64-instruction-variants-hash-table*) (list
VFMADD123PD-xmmreg.xmmreg.xmmrm128
VFMADD123PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADD123PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADD123PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADD123PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADD123PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADD123PS" *x64-instruction-variants-hash-table*) (list
VFMADD123PS-xmmreg.xmmreg.xmmrm128
VFMADD123PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADD123PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADD123PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADD123PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADD123PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADD123SD" *x64-instruction-variants-hash-table*) (list
VFMADD123SD-xmmreg.xmmreg.xmmrm64))

(setf (gethash "VFMADD123SD-xmmreg.xmmreg.xmmrm64" *x64-instruction-variants-hash-table*) (list
VFMADD123SD-xmmreg.xmmreg.xmmrm64))

(setf (gethash "VFMADD123SS" *x64-instruction-variants-hash-table*) (list
VFMADD123SS-xmmreg.xmmreg.xmmrm32))

(setf (gethash "VFMADD123SS-xmmreg.xmmreg.xmmrm32" *x64-instruction-variants-hash-table*) (list
VFMADD123SS-xmmreg.xmmreg.xmmrm32))

(setf (gethash "VFMADD132PD" *x64-instruction-variants-hash-table*) (list
VFMADD132PD-xmmreg.xmmreg.xmmrm128
VFMADD132PD-ymmreg.ymmreg.ymmrm256
VFMADD132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64
VFMADD132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64
VFMADD132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFMADD132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64" *x64-instruction-variants-hash-table*) (list
VFMADD132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64))

(setf (gethash "VFMADD132PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADD132PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADD132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64" *x64-instruction-variants-hash-table*) (list
VFMADD132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64))

(setf (gethash "VFMADD132PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADD132PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADD132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er" *x64-instruction-variants-hash-table*) (list
VFMADD132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFMADD132PS" *x64-instruction-variants-hash-table*) (list
VFMADD132PS-xmmreg.xmmreg.xmmrm128
VFMADD132PS-ymmreg.ymmreg.ymmrm256
VFMADD132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32
VFMADD132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32
VFMADD132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFMADD132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32" *x64-instruction-variants-hash-table*) (list
VFMADD132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32))

(setf (gethash "VFMADD132PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADD132PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADD132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32" *x64-instruction-variants-hash-table*) (list
VFMADD132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32))

(setf (gethash "VFMADD132PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADD132PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADD132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er" *x64-instruction-variants-hash-table*) (list
VFMADD132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFMADD132SD" *x64-instruction-variants-hash-table*) (list
VFMADD132SD-xmmreg.xmmreg.xmmrm64
VFMADD132SD-xmmreg-mask-z.xmmreg.xmmrm64-er))

(setf (gethash "VFMADD132SD-xmmreg-mask-z.xmmreg.xmmrm64-er" *x64-instruction-variants-hash-table*) (list
VFMADD132SD-xmmreg-mask-z.xmmreg.xmmrm64-er))

(setf (gethash "VFMADD132SD-xmmreg.xmmreg.xmmrm64" *x64-instruction-variants-hash-table*) (list
VFMADD132SD-xmmreg.xmmreg.xmmrm64))

(setf (gethash "VFMADD132SS" *x64-instruction-variants-hash-table*) (list
VFMADD132SS-xmmreg.xmmreg.xmmrm32
VFMADD132SS-xmmreg-mask-z.xmmreg.xmmrm32-er))

(setf (gethash "VFMADD132SS-xmmreg-mask-z.xmmreg.xmmrm32-er" *x64-instruction-variants-hash-table*) (list
VFMADD132SS-xmmreg-mask-z.xmmreg.xmmrm32-er))

(setf (gethash "VFMADD132SS-xmmreg.xmmreg.xmmrm32" *x64-instruction-variants-hash-table*) (list
VFMADD132SS-xmmreg.xmmreg.xmmrm32))

(setf (gethash "VFMADD213PD" *x64-instruction-variants-hash-table*) (list
VFMADD213PD-xmmreg.xmmreg.xmmrm128
VFMADD213PD-ymmreg.ymmreg.ymmrm256
VFMADD213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64
VFMADD213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64
VFMADD213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFMADD213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64" *x64-instruction-variants-hash-table*) (list
VFMADD213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64))

(setf (gethash "VFMADD213PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADD213PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADD213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64" *x64-instruction-variants-hash-table*) (list
VFMADD213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64))

(setf (gethash "VFMADD213PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADD213PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADD213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er" *x64-instruction-variants-hash-table*) (list
VFMADD213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFMADD213PS" *x64-instruction-variants-hash-table*) (list
VFMADD213PS-xmmreg.xmmreg.xmmrm128
VFMADD213PS-ymmreg.ymmreg.ymmrm256
VFMADD213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32
VFMADD213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32
VFMADD213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFMADD213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32" *x64-instruction-variants-hash-table*) (list
VFMADD213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32))

(setf (gethash "VFMADD213PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADD213PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADD213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32" *x64-instruction-variants-hash-table*) (list
VFMADD213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32))

(setf (gethash "VFMADD213PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADD213PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADD213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er" *x64-instruction-variants-hash-table*) (list
VFMADD213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFMADD213SD" *x64-instruction-variants-hash-table*) (list
VFMADD213SD-xmmreg.xmmreg.xmmrm64
VFMADD213SD-xmmreg-mask-z.xmmreg.xmmrm64-er))

(setf (gethash "VFMADD213SD-xmmreg-mask-z.xmmreg.xmmrm64-er" *x64-instruction-variants-hash-table*) (list
VFMADD213SD-xmmreg-mask-z.xmmreg.xmmrm64-er))

(setf (gethash "VFMADD213SD-xmmreg.xmmreg.xmmrm64" *x64-instruction-variants-hash-table*) (list
VFMADD213SD-xmmreg.xmmreg.xmmrm64))

(setf (gethash "VFMADD213SS" *x64-instruction-variants-hash-table*) (list
VFMADD213SS-xmmreg.xmmreg.xmmrm32
VFMADD213SS-xmmreg-mask-z.xmmreg.xmmrm32-er))

(setf (gethash "VFMADD213SS-xmmreg-mask-z.xmmreg.xmmrm32-er" *x64-instruction-variants-hash-table*) (list
VFMADD213SS-xmmreg-mask-z.xmmreg.xmmrm32-er))

(setf (gethash "VFMADD213SS-xmmreg.xmmreg.xmmrm32" *x64-instruction-variants-hash-table*) (list
VFMADD213SS-xmmreg.xmmreg.xmmrm32))

(setf (gethash "VFMADD231PD" *x64-instruction-variants-hash-table*) (list
VFMADD231PD-xmmreg.xmmreg.xmmrm128
VFMADD231PD-ymmreg.ymmreg.ymmrm256
VFMADD231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64
VFMADD231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64
VFMADD231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFMADD231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64" *x64-instruction-variants-hash-table*) (list
VFMADD231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64))

(setf (gethash "VFMADD231PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADD231PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADD231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64" *x64-instruction-variants-hash-table*) (list
VFMADD231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64))

(setf (gethash "VFMADD231PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADD231PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADD231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er" *x64-instruction-variants-hash-table*) (list
VFMADD231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFMADD231PS" *x64-instruction-variants-hash-table*) (list
VFMADD231PS-xmmreg.xmmreg.xmmrm128
VFMADD231PS-ymmreg.ymmreg.ymmrm256
VFMADD231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32
VFMADD231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32
VFMADD231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFMADD231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32" *x64-instruction-variants-hash-table*) (list
VFMADD231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32))

(setf (gethash "VFMADD231PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADD231PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADD231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32" *x64-instruction-variants-hash-table*) (list
VFMADD231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32))

(setf (gethash "VFMADD231PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADD231PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADD231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er" *x64-instruction-variants-hash-table*) (list
VFMADD231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFMADD231SD" *x64-instruction-variants-hash-table*) (list
VFMADD231SD-xmmreg.xmmreg.xmmrm64
VFMADD231SD-xmmreg-mask-z.xmmreg.xmmrm64-er))

(setf (gethash "VFMADD231SD-xmmreg-mask-z.xmmreg.xmmrm64-er" *x64-instruction-variants-hash-table*) (list
VFMADD231SD-xmmreg-mask-z.xmmreg.xmmrm64-er))

(setf (gethash "VFMADD231SD-xmmreg.xmmreg.xmmrm64" *x64-instruction-variants-hash-table*) (list
VFMADD231SD-xmmreg.xmmreg.xmmrm64))

(setf (gethash "VFMADD231SS" *x64-instruction-variants-hash-table*) (list
VFMADD231SS-xmmreg.xmmreg.xmmrm32
VFMADD231SS-xmmreg-mask-z.xmmreg.xmmrm32-er))

(setf (gethash "VFMADD231SS-xmmreg-mask-z.xmmreg.xmmrm32-er" *x64-instruction-variants-hash-table*) (list
VFMADD231SS-xmmreg-mask-z.xmmreg.xmmrm32-er))

(setf (gethash "VFMADD231SS-xmmreg.xmmreg.xmmrm32" *x64-instruction-variants-hash-table*) (list
VFMADD231SS-xmmreg.xmmreg.xmmrm32))

(setf (gethash "VFMADD312PD" *x64-instruction-variants-hash-table*) (list
VFMADD312PD-xmmreg.xmmreg.xmmrm128
VFMADD312PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADD312PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADD312PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADD312PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADD312PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADD312PS" *x64-instruction-variants-hash-table*) (list
VFMADD312PS-xmmreg.xmmreg.xmmrm128
VFMADD312PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADD312PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADD312PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADD312PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADD312PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADD312SD" *x64-instruction-variants-hash-table*) (list
VFMADD312SD-xmmreg.xmmreg.xmmrm64))

(setf (gethash "VFMADD312SD-xmmreg.xmmreg.xmmrm64" *x64-instruction-variants-hash-table*) (list
VFMADD312SD-xmmreg.xmmreg.xmmrm64))

(setf (gethash "VFMADD312SS" *x64-instruction-variants-hash-table*) (list
VFMADD312SS-xmmreg.xmmreg.xmmrm32))

(setf (gethash "VFMADD312SS-xmmreg.xmmreg.xmmrm32" *x64-instruction-variants-hash-table*) (list
VFMADD312SS-xmmreg.xmmreg.xmmrm32))

(setf (gethash "VFMADD321PD" *x64-instruction-variants-hash-table*) (list
VFMADD321PD-xmmreg.xmmreg.xmmrm128
VFMADD321PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADD321PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADD321PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADD321PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADD321PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADD321PS" *x64-instruction-variants-hash-table*) (list
VFMADD321PS-xmmreg.xmmreg.xmmrm128
VFMADD321PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADD321PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADD321PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADD321PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADD321PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADD321SD" *x64-instruction-variants-hash-table*) (list
VFMADD321SD-xmmreg.xmmreg.xmmrm64))

(setf (gethash "VFMADD321SD-xmmreg.xmmreg.xmmrm64" *x64-instruction-variants-hash-table*) (list
VFMADD321SD-xmmreg.xmmreg.xmmrm64))

(setf (gethash "VFMADD321SS" *x64-instruction-variants-hash-table*) (list
VFMADD321SS-xmmreg.xmmreg.xmmrm32))

(setf (gethash "VFMADD321SS-xmmreg.xmmreg.xmmrm32" *x64-instruction-variants-hash-table*) (list
VFMADD321SS-xmmreg.xmmreg.xmmrm32))

(setf (gethash "VFMADDPD" *x64-instruction-variants-hash-table*) (list
VFMADDPD-xmmreg.xmmreg*.xmmrm128.xmmreg
VFMADDPD-ymmreg.ymmreg*.ymmrm256.ymmreg
VFMADDPD-xmmreg.xmmreg*.xmmreg.xmmrm128
VFMADDPD-ymmreg.ymmreg*.ymmreg.ymmrm256))

(setf (gethash "VFMADDPD-xmmreg.xmmreg*.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADDPD-xmmreg.xmmreg*.xmmreg.xmmrm128))

(setf (gethash "VFMADDPD-xmmreg.xmmreg*.xmmrm128.xmmreg" *x64-instruction-variants-hash-table*) (list
VFMADDPD-xmmreg.xmmreg*.xmmrm128.xmmreg))

(setf (gethash "VFMADDPD-ymmreg.ymmreg*.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADDPD-ymmreg.ymmreg*.ymmreg.ymmrm256))

(setf (gethash "VFMADDPD-ymmreg.ymmreg*.ymmrm256.ymmreg" *x64-instruction-variants-hash-table*) (list
VFMADDPD-ymmreg.ymmreg*.ymmrm256.ymmreg))

(setf (gethash "VFMADDPS" *x64-instruction-variants-hash-table*) (list
VFMADDPS-xmmreg.xmmreg*.xmmrm128.xmmreg
VFMADDPS-ymmreg.ymmreg*.ymmrm256.ymmreg
VFMADDPS-xmmreg.xmmreg*.xmmreg.xmmrm128
VFMADDPS-ymmreg.ymmreg*.ymmreg.ymmrm256))

(setf (gethash "VFMADDPS-xmmreg.xmmreg*.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADDPS-xmmreg.xmmreg*.xmmreg.xmmrm128))

(setf (gethash "VFMADDPS-xmmreg.xmmreg*.xmmrm128.xmmreg" *x64-instruction-variants-hash-table*) (list
VFMADDPS-xmmreg.xmmreg*.xmmrm128.xmmreg))

(setf (gethash "VFMADDPS-ymmreg.ymmreg*.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADDPS-ymmreg.ymmreg*.ymmreg.ymmrm256))

(setf (gethash "VFMADDPS-ymmreg.ymmreg*.ymmrm256.ymmreg" *x64-instruction-variants-hash-table*) (list
VFMADDPS-ymmreg.ymmreg*.ymmrm256.ymmreg))

(setf (gethash "VFMADDSD" *x64-instruction-variants-hash-table*) (list
VFMADDSD-xmmreg.xmmreg*.xmmrm64.xmmreg
VFMADDSD-xmmreg.xmmreg*.xmmreg.xmmrm64))

(setf (gethash "VFMADDSD-xmmreg.xmmreg*.xmmreg.xmmrm64" *x64-instruction-variants-hash-table*) (list
VFMADDSD-xmmreg.xmmreg*.xmmreg.xmmrm64))

(setf (gethash "VFMADDSD-xmmreg.xmmreg*.xmmrm64.xmmreg" *x64-instruction-variants-hash-table*) (list
VFMADDSD-xmmreg.xmmreg*.xmmrm64.xmmreg))

(setf (gethash "VFMADDSS" *x64-instruction-variants-hash-table*) (list
VFMADDSS-xmmreg.xmmreg*.xmmrm32.xmmreg
VFMADDSS-xmmreg.xmmreg*.xmmreg.xmmrm32))

(setf (gethash "VFMADDSS-xmmreg.xmmreg*.xmmreg.xmmrm32" *x64-instruction-variants-hash-table*) (list
VFMADDSS-xmmreg.xmmreg*.xmmreg.xmmrm32))

(setf (gethash "VFMADDSS-xmmreg.xmmreg*.xmmrm32.xmmreg" *x64-instruction-variants-hash-table*) (list
VFMADDSS-xmmreg.xmmreg*.xmmrm32.xmmreg))

(setf (gethash "VFMADDSUB123PD" *x64-instruction-variants-hash-table*) (list
VFMADDSUB123PD-xmmreg.xmmreg.xmmrm128
VFMADDSUB123PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUB123PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADDSUB123PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADDSUB123PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADDSUB123PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUB123PS" *x64-instruction-variants-hash-table*) (list
VFMADDSUB123PS-xmmreg.xmmreg.xmmrm128
VFMADDSUB123PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUB123PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADDSUB123PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADDSUB123PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADDSUB123PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUB132PD" *x64-instruction-variants-hash-table*) (list
VFMADDSUB132PD-xmmreg.xmmreg.xmmrm128
VFMADDSUB132PD-ymmreg.ymmreg.ymmrm256
VFMADDSUB132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64
VFMADDSUB132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64
VFMADDSUB132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFMADDSUB132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64" *x64-instruction-variants-hash-table*) (list
VFMADDSUB132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64))

(setf (gethash "VFMADDSUB132PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADDSUB132PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADDSUB132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64" *x64-instruction-variants-hash-table*) (list
VFMADDSUB132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64))

(setf (gethash "VFMADDSUB132PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADDSUB132PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUB132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er" *x64-instruction-variants-hash-table*) (list
VFMADDSUB132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFMADDSUB132PS" *x64-instruction-variants-hash-table*) (list
VFMADDSUB132PS-xmmreg.xmmreg.xmmrm128
VFMADDSUB132PS-ymmreg.ymmreg.ymmrm256
VFMADDSUB132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32
VFMADDSUB132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32
VFMADDSUB132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFMADDSUB132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32" *x64-instruction-variants-hash-table*) (list
VFMADDSUB132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32))

(setf (gethash "VFMADDSUB132PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADDSUB132PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADDSUB132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32" *x64-instruction-variants-hash-table*) (list
VFMADDSUB132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32))

(setf (gethash "VFMADDSUB132PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADDSUB132PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUB132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er" *x64-instruction-variants-hash-table*) (list
VFMADDSUB132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFMADDSUB213PD" *x64-instruction-variants-hash-table*) (list
VFMADDSUB213PD-xmmreg.xmmreg.xmmrm128
VFMADDSUB213PD-ymmreg.ymmreg.ymmrm256
VFMADDSUB213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64
VFMADDSUB213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64
VFMADDSUB213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFMADDSUB213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64" *x64-instruction-variants-hash-table*) (list
VFMADDSUB213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64))

(setf (gethash "VFMADDSUB213PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADDSUB213PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADDSUB213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64" *x64-instruction-variants-hash-table*) (list
VFMADDSUB213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64))

(setf (gethash "VFMADDSUB213PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADDSUB213PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUB213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er" *x64-instruction-variants-hash-table*) (list
VFMADDSUB213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFMADDSUB213PS" *x64-instruction-variants-hash-table*) (list
VFMADDSUB213PS-xmmreg.xmmreg.xmmrm128
VFMADDSUB213PS-ymmreg.ymmreg.ymmrm256
VFMADDSUB213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32
VFMADDSUB213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32
VFMADDSUB213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFMADDSUB213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32" *x64-instruction-variants-hash-table*) (list
VFMADDSUB213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32))

(setf (gethash "VFMADDSUB213PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADDSUB213PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADDSUB213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32" *x64-instruction-variants-hash-table*) (list
VFMADDSUB213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32))

(setf (gethash "VFMADDSUB213PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADDSUB213PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUB213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er" *x64-instruction-variants-hash-table*) (list
VFMADDSUB213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFMADDSUB231PD" *x64-instruction-variants-hash-table*) (list
VFMADDSUB231PD-xmmreg.xmmreg.xmmrm128
VFMADDSUB231PD-ymmreg.ymmreg.ymmrm256
VFMADDSUB231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64
VFMADDSUB231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64
VFMADDSUB231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFMADDSUB231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64" *x64-instruction-variants-hash-table*) (list
VFMADDSUB231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64))

(setf (gethash "VFMADDSUB231PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADDSUB231PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADDSUB231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64" *x64-instruction-variants-hash-table*) (list
VFMADDSUB231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64))

(setf (gethash "VFMADDSUB231PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADDSUB231PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUB231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er" *x64-instruction-variants-hash-table*) (list
VFMADDSUB231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFMADDSUB231PS" *x64-instruction-variants-hash-table*) (list
VFMADDSUB231PS-xmmreg.xmmreg.xmmrm128
VFMADDSUB231PS-ymmreg.ymmreg.ymmrm256
VFMADDSUB231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32
VFMADDSUB231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32
VFMADDSUB231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFMADDSUB231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32" *x64-instruction-variants-hash-table*) (list
VFMADDSUB231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32))

(setf (gethash "VFMADDSUB231PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADDSUB231PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADDSUB231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32" *x64-instruction-variants-hash-table*) (list
VFMADDSUB231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32))

(setf (gethash "VFMADDSUB231PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADDSUB231PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUB231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er" *x64-instruction-variants-hash-table*) (list
VFMADDSUB231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFMADDSUB312PD" *x64-instruction-variants-hash-table*) (list
VFMADDSUB312PD-xmmreg.xmmreg.xmmrm128
VFMADDSUB312PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUB312PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADDSUB312PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADDSUB312PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADDSUB312PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUB312PS" *x64-instruction-variants-hash-table*) (list
VFMADDSUB312PS-xmmreg.xmmreg.xmmrm128
VFMADDSUB312PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUB312PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADDSUB312PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADDSUB312PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADDSUB312PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUB321PD" *x64-instruction-variants-hash-table*) (list
VFMADDSUB321PD-xmmreg.xmmreg.xmmrm128
VFMADDSUB321PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUB321PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADDSUB321PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADDSUB321PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADDSUB321PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUB321PS" *x64-instruction-variants-hash-table*) (list
VFMADDSUB321PS-xmmreg.xmmreg.xmmrm128
VFMADDSUB321PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUB321PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADDSUB321PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMADDSUB321PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADDSUB321PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUBPD" *x64-instruction-variants-hash-table*) (list
VFMADDSUBPD-xmmreg.xmmreg*.xmmrm128.xmmreg
VFMADDSUBPD-ymmreg.ymmreg*.ymmrm256.ymmreg
VFMADDSUBPD-xmmreg.xmmreg*.xmmreg.xmmrm128
VFMADDSUBPD-ymmreg.ymmreg*.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUBPD-xmmreg.xmmreg*.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADDSUBPD-xmmreg.xmmreg*.xmmreg.xmmrm128))

(setf (gethash "VFMADDSUBPD-xmmreg.xmmreg*.xmmrm128.xmmreg" *x64-instruction-variants-hash-table*) (list
VFMADDSUBPD-xmmreg.xmmreg*.xmmrm128.xmmreg))

(setf (gethash "VFMADDSUBPD-ymmreg.ymmreg*.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADDSUBPD-ymmreg.ymmreg*.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUBPD-ymmreg.ymmreg*.ymmrm256.ymmreg" *x64-instruction-variants-hash-table*) (list
VFMADDSUBPD-ymmreg.ymmreg*.ymmrm256.ymmreg))

(setf (gethash "VFMADDSUBPS" *x64-instruction-variants-hash-table*) (list
VFMADDSUBPS-xmmreg.xmmreg*.xmmrm128.xmmreg
VFMADDSUBPS-ymmreg.ymmreg*.ymmrm256.ymmreg
VFMADDSUBPS-xmmreg.xmmreg*.xmmreg.xmmrm128
VFMADDSUBPS-ymmreg.ymmreg*.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUBPS-xmmreg.xmmreg*.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMADDSUBPS-xmmreg.xmmreg*.xmmreg.xmmrm128))

(setf (gethash "VFMADDSUBPS-xmmreg.xmmreg*.xmmrm128.xmmreg" *x64-instruction-variants-hash-table*) (list
VFMADDSUBPS-xmmreg.xmmreg*.xmmrm128.xmmreg))

(setf (gethash "VFMADDSUBPS-ymmreg.ymmreg*.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMADDSUBPS-ymmreg.ymmreg*.ymmreg.ymmrm256))

(setf (gethash "VFMADDSUBPS-ymmreg.ymmreg*.ymmrm256.ymmreg" *x64-instruction-variants-hash-table*) (list
VFMADDSUBPS-ymmreg.ymmreg*.ymmrm256.ymmreg))

(setf (gethash "VFMSUBADD123PD" *x64-instruction-variants-hash-table*) (list
VFMSUBADD123PD-xmmreg.xmmreg.xmmrm128
VFMSUBADD123PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADD123PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMSUBADD123PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMSUBADD123PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMSUBADD123PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADD123PS" *x64-instruction-variants-hash-table*) (list
VFMSUBADD123PS-xmmreg.xmmreg.xmmrm128
VFMSUBADD123PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADD123PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMSUBADD123PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMSUBADD123PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMSUBADD123PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADD132PD" *x64-instruction-variants-hash-table*) (list
VFMSUBADD132PD-xmmreg.xmmreg.xmmrm128
VFMSUBADD132PD-ymmreg.ymmreg.ymmrm256
VFMSUBADD132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64
VFMSUBADD132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64
VFMSUBADD132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFMSUBADD132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64" *x64-instruction-variants-hash-table*) (list
VFMSUBADD132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64))

(setf (gethash "VFMSUBADD132PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMSUBADD132PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMSUBADD132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64" *x64-instruction-variants-hash-table*) (list
VFMSUBADD132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64))

(setf (gethash "VFMSUBADD132PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMSUBADD132PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADD132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er" *x64-instruction-variants-hash-table*) (list
VFMSUBADD132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFMSUBADD132PS" *x64-instruction-variants-hash-table*) (list
VFMSUBADD132PS-xmmreg.xmmreg.xmmrm128
VFMSUBADD132PS-ymmreg.ymmreg.ymmrm256
VFMSUBADD132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32
VFMSUBADD132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32
VFMSUBADD132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFMSUBADD132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32" *x64-instruction-variants-hash-table*) (list
VFMSUBADD132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32))

(setf (gethash "VFMSUBADD132PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMSUBADD132PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMSUBADD132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32" *x64-instruction-variants-hash-table*) (list
VFMSUBADD132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32))

(setf (gethash "VFMSUBADD132PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMSUBADD132PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADD132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er" *x64-instruction-variants-hash-table*) (list
VFMSUBADD132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFMSUBADD213PD" *x64-instruction-variants-hash-table*) (list
VFMSUBADD213PD-xmmreg.xmmreg.xmmrm128
VFMSUBADD213PD-ymmreg.ymmreg.ymmrm256
VFMSUBADD213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64
VFMSUBADD213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64
VFMSUBADD213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFMSUBADD213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64" *x64-instruction-variants-hash-table*) (list
VFMSUBADD213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64))

(setf (gethash "VFMSUBADD213PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMSUBADD213PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMSUBADD213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64" *x64-instruction-variants-hash-table*) (list
VFMSUBADD213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64))

(setf (gethash "VFMSUBADD213PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMSUBADD213PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADD213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er" *x64-instruction-variants-hash-table*) (list
VFMSUBADD213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFMSUBADD213PS" *x64-instruction-variants-hash-table*) (list
VFMSUBADD213PS-xmmreg.xmmreg.xmmrm128
VFMSUBADD213PS-ymmreg.ymmreg.ymmrm256
VFMSUBADD213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32
VFMSUBADD213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32
VFMSUBADD213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFMSUBADD213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32" *x64-instruction-variants-hash-table*) (list
VFMSUBADD213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32))

(setf (gethash "VFMSUBADD213PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMSUBADD213PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMSUBADD213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32" *x64-instruction-variants-hash-table*) (list
VFMSUBADD213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32))

(setf (gethash "VFMSUBADD213PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMSUBADD213PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADD213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er" *x64-instruction-variants-hash-table*) (list
VFMSUBADD213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFMSUBADD231PD" *x64-instruction-variants-hash-table*) (list
VFMSUBADD231PD-xmmreg.xmmreg.xmmrm128
VFMSUBADD231PD-ymmreg.ymmreg.ymmrm256
VFMSUBADD231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64
VFMSUBADD231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64
VFMSUBADD231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFMSUBADD231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64" *x64-instruction-variants-hash-table*) (list
VFMSUBADD231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64))

(setf (gethash "VFMSUBADD231PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMSUBADD231PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMSUBADD231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64" *x64-instruction-variants-hash-table*) (list
VFMSUBADD231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64))

(setf (gethash "VFMSUBADD231PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMSUBADD231PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADD231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er" *x64-instruction-variants-hash-table*) (list
VFMSUBADD231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFMSUBADD231PS" *x64-instruction-variants-hash-table*) (list
VFMSUBADD231PS-xmmreg.xmmreg.xmmrm128
VFMSUBADD231PS-ymmreg.ymmreg.ymmrm256
VFMSUBADD231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32
VFMSUBADD231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32
VFMSUBADD231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFMSUBADD231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32" *x64-instruction-variants-hash-table*) (list
VFMSUBADD231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32))

(setf (gethash "VFMSUBADD231PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMSUBADD231PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMSUBADD231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32" *x64-instruction-variants-hash-table*) (list
VFMSUBADD231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32))

(setf (gethash "VFMSUBADD231PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMSUBADD231PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADD231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er" *x64-instruction-variants-hash-table*) (list
VFMSUBADD231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFMSUBADD312PD" *x64-instruction-variants-hash-table*) (list
VFMSUBADD312PD-xmmreg.xmmreg.xmmrm128
VFMSUBADD312PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADD312PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMSUBADD312PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMSUBADD312PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMSUBADD312PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADD312PS" *x64-instruction-variants-hash-table*) (list
VFMSUBADD312PS-xmmreg.xmmreg.xmmrm128
VFMSUBADD312PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADD312PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMSUBADD312PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMSUBADD312PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMSUBADD312PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADD321PD" *x64-instruction-variants-hash-table*) (list
VFMSUBADD321PD-xmmreg.xmmreg.xmmrm128
VFMSUBADD321PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADD321PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMSUBADD321PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMSUBADD321PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMSUBADD321PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADD321PS" *x64-instruction-variants-hash-table*) (list
VFMSUBADD321PS-xmmreg.xmmreg.xmmrm128
VFMSUBADD321PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADD321PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMSUBADD321PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFMSUBADD321PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMSUBADD321PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADDPD" *x64-instruction-variants-hash-table*) (list
VFMSUBADDPD-xmmreg.xmmreg*.xmmrm128.xmmreg
VFMSUBADDPD-ymmreg.ymmreg*.ymmrm256.ymmreg
VFMSUBADDPD-xmmreg.xmmreg*.xmmreg.xmmrm128
VFMSUBADDPD-ymmreg.ymmreg*.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADDPD-xmmreg.xmmreg*.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMSUBADDPD-xmmreg.xmmreg*.xmmreg.xmmrm128))

(setf (gethash "VFMSUBADDPD-xmmreg.xmmreg*.xmmrm128.xmmreg" *x64-instruction-variants-hash-table*) (list
VFMSUBADDPD-xmmreg.xmmreg*.xmmrm128.xmmreg))

(setf (gethash "VFMSUBADDPD-ymmreg.ymmreg*.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMSUBADDPD-ymmreg.ymmreg*.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADDPD-ymmreg.ymmreg*.ymmrm256.ymmreg" *x64-instruction-variants-hash-table*) (list
VFMSUBADDPD-ymmreg.ymmreg*.ymmrm256.ymmreg))

(setf (gethash "VFMSUBADDPS" *x64-instruction-variants-hash-table*) (list
VFMSUBADDPS-xmmreg.xmmreg*.xmmrm128.xmmreg
VFMSUBADDPS-ymmreg.ymmreg*.ymmrm256.ymmreg
VFMSUBADDPS-xmmreg.xmmreg*.xmmreg.xmmrm128
VFMSUBADDPS-ymmreg.ymmreg*.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADDPS-xmmreg.xmmreg*.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFMSUBADDPS-xmmreg.xmmreg*.xmmreg.xmmrm128))

(setf (gethash "VFMSUBADDPS-xmmreg.xmmreg*.xmmrm128.xmmreg" *x64-instruction-variants-hash-table*) (list
VFMSUBADDPS-xmmreg.xmmreg*.xmmrm128.xmmreg))

(setf (gethash "VFMSUBADDPS-ymmreg.ymmreg*.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFMSUBADDPS-ymmreg.ymmreg*.ymmreg.ymmrm256))

(setf (gethash "VFMSUBADDPS-ymmreg.ymmreg*.ymmrm256.ymmreg" *x64-instruction-variants-hash-table*) (list
VFMSUBADDPS-ymmreg.ymmreg*.ymmrm256.ymmreg))

(setf (gethash "VFNMADD123PD" *x64-instruction-variants-hash-table*) (list
VFNMADD123PD-xmmreg.xmmreg.xmmrm128
VFNMADD123PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFNMADD123PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFNMADD123PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFNMADD123PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFNMADD123PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFNMADD123PS" *x64-instruction-variants-hash-table*) (list
VFNMADD123PS-xmmreg.xmmreg.xmmrm128
VFNMADD123PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFNMADD123PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFNMADD123PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFNMADD123PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFNMADD123PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFNMADD123SD" *x64-instruction-variants-hash-table*) (list
VFNMADD123SD-xmmreg.xmmreg.xmmrm64))

(setf (gethash "VFNMADD123SD-xmmreg.xmmreg.xmmrm64" *x64-instruction-variants-hash-table*) (list
VFNMADD123SD-xmmreg.xmmreg.xmmrm64))

(setf (gethash "VFNMADD123SS" *x64-instruction-variants-hash-table*) (list
VFNMADD123SS-xmmreg.xmmreg.xmmrm32))

(setf (gethash "VFNMADD123SS-xmmreg.xmmreg.xmmrm32" *x64-instruction-variants-hash-table*) (list
VFNMADD123SS-xmmreg.xmmreg.xmmrm32))

(setf (gethash "VFNMADD132PD" *x64-instruction-variants-hash-table*) (list
VFNMADD132PD-xmmreg.xmmreg.xmmrm128
VFNMADD132PD-ymmreg.ymmreg.ymmrm256
VFNMADD132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64
VFNMADD132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64
VFNMADD132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFNMADD132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64" *x64-instruction-variants-hash-table*) (list
VFNMADD132PD-xmmreg-mask-z.xmmreg.xmmrm128-b64))

(setf (gethash "VFNMADD132PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFNMADD132PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFNMADD132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64" *x64-instruction-variants-hash-table*) (list
VFNMADD132PD-ymmreg-mask-z.ymmreg.ymmrm256-b64))

(setf (gethash "VFNMADD132PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFNMADD132PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFNMADD132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er" *x64-instruction-variants-hash-table*) (list
VFNMADD132PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFNMADD132PS" *x64-instruction-variants-hash-table*) (list
VFNMADD132PS-xmmreg.xmmreg.xmmrm128
VFNMADD132PS-ymmreg.ymmreg.ymmrm256
VFNMADD132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32
VFNMADD132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32
VFNMADD132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFNMADD132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32" *x64-instruction-variants-hash-table*) (list
VFNMADD132PS-xmmreg-mask-z.xmmreg.xmmrm128-b32))

(setf (gethash "VFNMADD132PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFNMADD132PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFNMADD132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32" *x64-instruction-variants-hash-table*) (list
VFNMADD132PS-ymmreg-mask-z.ymmreg.ymmrm256-b32))

(setf (gethash "VFNMADD132PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFNMADD132PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFNMADD132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er" *x64-instruction-variants-hash-table*) (list
VFNMADD132PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFNMADD132SD" *x64-instruction-variants-hash-table*) (list
VFNMADD132SD-xmmreg.xmmreg.xmmrm64
VFNMADD132SD-xmmreg-mask-z.xmmreg.xmmrm64-er))

(setf (gethash "VFNMADD132SD-xmmreg-mask-z.xmmreg.xmmrm64-er" *x64-instruction-variants-hash-table*) (list
VFNMADD132SD-xmmreg-mask-z.xmmreg.xmmrm64-er))

(setf (gethash "VFNMADD132SD-xmmreg.xmmreg.xmmrm64" *x64-instruction-variants-hash-table*) (list
VFNMADD132SD-xmmreg.xmmreg.xmmrm64))

(setf (gethash "VFNMADD132SS" *x64-instruction-variants-hash-table*) (list
VFNMADD132SS-xmmreg.xmmreg.xmmrm32
VFNMADD132SS-xmmreg-mask-z.xmmreg.xmmrm32-er))

(setf (gethash "VFNMADD132SS-xmmreg-mask-z.xmmreg.xmmrm32-er" *x64-instruction-variants-hash-table*) (list
VFNMADD132SS-xmmreg-mask-z.xmmreg.xmmrm32-er))

(setf (gethash "VFNMADD132SS-xmmreg.xmmreg.xmmrm32" *x64-instruction-variants-hash-table*) (list
VFNMADD132SS-xmmreg.xmmreg.xmmrm32))

(setf (gethash "VFNMADD213PD" *x64-instruction-variants-hash-table*) (list
VFNMADD213PD-xmmreg.xmmreg.xmmrm128
VFNMADD213PD-ymmreg.ymmreg.ymmrm256
VFNMADD213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64
VFNMADD213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64
VFNMADD213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFNMADD213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64" *x64-instruction-variants-hash-table*) (list
VFNMADD213PD-xmmreg-mask-z.xmmreg.xmmrm128-b64))

(setf (gethash "VFNMADD213PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFNMADD213PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFNMADD213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64" *x64-instruction-variants-hash-table*) (list
VFNMADD213PD-ymmreg-mask-z.ymmreg.ymmrm256-b64))

(setf (gethash "VFNMADD213PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFNMADD213PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFNMADD213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er" *x64-instruction-variants-hash-table*) (list
VFNMADD213PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFNMADD213PS" *x64-instruction-variants-hash-table*) (list
VFNMADD213PS-xmmreg.xmmreg.xmmrm128
VFNMADD213PS-ymmreg.ymmreg.ymmrm256
VFNMADD213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32
VFNMADD213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32
VFNMADD213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFNMADD213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32" *x64-instruction-variants-hash-table*) (list
VFNMADD213PS-xmmreg-mask-z.xmmreg.xmmrm128-b32))

(setf (gethash "VFNMADD213PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFNMADD213PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFNMADD213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32" *x64-instruction-variants-hash-table*) (list
VFNMADD213PS-ymmreg-mask-z.ymmreg.ymmrm256-b32))

(setf (gethash "VFNMADD213PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFNMADD213PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFNMADD213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er" *x64-instruction-variants-hash-table*) (list
VFNMADD213PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFNMADD213SD" *x64-instruction-variants-hash-table*) (list
VFNMADD213SD-xmmreg.xmmreg.xmmrm64
VFNMADD213SD-xmmreg-mask-z.xmmreg.xmmrm64-er))

(setf (gethash "VFNMADD213SD-xmmreg-mask-z.xmmreg.xmmrm64-er" *x64-instruction-variants-hash-table*) (list
VFNMADD213SD-xmmreg-mask-z.xmmreg.xmmrm64-er))

(setf (gethash "VFNMADD213SD-xmmreg.xmmreg.xmmrm64" *x64-instruction-variants-hash-table*) (list
VFNMADD213SD-xmmreg.xmmreg.xmmrm64))

(setf (gethash "VFNMADD213SS" *x64-instruction-variants-hash-table*) (list
VFNMADD213SS-xmmreg.xmmreg.xmmrm32
VFNMADD213SS-xmmreg-mask-z.xmmreg.xmmrm32-er))

(setf (gethash "VFNMADD213SS-xmmreg-mask-z.xmmreg.xmmrm32-er" *x64-instruction-variants-hash-table*) (list
VFNMADD213SS-xmmreg-mask-z.xmmreg.xmmrm32-er))

(setf (gethash "VFNMADD213SS-xmmreg.xmmreg.xmmrm32" *x64-instruction-variants-hash-table*) (list
VFNMADD213SS-xmmreg.xmmreg.xmmrm32))

(setf (gethash "VFNMADD231PD" *x64-instruction-variants-hash-table*) (list
VFNMADD231PD-xmmreg.xmmreg.xmmrm128
VFNMADD231PD-ymmreg.ymmreg.ymmrm256
VFNMADD231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64
VFNMADD231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64
VFNMADD231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFNMADD231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64" *x64-instruction-variants-hash-table*) (list
VFNMADD231PD-xmmreg-mask-z.xmmreg.xmmrm128-b64))

(setf (gethash "VFNMADD231PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFNMADD231PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFNMADD231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64" *x64-instruction-variants-hash-table*) (list
VFNMADD231PD-ymmreg-mask-z.ymmreg.ymmrm256-b64))

(setf (gethash "VFNMADD231PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFNMADD231PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFNMADD231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er" *x64-instruction-variants-hash-table*) (list
VFNMADD231PD-zmmreg-mask-z.zmmreg.zmmrm512-b64-er))

(setf (gethash "VFNMADD231PS" *x64-instruction-variants-hash-table*) (list
VFNMADD231PS-xmmreg.xmmreg.xmmrm128
VFNMADD231PS-ymmreg.ymmreg.ymmrm256
VFNMADD231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32
VFNMADD231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32
VFNMADD231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFNMADD231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32" *x64-instruction-variants-hash-table*) (list
VFNMADD231PS-xmmreg-mask-z.xmmreg.xmmrm128-b32))

(setf (gethash "VFNMADD231PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFNMADD231PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFNMADD231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32" *x64-instruction-variants-hash-table*) (list
VFNMADD231PS-ymmreg-mask-z.ymmreg.ymmrm256-b32))

(setf (gethash "VFNMADD231PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFNMADD231PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFNMADD231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er" *x64-instruction-variants-hash-table*) (list
VFNMADD231PS-zmmreg-mask-z.zmmreg.zmmrm512-b32-er))

(setf (gethash "VFNMADD231SD" *x64-instruction-variants-hash-table*) (list
VFNMADD231SD-xmmreg.xmmreg.xmmrm64
VFNMADD231SD-xmmreg-mask-z.xmmreg.xmmrm64-er))

(setf (gethash "VFNMADD231SD-xmmreg-mask-z.xmmreg.xmmrm64-er" *x64-instruction-variants-hash-table*) (list
VFNMADD231SD-xmmreg-mask-z.xmmreg.xmmrm64-er))

(setf (gethash "VFNMADD231SD-xmmreg.xmmreg.xmmrm64" *x64-instruction-variants-hash-table*) (list
VFNMADD231SD-xmmreg.xmmreg.xmmrm64))

(setf (gethash "VFNMADD231SS" *x64-instruction-variants-hash-table*) (list
VFNMADD231SS-xmmreg.xmmreg.xmmrm32
VFNMADD231SS-xmmreg-mask-z.xmmreg.xmmrm32-er))

(setf (gethash "VFNMADD231SS-xmmreg-mask-z.xmmreg.xmmrm32-er" *x64-instruction-variants-hash-table*) (list
VFNMADD231SS-xmmreg-mask-z.xmmreg.xmmrm32-er))

(setf (gethash "VFNMADD231SS-xmmreg.xmmreg.xmmrm32" *x64-instruction-variants-hash-table*) (list
VFNMADD231SS-xmmreg.xmmreg.xmmrm32))

(setf (gethash "VFNMADD312PD" *x64-instruction-variants-hash-table*) (list
VFNMADD312PD-xmmreg.xmmreg.xmmrm128
VFNMADD312PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFNMADD312PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFNMADD312PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFNMADD312PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFNMADD312PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFNMADD312PS" *x64-instruction-variants-hash-table*) (list
VFNMADD312PS-xmmreg.xmmreg.xmmrm128
VFNMADD312PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFNMADD312PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFNMADD312PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFNMADD312PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFNMADD312PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFNMADD312SD" *x64-instruction-variants-hash-table*) (list
VFNMADD312SD-xmmreg.xmmreg.xmmrm64))

(setf (gethash "VFNMADD312SD-xmmreg.xmmreg.xmmrm64" *x64-instruction-variants-hash-table*) (list
VFNMADD312SD-xmmreg.xmmreg.xmmrm64))

(setf (gethash "VFNMADD312SS" *x64-instruction-variants-hash-table*) (list
VFNMADD312SS-xmmreg.xmmreg.xmmrm32))

(setf (gethash "VFNMADD312SS-xmmreg.xmmreg.xmmrm32" *x64-instruction-variants-hash-table*) (list
VFNMADD312SS-xmmreg.xmmreg.xmmrm32))

(setf (gethash "VFNMADD321PD" *x64-instruction-variants-hash-table*) (list
VFNMADD321PD-xmmreg.xmmreg.xmmrm128
VFNMADD321PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFNMADD321PD-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFNMADD321PD-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFNMADD321PD-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFNMADD321PD-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFNMADD321PS" *x64-instruction-variants-hash-table*) (list
VFNMADD321PS-xmmreg.xmmreg.xmmrm128
VFNMADD321PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFNMADD321PS-xmmreg.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFNMADD321PS-xmmreg.xmmreg.xmmrm128))

(setf (gethash "VFNMADD321PS-ymmreg.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFNMADD321PS-ymmreg.ymmreg.ymmrm256))

(setf (gethash "VFNMADD321SD" *x64-instruction-variants-hash-table*) (list
VFNMADD321SD-xmmreg.xmmreg.xmmrm64))

(setf (gethash "VFNMADD321SD-xmmreg.xmmreg.xmmrm64" *x64-instruction-variants-hash-table*) (list
VFNMADD321SD-xmmreg.xmmreg.xmmrm64))

(setf (gethash "VFNMADD321SS" *x64-instruction-variants-hash-table*) (list
VFNMADD321SS-xmmreg.xmmreg.xmmrm32))

(setf (gethash "VFNMADD321SS-xmmreg.xmmreg.xmmrm32" *x64-instruction-variants-hash-table*) (list
VFNMADD321SS-xmmreg.xmmreg.xmmrm32))

(setf (gethash "VFNMADDPD" *x64-instruction-variants-hash-table*) (list
VFNMADDPD-xmmreg.xmmreg*.xmmrm128.xmmreg
VFNMADDPD-ymmreg.ymmreg*.ymmrm256.ymmreg
VFNMADDPD-xmmreg.xmmreg*.xmmreg.xmmrm128
VFNMADDPD-ymmreg.ymmreg*.ymmreg.ymmrm256))

(setf (gethash "VFNMADDPD-xmmreg.xmmreg*.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFNMADDPD-xmmreg.xmmreg*.xmmreg.xmmrm128))

(setf (gethash "VFNMADDPD-xmmreg.xmmreg*.xmmrm128.xmmreg" *x64-instruction-variants-hash-table*) (list
VFNMADDPD-xmmreg.xmmreg*.xmmrm128.xmmreg))

(setf (gethash "VFNMADDPD-ymmreg.ymmreg*.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFNMADDPD-ymmreg.ymmreg*.ymmreg.ymmrm256))

(setf (gethash "VFNMADDPD-ymmreg.ymmreg*.ymmrm256.ymmreg" *x64-instruction-variants-hash-table*) (list
VFNMADDPD-ymmreg.ymmreg*.ymmrm256.ymmreg))

(setf (gethash "VFNMADDPS" *x64-instruction-variants-hash-table*) (list
VFNMADDPS-xmmreg.xmmreg*.xmmrm128.xmmreg
VFNMADDPS-ymmreg.ymmreg*.ymmrm256.ymmreg
VFNMADDPS-xmmreg.xmmreg*.xmmreg.xmmrm128
VFNMADDPS-ymmreg.ymmreg*.ymmreg.ymmrm256))

(setf (gethash "VFNMADDPS-xmmreg.xmmreg*.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VFNMADDPS-xmmreg.xmmreg*.xmmreg.xmmrm128))

(setf (gethash "VFNMADDPS-xmmreg.xmmreg*.xmmrm128.xmmreg" *x64-instruction-variants-hash-table*) (list
VFNMADDPS-xmmreg.xmmreg*.xmmrm128.xmmreg))

(setf (gethash "VFNMADDPS-ymmreg.ymmreg*.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VFNMADDPS-ymmreg.ymmreg*.ymmreg.ymmrm256))

(setf (gethash "VFNMADDPS-ymmreg.ymmreg*.ymmrm256.ymmreg" *x64-instruction-variants-hash-table*) (list
VFNMADDPS-ymmreg.ymmreg*.ymmrm256.ymmreg))

(setf (gethash "VFNMADDSD" *x64-instruction-variants-hash-table*) (list
VFNMADDSD-xmmreg.xmmreg*.xmmrm64.xmmreg
VFNMADDSD-xmmreg.xmmreg*.xmmreg.xmmrm64))

(setf (gethash "VFNMADDSD-xmmreg.xmmreg*.xmmreg.xmmrm64" *x64-instruction-variants-hash-table*) (list
VFNMADDSD-xmmreg.xmmreg*.xmmreg.xmmrm64))

(setf (gethash "VFNMADDSD-xmmreg.xmmreg*.xmmrm64.xmmreg" *x64-instruction-variants-hash-table*) (list
VFNMADDSD-xmmreg.xmmreg*.xmmrm64.xmmreg))

(setf (gethash "VFNMADDSS" *x64-instruction-variants-hash-table*) (list
VFNMADDSS-xmmreg.xmmreg*.xmmrm32.xmmreg
VFNMADDSS-xmmreg.xmmreg*.xmmreg.xmmrm32))

(setf (gethash "VFNMADDSS-xmmreg.xmmreg*.xmmreg.xmmrm32" *x64-instruction-variants-hash-table*) (list
VFNMADDSS-xmmreg.xmmreg*.xmmreg.xmmrm32))

(setf (gethash "VFNMADDSS-xmmreg.xmmreg*.xmmrm32.xmmreg" *x64-instruction-variants-hash-table*) (list
VFNMADDSS-xmmreg.xmmreg*.xmmrm32.xmmreg))

(setf (gethash "VHADDPD" *x64-instruction-variants-hash-table*) (list
VHADDPD-xmmreg.xmmreg*.xmmrm128
VHADDPD-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VHADDPD-xmmreg.xmmreg*.xmmrm128" *x64-instruction-variants-hash-table*) (list
VHADDPD-xmmreg.xmmreg*.xmmrm128))

(setf (gethash "VHADDPD-ymmreg.ymmreg*.ymmrm256" *x64-instruction-variants-hash-table*) (list
VHADDPD-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VHADDPS" *x64-instruction-variants-hash-table*) (list
VHADDPS-xmmreg.xmmreg*.xmmrm128
VHADDPS-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VHADDPS-xmmreg.xmmreg*.xmmrm128" *x64-instruction-variants-hash-table*) (list
VHADDPS-xmmreg.xmmreg*.xmmrm128))

(setf (gethash "VHADDPS-ymmreg.ymmreg*.ymmrm256" *x64-instruction-variants-hash-table*) (list
VHADDPS-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VPADDB" *x64-instruction-variants-hash-table*) (list
VPADDB-xmmreg.xmmreg*.xmmrm128
VPADDB-ymmreg.ymmreg*.ymmrm256
VPADDB-xmmreg-mask-z.xmmreg.xmmrm128
VPADDB-ymmreg-mask-z.ymmreg.ymmrm256
VPADDB-zmmreg-mask-z.zmmreg.zmmrm512))

(setf (gethash "VPADDB-xmmreg-mask-z.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPADDB-xmmreg-mask-z.xmmreg.xmmrm128))

(setf (gethash "VPADDB-xmmreg.xmmreg*.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPADDB-xmmreg.xmmreg*.xmmrm128))

(setf (gethash "VPADDB-ymmreg-mask-z.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPADDB-ymmreg-mask-z.ymmreg.ymmrm256))

(setf (gethash "VPADDB-ymmreg.ymmreg*.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPADDB-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VPADDB-zmmreg-mask-z.zmmreg.zmmrm512" *x64-instruction-variants-hash-table*) (list
VPADDB-zmmreg-mask-z.zmmreg.zmmrm512))

(setf (gethash "VPADDD" *x64-instruction-variants-hash-table*) (list
VPADDD-xmmreg.xmmreg*.xmmrm128
VPADDD-ymmreg.ymmreg*.ymmrm256
VPADDD-xmmreg-mask-z.xmmreg.xmmrm128-b32
VPADDD-ymmreg-mask-z.ymmreg.ymmrm256-b32
VPADDD-zmmreg-mask-z.zmmreg.zmmrm512-b32))

(setf (gethash "VPADDD-xmmreg-mask-z.xmmreg.xmmrm128-b32" *x64-instruction-variants-hash-table*) (list
VPADDD-xmmreg-mask-z.xmmreg.xmmrm128-b32))

(setf (gethash "VPADDD-xmmreg.xmmreg*.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPADDD-xmmreg.xmmreg*.xmmrm128))

(setf (gethash "VPADDD-ymmreg-mask-z.ymmreg.ymmrm256-b32" *x64-instruction-variants-hash-table*) (list
VPADDD-ymmreg-mask-z.ymmreg.ymmrm256-b32))

(setf (gethash "VPADDD-ymmreg.ymmreg*.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPADDD-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VPADDD-zmmreg-mask-z.zmmreg.zmmrm512-b32" *x64-instruction-variants-hash-table*) (list
VPADDD-zmmreg-mask-z.zmmreg.zmmrm512-b32))

(setf (gethash "VPADDQ" *x64-instruction-variants-hash-table*) (list
VPADDQ-xmmreg.xmmreg*.xmmrm128
VPADDQ-ymmreg.ymmreg*.ymmrm256
VPADDQ-xmmreg-mask-z.xmmreg.xmmrm128-b64
VPADDQ-ymmreg-mask-z.ymmreg.ymmrm256-b64
VPADDQ-zmmreg-mask-z.zmmreg.zmmrm512-b64))

(setf (gethash "VPADDQ-xmmreg-mask-z.xmmreg.xmmrm128-b64" *x64-instruction-variants-hash-table*) (list
VPADDQ-xmmreg-mask-z.xmmreg.xmmrm128-b64))

(setf (gethash "VPADDQ-xmmreg.xmmreg*.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPADDQ-xmmreg.xmmreg*.xmmrm128))

(setf (gethash "VPADDQ-ymmreg-mask-z.ymmreg.ymmrm256-b64" *x64-instruction-variants-hash-table*) (list
VPADDQ-ymmreg-mask-z.ymmreg.ymmrm256-b64))

(setf (gethash "VPADDQ-ymmreg.ymmreg*.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPADDQ-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VPADDQ-zmmreg-mask-z.zmmreg.zmmrm512-b64" *x64-instruction-variants-hash-table*) (list
VPADDQ-zmmreg-mask-z.zmmreg.zmmrm512-b64))

(setf (gethash "VPADDSB" *x64-instruction-variants-hash-table*) (list
VPADDSB-xmmreg.xmmreg*.xmmrm128
VPADDSB-ymmreg.ymmreg*.ymmrm256
VPADDSB-xmmreg-mask-z.xmmreg.xmmrm128
VPADDSB-ymmreg-mask-z.ymmreg.ymmrm256
VPADDSB-zmmreg-mask-z.zmmreg.zmmrm512))

(setf (gethash "VPADDSB-xmmreg-mask-z.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPADDSB-xmmreg-mask-z.xmmreg.xmmrm128))

(setf (gethash "VPADDSB-xmmreg.xmmreg*.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPADDSB-xmmreg.xmmreg*.xmmrm128))

(setf (gethash "VPADDSB-ymmreg-mask-z.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPADDSB-ymmreg-mask-z.ymmreg.ymmrm256))

(setf (gethash "VPADDSB-ymmreg.ymmreg*.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPADDSB-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VPADDSB-zmmreg-mask-z.zmmreg.zmmrm512" *x64-instruction-variants-hash-table*) (list
VPADDSB-zmmreg-mask-z.zmmreg.zmmrm512))

(setf (gethash "VPADDSW" *x64-instruction-variants-hash-table*) (list
VPADDSW-xmmreg.xmmreg*.xmmrm128
VPADDSW-ymmreg.ymmreg*.ymmrm256
VPADDSW-xmmreg-mask-z.xmmreg.xmmrm128
VPADDSW-ymmreg-mask-z.ymmreg.ymmrm256
VPADDSW-zmmreg-mask-z.zmmreg.zmmrm512))

(setf (gethash "VPADDSW-xmmreg-mask-z.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPADDSW-xmmreg-mask-z.xmmreg.xmmrm128))

(setf (gethash "VPADDSW-xmmreg.xmmreg*.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPADDSW-xmmreg.xmmreg*.xmmrm128))

(setf (gethash "VPADDSW-ymmreg-mask-z.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPADDSW-ymmreg-mask-z.ymmreg.ymmrm256))

(setf (gethash "VPADDSW-ymmreg.ymmreg*.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPADDSW-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VPADDSW-zmmreg-mask-z.zmmreg.zmmrm512" *x64-instruction-variants-hash-table*) (list
VPADDSW-zmmreg-mask-z.zmmreg.zmmrm512))

(setf (gethash "VPADDUSB" *x64-instruction-variants-hash-table*) (list
VPADDUSB-xmmreg.xmmreg*.xmmrm128
VPADDUSB-ymmreg.ymmreg*.ymmrm256
VPADDUSB-xmmreg-mask-z.xmmreg.xmmrm128
VPADDUSB-ymmreg-mask-z.ymmreg.ymmrm256
VPADDUSB-zmmreg-mask-z.zmmreg.zmmrm512))

(setf (gethash "VPADDUSB-xmmreg-mask-z.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPADDUSB-xmmreg-mask-z.xmmreg.xmmrm128))

(setf (gethash "VPADDUSB-xmmreg.xmmreg*.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPADDUSB-xmmreg.xmmreg*.xmmrm128))

(setf (gethash "VPADDUSB-ymmreg-mask-z.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPADDUSB-ymmreg-mask-z.ymmreg.ymmrm256))

(setf (gethash "VPADDUSB-ymmreg.ymmreg*.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPADDUSB-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VPADDUSB-zmmreg-mask-z.zmmreg.zmmrm512" *x64-instruction-variants-hash-table*) (list
VPADDUSB-zmmreg-mask-z.zmmreg.zmmrm512))

(setf (gethash "VPADDUSW" *x64-instruction-variants-hash-table*) (list
VPADDUSW-xmmreg.xmmreg*.xmmrm128
VPADDUSW-ymmreg.ymmreg*.ymmrm256
VPADDUSW-xmmreg-mask-z.xmmreg.xmmrm128
VPADDUSW-ymmreg-mask-z.ymmreg.ymmrm256
VPADDUSW-zmmreg-mask-z.zmmreg.zmmrm512))

(setf (gethash "VPADDUSW-xmmreg-mask-z.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPADDUSW-xmmreg-mask-z.xmmreg.xmmrm128))

(setf (gethash "VPADDUSW-xmmreg.xmmreg*.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPADDUSW-xmmreg.xmmreg*.xmmrm128))

(setf (gethash "VPADDUSW-ymmreg-mask-z.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPADDUSW-ymmreg-mask-z.ymmreg.ymmrm256))

(setf (gethash "VPADDUSW-ymmreg.ymmreg*.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPADDUSW-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VPADDUSW-zmmreg-mask-z.zmmreg.zmmrm512" *x64-instruction-variants-hash-table*) (list
VPADDUSW-zmmreg-mask-z.zmmreg.zmmrm512))

(setf (gethash "VPADDW" *x64-instruction-variants-hash-table*) (list
VPADDW-xmmreg.xmmreg*.xmmrm128
VPADDW-ymmreg.ymmreg*.ymmrm256
VPADDW-xmmreg-mask-z.xmmreg.xmmrm128
VPADDW-ymmreg-mask-z.ymmreg.ymmrm256
VPADDW-zmmreg-mask-z.zmmreg.zmmrm512))

(setf (gethash "VPADDW-xmmreg-mask-z.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPADDW-xmmreg-mask-z.xmmreg.xmmrm128))

(setf (gethash "VPADDW-xmmreg.xmmreg*.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPADDW-xmmreg.xmmreg*.xmmrm128))

(setf (gethash "VPADDW-ymmreg-mask-z.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPADDW-ymmreg-mask-z.ymmreg.ymmrm256))

(setf (gethash "VPADDW-ymmreg.ymmreg*.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPADDW-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VPADDW-zmmreg-mask-z.zmmreg.zmmrm512" *x64-instruction-variants-hash-table*) (list
VPADDW-zmmreg-mask-z.zmmreg.zmmrm512))

(setf (gethash "VPHADDBD" *x64-instruction-variants-hash-table*) (list
VPHADDBD-xmmreg.xmmrm128*))

(setf (gethash "VPHADDBD-xmmreg.xmmrm128*" *x64-instruction-variants-hash-table*) (list
VPHADDBD-xmmreg.xmmrm128*))

(setf (gethash "VPHADDBQ" *x64-instruction-variants-hash-table*) (list
VPHADDBQ-xmmreg.xmmrm128*))

(setf (gethash "VPHADDBQ-xmmreg.xmmrm128*" *x64-instruction-variants-hash-table*) (list
VPHADDBQ-xmmreg.xmmrm128*))

(setf (gethash "VPHADDBW" *x64-instruction-variants-hash-table*) (list
VPHADDBW-xmmreg.xmmrm128*))

(setf (gethash "VPHADDBW-xmmreg.xmmrm128*" *x64-instruction-variants-hash-table*) (list
VPHADDBW-xmmreg.xmmrm128*))

(setf (gethash "VPHADDD" *x64-instruction-variants-hash-table*) (list
VPHADDD-xmmreg.xmmreg*.xmmrm128
VPHADDD-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VPHADDD-xmmreg.xmmreg*.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPHADDD-xmmreg.xmmreg*.xmmrm128))

(setf (gethash "VPHADDD-ymmreg.ymmreg*.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPHADDD-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VPHADDDQ" *x64-instruction-variants-hash-table*) (list
VPHADDDQ-xmmreg.xmmrm128*))

(setf (gethash "VPHADDDQ-xmmreg.xmmrm128*" *x64-instruction-variants-hash-table*) (list
VPHADDDQ-xmmreg.xmmrm128*))

(setf (gethash "VPHADDSW" *x64-instruction-variants-hash-table*) (list
VPHADDSW-xmmreg.xmmreg*.xmmrm128
VPHADDSW-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VPHADDSW-xmmreg.xmmreg*.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPHADDSW-xmmreg.xmmreg*.xmmrm128))

(setf (gethash "VPHADDSW-ymmreg.ymmreg*.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPHADDSW-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VPHADDUBD" *x64-instruction-variants-hash-table*) (list
VPHADDUBD-xmmreg.xmmrm128*))

(setf (gethash "VPHADDUBD-xmmreg.xmmrm128*" *x64-instruction-variants-hash-table*) (list
VPHADDUBD-xmmreg.xmmrm128*))

(setf (gethash "VPHADDUBQ" *x64-instruction-variants-hash-table*) (list
VPHADDUBQ-xmmreg.xmmrm128*))

(setf (gethash "VPHADDUBQ-xmmreg.xmmrm128*" *x64-instruction-variants-hash-table*) (list
VPHADDUBQ-xmmreg.xmmrm128*))

(setf (gethash "VPHADDUBW" *x64-instruction-variants-hash-table*) (list
VPHADDUBW-xmmreg.xmmrm128*))

(setf (gethash "VPHADDUBW-xmmreg.xmmrm128*" *x64-instruction-variants-hash-table*) (list
VPHADDUBW-xmmreg.xmmrm128*))

(setf (gethash "VPHADDUDQ" *x64-instruction-variants-hash-table*) (list
VPHADDUDQ-xmmreg.xmmrm128*))

(setf (gethash "VPHADDUDQ-xmmreg.xmmrm128*" *x64-instruction-variants-hash-table*) (list
VPHADDUDQ-xmmreg.xmmrm128*))

(setf (gethash "VPHADDUWD" *x64-instruction-variants-hash-table*) (list
VPHADDUWD-xmmreg.xmmrm128*))

(setf (gethash "VPHADDUWD-xmmreg.xmmrm128*" *x64-instruction-variants-hash-table*) (list
VPHADDUWD-xmmreg.xmmrm128*))

(setf (gethash "VPHADDUWQ" *x64-instruction-variants-hash-table*) (list
VPHADDUWQ-xmmreg.xmmrm128*))

(setf (gethash "VPHADDUWQ-xmmreg.xmmrm128*" *x64-instruction-variants-hash-table*) (list
VPHADDUWQ-xmmreg.xmmrm128*))

(setf (gethash "VPHADDW" *x64-instruction-variants-hash-table*) (list
VPHADDW-xmmreg.xmmreg*.xmmrm128
VPHADDW-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VPHADDW-xmmreg.xmmreg*.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPHADDW-xmmreg.xmmreg*.xmmrm128))

(setf (gethash "VPHADDW-ymmreg.ymmreg*.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPHADDW-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VPHADDWD" *x64-instruction-variants-hash-table*) (list
VPHADDWD-xmmreg.xmmrm128*))

(setf (gethash "VPHADDWD-xmmreg.xmmrm128*" *x64-instruction-variants-hash-table*) (list
VPHADDWD-xmmreg.xmmrm128*))

(setf (gethash "VPHADDWQ" *x64-instruction-variants-hash-table*) (list
VPHADDWQ-xmmreg.xmmrm128*))

(setf (gethash "VPHADDWQ-xmmreg.xmmrm128*" *x64-instruction-variants-hash-table*) (list
VPHADDWQ-xmmreg.xmmrm128*))

(setf (gethash "VPMADD52HUQ" *x64-instruction-variants-hash-table*) (list
VPMADD52HUQ-xmmreg-mask-z.xmmreg.xmmrm128-b64
VPMADD52HUQ-ymmreg-mask-z.ymmreg.ymmrm256-b64
VPMADD52HUQ-zmmreg-mask-z.zmmreg.zmmrm512-b64))

(setf (gethash "VPMADD52HUQ-xmmreg-mask-z.xmmreg.xmmrm128-b64" *x64-instruction-variants-hash-table*) (list
VPMADD52HUQ-xmmreg-mask-z.xmmreg.xmmrm128-b64))

(setf (gethash "VPMADD52HUQ-ymmreg-mask-z.ymmreg.ymmrm256-b64" *x64-instruction-variants-hash-table*) (list
VPMADD52HUQ-ymmreg-mask-z.ymmreg.ymmrm256-b64))

(setf (gethash "VPMADD52HUQ-zmmreg-mask-z.zmmreg.zmmrm512-b64" *x64-instruction-variants-hash-table*) (list
VPMADD52HUQ-zmmreg-mask-z.zmmreg.zmmrm512-b64))

(setf (gethash "VPMADD52LUQ" *x64-instruction-variants-hash-table*) (list
VPMADD52LUQ-xmmreg-mask-z.xmmreg.xmmrm128-b64
VPMADD52LUQ-ymmreg-mask-z.ymmreg.ymmrm256-b64
VPMADD52LUQ-zmmreg-mask-z.zmmreg.zmmrm512-b64))

(setf (gethash "VPMADD52LUQ-xmmreg-mask-z.xmmreg.xmmrm128-b64" *x64-instruction-variants-hash-table*) (list
VPMADD52LUQ-xmmreg-mask-z.xmmreg.xmmrm128-b64))

(setf (gethash "VPMADD52LUQ-ymmreg-mask-z.ymmreg.ymmrm256-b64" *x64-instruction-variants-hash-table*) (list
VPMADD52LUQ-ymmreg-mask-z.ymmreg.ymmrm256-b64))

(setf (gethash "VPMADD52LUQ-zmmreg-mask-z.zmmreg.zmmrm512-b64" *x64-instruction-variants-hash-table*) (list
VPMADD52LUQ-zmmreg-mask-z.zmmreg.zmmrm512-b64))

(setf (gethash "VPMADDUBSW" *x64-instruction-variants-hash-table*) (list
VPMADDUBSW-xmmreg.xmmreg*.xmmrm128
VPMADDUBSW-ymmreg.ymmreg*.ymmrm256
VPMADDUBSW-xmmreg-mask-z.xmmreg.xmmrm128
VPMADDUBSW-ymmreg-mask-z.ymmreg.ymmrm256
VPMADDUBSW-zmmreg-mask-z.zmmreg.zmmrm512))

(setf (gethash "VPMADDUBSW-xmmreg-mask-z.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPMADDUBSW-xmmreg-mask-z.xmmreg.xmmrm128))

(setf (gethash "VPMADDUBSW-xmmreg.xmmreg*.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPMADDUBSW-xmmreg.xmmreg*.xmmrm128))

(setf (gethash "VPMADDUBSW-ymmreg-mask-z.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPMADDUBSW-ymmreg-mask-z.ymmreg.ymmrm256))

(setf (gethash "VPMADDUBSW-ymmreg.ymmreg*.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPMADDUBSW-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VPMADDUBSW-zmmreg-mask-z.zmmreg.zmmrm512" *x64-instruction-variants-hash-table*) (list
VPMADDUBSW-zmmreg-mask-z.zmmreg.zmmrm512))

(setf (gethash "VPMADDWD" *x64-instruction-variants-hash-table*) (list
VPMADDWD-xmmreg.xmmreg*.xmmrm128
VPMADDWD-ymmreg.ymmreg*.ymmrm256
VPMADDWD-xmmreg-mask-z.xmmreg.xmmrm128
VPMADDWD-ymmreg-mask-z.ymmreg.ymmrm256
VPMADDWD-zmmreg-mask-z.zmmreg.zmmrm512))

(setf (gethash "VPMADDWD-xmmreg-mask-z.xmmreg.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPMADDWD-xmmreg-mask-z.xmmreg.xmmrm128))

(setf (gethash "VPMADDWD-xmmreg.xmmreg*.xmmrm128" *x64-instruction-variants-hash-table*) (list
VPMADDWD-xmmreg.xmmreg*.xmmrm128))

(setf (gethash "VPMADDWD-ymmreg-mask-z.ymmreg.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPMADDWD-ymmreg-mask-z.ymmreg.ymmrm256))

(setf (gethash "VPMADDWD-ymmreg.ymmreg*.ymmrm256" *x64-instruction-variants-hash-table*) (list
VPMADDWD-ymmreg.ymmreg*.ymmrm256))

(setf (gethash "VPMADDWD-zmmreg-mask-z.zmmreg.zmmrm512" *x64-instruction-variants-hash-table*) (list
VPMADDWD-zmmreg-mask-z.zmmreg.zmmrm512))

(setf (gethash "XADD" *x64-instruction-variants-hash-table*) (list
XADD-mem.reg8
XADD-reg8.reg8-mr
XADD-mem.reg16
XADD-reg16.reg16-mr
XADD-mem.reg32
XADD-reg32.reg32-mr
XADD-mem.reg64
XADD-reg64.reg64-mr))

(setf (gethash "XADD-mem.reg16" *x64-instruction-variants-hash-table*) (list
XADD-mem.reg16))

(setf (gethash "XADD-mem.reg32" *x64-instruction-variants-hash-table*) (list
XADD-mem.reg32))

(setf (gethash "XADD-mem.reg64" *x64-instruction-variants-hash-table*) (list
XADD-mem.reg64))

(setf (gethash "XADD-mem.reg8" *x64-instruction-variants-hash-table*) (list
XADD-mem.reg8))

(setf (gethash "XADD-reg16.reg16-mr" *x64-instruction-variants-hash-table*) (list
XADD-reg16.reg16-mr))

(setf (gethash "XADD-reg32.reg32-mr" *x64-instruction-variants-hash-table*) (list
XADD-reg32.reg32-mr))

(setf (gethash "XADD-reg64.reg64-mr" *x64-instruction-variants-hash-table*) (list
XADD-reg64.reg64-mr))

(setf (gethash "XADD-reg8.reg8-mr" *x64-instruction-variants-hash-table*) (list
XADD-reg8.reg8-mr))
