;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defparameter *emit-function-hash-table-x64* (make-hash-table :test 'equalp))
;;; pseudo-ops.
(setf (gethash "[bits"      *emit-function-hash-table-x64*) (list #'bits-pseudo-op))
;;; segment registers.
(setf (gethash "cs:"        *emit-function-hash-table-x64*) (list #'cs-x86))
(setf (gethash "ds:"        *emit-function-hash-table-x64*) (list #'ds-x86))
(setf (gethash "es:"        *emit-function-hash-table-x64*) (list #'es-x86))
(setf (gethash "fs:"        *emit-function-hash-table-x64*) (list #'fs-x86))
(setf (gethash "gs:"        *emit-function-hash-table-x64*) (list #'gs-x86))
(setf (gethash "ss:"        *emit-function-hash-table-x64*) (list #'ss-x86))
                           
;;; instructions in alphaaabetical order.
(setf (gethash "adc"        *emit-function-hash-table-x64*) (list #'adc-x64))
(setf (gethash "adc-reg-rm" *emit-function-hash-table-x64*) (list #'adc-reg-rm-x64))
(setf (gethash "adc-rm-reg" *emit-function-hash-table-x64*) (list #'adc-rm-reg-x64))
(setf (gethash "add"        *emit-function-hash-table-x64*) (list #'add-x64))
(setf (gethash "add-reg-rm" *emit-function-hash-table-x64*) (list #'add-reg-rm-x64))
(setf (gethash "add-rm-reg" *emit-function-hash-table-x64*) (list #'add-rm-reg-x64))
(setf (gethash "and"        *emit-function-hash-table-x64*) (list #'and-x64))
(setf (gethash "and-reg-rm" *emit-function-hash-table-x64*) (list #'and-reg-rm-x64))
(setf (gethash "and-rm-reg" *emit-function-hash-table-x64*) (list #'and-rm-reg-x64))
(setf (gethash "clc"        *emit-function-hash-table-x64*) (list #'clc-x86))
(setf (gethash "cld"        *emit-function-hash-table-x64*) (list #'cld-x86))
(setf (gethash "cli"        *emit-function-hash-table-x64*) (list #'cli-x86))
(setf (gethash "cmc"        *emit-function-hash-table-x64*) (list #'cmc-x86))
(setf (gethash "cmp"        *emit-function-hash-table-x64*) (list #'cmp-x64))
(setf (gethash "cmp-reg-rm" *emit-function-hash-table-x64*) (list #'cmp-reg-rm-x64))
(setf (gethash "cmp-rm-reg" *emit-function-hash-table-x64*) (list #'cmp-rm-reg-x64))
(setf (gethash "cmpsb"      *emit-function-hash-table-x64*) (list #'cmpsb-x86))
(setf (gethash "cmpsd"      *emit-function-hash-table-x64*) (list #'cmpsd-x32-x64))
(setf (gethash "cmpsq"      *emit-function-hash-table-x64*) (list #'cmpsq-48-x64 #'cmpsq-49-x64 #'cmpsq-4a-x64 #'cmpsq-4b-x64 #'cmpsq-4c-x64 #'cmpsq-4d-x64 #'cmpsq-4e-x64 #'cmpsq-4f-x64))
(setf (gethash "cmpsw"      *emit-function-hash-table-x64*) (list #'cmpsw-x86))
(setf (gethash "dec"        *emit-function-hash-table-x64*) (list #'dec-x64))
(setf (gethash "hlt"        *emit-function-hash-table-x64*) (list #'hlt-x86))
(setf (gethash "in"         *emit-function-hash-table-x64*) (list #'in-x32-x64))
(setf (gethash "inc"        *emit-function-hash-table-x64*) (list #'inc-x64))
(setf (gethash "insb"       *emit-function-hash-table-x64*) (list #'insb-x86))
(setf (gethash "insd"       *emit-function-hash-table-x64*) (list #'insd-x32-x64))
(setf (gethash "insw"       *emit-function-hash-table-x64*) (list #'insw-x86))
(setf (gethash "lea"        *emit-function-hash-table-x64*) (list #'lea-x64))
(setf (gethash "lodsb"      *emit-function-hash-table-x64*) (list #'lodsb-x86))
(setf (gethash "lodsd"      *emit-function-hash-table-x64*) (list #'lodsd-x32-x64))
(setf (gethash "lodsq"      *emit-function-hash-table-x64*) (list #'lodsq-48-x64 #'lodsq-49-x64 #'lodsq-4a-x64 #'lodsq-4b-x64 #'lodsq-4c-x64 #'lodsq-4d-x64 #'lodsq-4e-x64 #'lodsq-4f-x64))
(setf (gethash "lodsw"      *emit-function-hash-table-x64*) (list #'lodsw-x86))
(setf (gethash "neg"        *emit-function-hash-table-x64*) (list #'neg-x64))
(setf (gethash "nop"        *emit-function-hash-table-x64*) (list #'nop-x86))
(setf (gethash "not"        *emit-function-hash-table-x64*) (list #'not-x64))
(setf (gethash "or"         *emit-function-hash-table-x64*) (list #'or-x64))
(setf (gethash "or-reg-rm"  *emit-function-hash-table-x64*) (list #'or-reg-rm-x64))
(setf (gethash "or-rm-reg"  *emit-function-hash-table-x64*) (list #'or-rm-reg-x64))
(setf (gethash "rep"        *emit-function-hash-table-x64*) (list #'rep-repz-x32-x64))
(setf (gethash "rcl-1"      *emit-function-hash-table-x64*) (list #'rcl-1-x64))
(setf (gethash "rcl-cl"     *emit-function-hash-table-x64*) (list #'rcl-cl-x64))
(setf (gethash "rcr-1"      *emit-function-hash-table-x64*) (list #'rcr-1-x64))
(setf (gethash "rcr-cl"     *emit-function-hash-table-x64*) (list #'rcr-cl-x64))
(setf (gethash "rol-1"      *emit-function-hash-table-x64*) (list #'rol-1-x64))
(setf (gethash "rol-cl"     *emit-function-hash-table-x64*) (list #'rol-cl-x64))
(setf (gethash "ror-1"      *emit-function-hash-table-x64*) (list #'ror-1-x64))
(setf (gethash "ror-cl"     *emit-function-hash-table-x64*) (list #'ror-cl-x64))
(setf (gethash "repe"       *emit-function-hash-table-x64*) (list #'rep-repz-x32-x64))
(setf (gethash "repne"      *emit-function-hash-table-x64*) (list #'repnz-x32-x64))
(setf (gethash "repnz"      *emit-function-hash-table-x64*) (list #'repnz-x32-x64))
(setf (gethash "repz"       *emit-function-hash-table-x64*) (list #'rep-repz-x32-x64))
(setf (gethash "sal-1"      *emit-function-hash-table-x64*) (list #'shl-1-x64))
(setf (gethash "sal-cl"     *emit-function-hash-table-x64*) (list #'shl-cl-x64))
(setf (gethash "sar-1"      *emit-function-hash-table-x64*) (list #'sar-1-x64))
(setf (gethash "sar-cl"     *emit-function-hash-table-x64*) (list #'sar-cl-x64))
(setf (gethash "shl-1"      *emit-function-hash-table-x64*) (list #'shl-1-x64))
(setf (gethash "shl-cl"     *emit-function-hash-table-x64*) (list #'shl-cl-x64))
(setf (gethash "shr-1"      *emit-function-hash-table-x64*) (list #'shr-1-x64))
(setf (gethash "shr-cl"     *emit-function-hash-table-x64*) (list #'shr-cl-x64))
(setf (gethash "out"        *emit-function-hash-table-x64*) (list #'out-x32-x64))
(setf (gethash "outsb"      *emit-function-hash-table-x64*) (list #'outsb-x86))
(setf (gethash "outsd"      *emit-function-hash-table-x64*) (list #'outsd-x32-x64))
(setf (gethash "outsw"      *emit-function-hash-table-x64*) (list #'outsw-x86))
(setf (gethash "pop"        *emit-function-hash-table-x64*) (list #'pop-x64))
(setf (gethash "push"       *emit-function-hash-table-x64*) (list #'push-x64))
(setf (gethash "sbb"        *emit-function-hash-table-x64*) (list #'sbb-x64))
(setf (gethash "sbb-reg-rm" *emit-function-hash-table-x64*) (list #'sbb-reg-rm-x64))
(setf (gethash "sbb-rm-reg" *emit-function-hash-table-x64*) (list #'sbb-rm-reg-x64))
(setf (gethash "scasb"      *emit-function-hash-table-x64*) (list #'scasb-x86))
(setf (gethash "scasd"      *emit-function-hash-table-x64*) (list #'scasd-x32-x64))
(setf (gethash "scasq"      *emit-function-hash-table-x64*) (list #'scasq-48-x64 #'scasq-49-x64 #'scasq-4a-x64 #'scasq-4b-x64 #'scasq-4c-x64 #'scasq-4d-x64 #'scasq-4e-x64 #'scasq-4f-x64))
(setf (gethash "scasw"      *emit-function-hash-table-x64*) (list #'scasw-x86))
(setf (gethash "stc"        *emit-function-hash-table-x64*) (list #'stc-x86))
(setf (gethash "std"        *emit-function-hash-table-x64*) (list #'std-x86))
(setf (gethash "sti"        *emit-function-hash-table-x64*) (list #'sti-x86))
(setf (gethash "stosb"      *emit-function-hash-table-x64*) (list #'stosb-x86))
(setf (gethash "stosd"      *emit-function-hash-table-x64*) (list #'stosd-x32-x64))
(setf (gethash "stosq"      *emit-function-hash-table-x64*) (list #'stosq-48-x64 #'stosq-49-x64 #'stosq-4a-x64 #'stosq-4b-x64 #'stosq-4c-x64 #'stosq-4d-x64 #'stosq-4e-x64 #'stosq-4f-x64))
(setf (gethash "stosw"      *emit-function-hash-table-x64*) (list #'stosw-x86))
(setf (gethash "sub"        *emit-function-hash-table-x64*) (list #'sub-x64))
(setf (gethash "sub-reg-rm" *emit-function-hash-table-x64*) (list #'sub-reg-rm-x64))
(setf (gethash "sub-rm-reg" *emit-function-hash-table-x64*) (list #'sub-rm-reg-x64))
(setf (gethash "syscall"    *emit-function-hash-table-x64*) (list #'syscall-x64))
(setf (gethash "mov"        *emit-function-hash-table-x64*) (list #'mov-x64))
(setf (gethash "mov-reg-rm" *emit-function-hash-table-x64*) (list #'mov-reg-rm-x64))
(setf (gethash "mov-rm-reg" *emit-function-hash-table-x64*) (list #'mov-rm-reg-x64))
(setf (gethash "movsb"      *emit-function-hash-table-x64*) (list #'movsb-x86))
(setf (gethash "movsd"      *emit-function-hash-table-x64*) (list #'movsd-x32-x64))
(setf (gethash "movsq"      *emit-function-hash-table-x64*) (list #'movsq-48-x64 #'movsq-49-x64 #'movsq-4a-x64 #'movsq-4b-x64 #'movsq-4c-x64 #'movsq-4d-x64 #'movsq-4e-x64 #'movsq-4f-x64))
(setf (gethash "movsw"      *emit-function-hash-table-x64*) (list #'movsw-x86))
(setf (gethash "xor"        *emit-function-hash-table-x64*) (list #'xor-x64))
(setf (gethash "xor-reg-rm" *emit-function-hash-table-x64*) (list #'xor-reg-rm-x64))
(setf (gethash "xor-rm-reg" *emit-function-hash-table-x64*) (list #'xor-rm-reg-x64))
