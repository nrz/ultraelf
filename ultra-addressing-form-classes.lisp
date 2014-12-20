;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

;; order of class definitions: most general first, subclasses of same level in alphanumeric order.
;; is-something defined `:initform nil` in the upper more general classes and `:initform t` in the relevant class.
;; usually there should not be several `:initform nil` for the same slot.
;;
;;
;;                                                                                                           slots with _fixed_ values (fixed values should
;;                                                                                                           have `:allocation :class`) values and slots with
;;                                                                                                           _required_ values  (the required values should have
;;                                                                                                           `:initarg :foo` and
;;                                                                                                           `:initform (error "foo must be specified")`.
;;                                                                                                           this list of defined slots is still incomplete!
;;                                                                                                           TODO: fill in the missing slot documentation below!
;; classes that are only used to define some variable values, do not have parents.
;;
;; x64-rex-r-0 ()                                                                                            `rex-r 0`.
;; x64-rex-r-1 ()                                                                                            `rex-r 1`.
;;
;; x64-rex-b-0 ()                                                                                            `rex-b 0`.
;; x64-rex-b-1 ()                                                                                            `rex-b 1`.
;;
;; `argument` class and classes that inherit it.
;;
;; argument ()                                                                                               `name` (required).
;;   ...
;; 2 addressing-form (argument)                                                                              `is-reg nil`, `is-memory-addressing nil`,
;;     ...                                                                                                   `is-abs-addressing nil`, `is-pc-relative nil`, ...
;;     ...                                                                                                   `is-reg-indirect nil`, `displacement-size 0`,
;;     ...                                                                                                   `r/m` (required).
;;   3 arm-addressing-form (addressing-form)
;;   3 memory-addressing (addressing-form)                                                                   `is-memory-addressing  t`.
;;     4 abs-addressing (memory-addressing)
;;       5 x86-abs-addressing (abs-addressing x86-memory-addressing)
;;         6 x86-abs-disp32 (x86-abs-addressing)
;;       5 arm-abs-addressing (memory-addressing arm-memory-addressing)
;;         ! subclasses of arm-abs-addressing not yet defined! TODO: define the subclasses!
;;     4 arm-memory-addressing (memory-addressing arm-addressing-form)
;;       5 ^ arm-abs-addressing (memory-addressing arm-addressing-form) ^ see above ^
;;       5 arm-literal-addressing (pc-relative arm-memory-addressing)
;;       5 arm-reg-indirect (reg-indirect arm-memory-addressing)                                             `works-with-rex t`.
;;     4 pc-relative (memory-addressing)
;;       5 ^ arm-literal-addressing (pc-relative arm-memory-addressing) ^ see above ^
;;       5 x64-rip-relative (pc-relative x86-memory-addressing)                                              `displacement-size 32`, `needs-sib nil`, `needs-rex nil`.
;;         6 x86-rip-disp32 (x64-rip-relative)
;;         6 x86-rip-disp32-0 (x64-rex-b-0 x64-rip-relative)
;;         6 x86-rip-disp32-1 (x64-rex-b-1 x64-rip-relative)
;;     4 reg-indirect (memory-addressing)
;;       5 ^ arm-reg-indirect (reg-indirect arm-memory-addressing) ^ see above ^
;;       5 x86-reg-indirect (reg-indirect x86-memory-addressing)                                             `works-with-rex t`.
;;         6 x16-reg-indirect (x86-reg-indirect)
;;           7 x16-reg-indirect-does-not-need-sib (x16-reg-indirect)
;;           7 x16-reg-indirect-needs-sib (needs-sib x16-reg-indirect)
;;         6 x32-reg-indirect (x86-reg-indirect)
;;           7 x32-reg-indirect-does-not-need-sib (x32-reg-indirect)
;;           7 x32-reg-indirect-needs-sib (needs-sib x32-reg-indirect)
;;         6 x64-reg-indirect (x86-reg-indirect)
;;           7 x64-new-reg-indirect (x86-new-reg-indirect x64-reg-indirect)
;;             8 x64-new-reg-indirect-does-not-need-sib (x64-new-reg-indirect)
;;             8 x64-new-reg-indirect-needs-sib (needs-sib x64-new-reg-indirect)
;;               9 x64-new-reg-indirect-sib-with-disp0 (x64-new-reg-indirect-needs-sib)
;;               9 x64-new-reg-indirect-sib-with-disp8 (x64-new-reg-indirect-needs-sib)
;;               9 x64-new-reg-indirect-sib-with-disp32 (x64-new-reg-indirect-needs-sib)
;;           7 x64-old-reg-indirect (x86-old-reg-indirect x64-reg-indirect)
;;             8 x64-old-reg-indirect-does-not-need-sib (x64-old-reg-indirect)
;;             8 x64-old-reg-indirect-needs-sib (needs-sib x64-old-reg-indirect)
;;           7 x64-reg-indirect-does-not-need-sib (x64-reg-indirect)
;;             8 ^ x64-new-reg-indirect-does-not-need-sib (x64-new-reg-indirect) ^ see above ^
;;             8 ^ x64-old-reg-indirect-does-not-need-sib (x64-old-reg-indirect) ^ see above ^
;;           7 x64-reg-indirect-needs-sib (needs-sib x64-reg-indirect)
;;             8 ^ x64-new-reg-indirect-needs-sib (needs-sib x64-new-reg-indirect) ^ see above ^
;;             8 ^ x64-old-reg-indirect-needs-sib (needs-sib x64-old-reg-indirect) ^ see above ^
;;         6 x86-new-reg-indirect (x64-rex-r-1 x64-rex-b-1 x86-reg-indirect)
;;         6 x86-old-reg-indirect (x64-rex-r-0 x64-rex-b-0 x86-reg-indirect)
;;         6 x86-reg-indirect-disp0 (x86-reg-indirect)                                                       `displacement-size 0`
;;         6 x86-reg-indirect-disp8 (x86-reg-indirect)                                                       `displacement-size 8`
;;         6 x86-reg-indirect-disp32 (x86-reg-indirect)                                                      `displacement-size 32`
;;         6 x86-reg-indirect-needs-sib (needs-sib x86-reg-indirect)
;;         6 x86-reg-indirect-does-not-need-sib (x86-reg-indirect)
;;     4 x86-memory-addressing (memory-addressing x86-addressing-form)
;;       5 ^ x86-abs-addressing (abs-addressing x86-memory-addressing) ^ see above ^
;;       5 ^ x86-reg-indirect (reg-indirect x86-memory-addressing) ^ see above ^
;;       5 ^ x64-rip-relative (pc-relative x86-memory-addressing) ^ see above ^
;;   3 reg (addressing-form)                                                                                 `is-reg t`, `reg-name`.
;;     4 arm-reg (reg arm-addressing-form)
;;     4 x86-reg (reg x86-addressing-form)
;;       5 x86-8-bits-reg (x86-reg)
;;         6 x86-new-8-bits-reg (x86-new-reg x86-8-bits-reg)
;;           7 x86-new-8-bits-reg-rex-r-0 (x64-rex-r-0 x64-rex-b-0 x86-new-8-bits-reg)
;;           7 x86-new-8-bits-reg-rex-r-1 (x64-rex-r-1 x64-rex-b-1 x86-new-8-bits-reg)
;;         6 x86-old-8-bits-high-reg (x86-old-reg x86-8-bits-reg)
;;         6 x86-old-8-bits-low-reg (x86-old-reg x86-8-bits-reg)
;;       5 x86-16-bits-reg (x86-reg)
;;         6 x86-old-16-bits-reg (x86-old-reg x86-16-bits-reg)
;;         6 x86-new-16-bits-reg (x64-rex-r-1 x64-rex-b-1 x86-new-reg x86-16-bits-reg)
;;       5 x86-32-bits-reg (x86-reg)
;;         6 x86-new-32-bits-reg (x64-rex-r-1 x64-rex-b-1 x86-new-reg x86-32-bits-reg)
;;         6 x86-old-32-bits-reg (x86-old-reg x86-32-bits-reg)
;;       5 x86-64-bits-reg (x86-reg)
;;         6 x86-new-64-bits-reg (x64-rex-r-1 x64-rex-b-1 x86-new-reg x86-64-bits-reg)
;;         6 x86-old-64-bits-reg (x86-old-reg x86-64-bits-reg)
;;       5 x86-mmx-reg (x86-reg)
;;       5 x86-new-reg (x86-reg)
;;         6 ^ x86-new-8-bits-reg (x86-new-reg x86-8-bits-reg) ^ see above ^
;;         6 ^ x86-new-16-bits-reg (x64-rex-r-1 x64-rex-b-1 x86-new-reg x86-16-bits-reg) ^ see above ^
;;         6 ^ x86-new-32-bits-reg (x64-rex-r-1 x64-rex-b-1 x86-new-reg x86-32-bits-reg) ^ see above ^
;;         6 ^ x86-new-64-bits-reg (x64-rex-r-1 x64-rex-b-1 x86-new-reg x86-64-bits-reg) ^ see above ^
;;         6 x86-new-xmm-reg (x64-rex-r-1 x64-rex-b-1 x86-xmm-reg)
;;         6 x86-new-ymm-reg (x64-rex-r-1 x64-rex-b-1 x86-ymm-reg)
;;         6 x86-new-zmm-reg (x64-rex-b-1 x64-rex-r-1 x86-zmm-reg)
;;       5 x86-old-reg (x64-rex-r-0 x64-rex-b-0 x86-reg)
;;         6 ^ x86-old-8-bits-low-reg (x86-old-reg x86-8-bits-reg) ^ see above ^
;;         6 ^ x86-old-8-bits-high-reg (x86-old-reg x86-8-bits-reg) ^ see above ^
;;         6 ^ x86-old-16-bits-reg (x86-old-reg x86-16-bits-reg) ^ see above ^
;;         6 ^ x86-old-32-bits-reg (x86-old-reg x86-32-bits-reg) ^ see above ^
;;         6 ^ x86-old-64-bits-reg (x86-old-reg x86-64-bits-reg) ^ see above ^
;;         6 x86-old-xmm-reg (x64-rex-r-0 x64-rex-b-0 x86-xmm-reg)
;;         6 x86-old-ymm-reg (x64-rex-r-0 x64-rex-b-0 x86-ymm-reg)
;;         6 x86-old-zmm-reg (x64-rex-b-0 x64-rex-r-0 x86-zmm-reg)
;;       5 x86-xmm-reg (x86-reg)
;;         6 ^ x86-old-xmm-reg (x64-rex-r-0 x64-rex-b-0 x86-xmm-reg) ^ see above ^
;;         6 ^ x86-new-xmm-reg (x64-rex-r-1 x64-rex-b-1 x86-xmm-reg) ^ see above ^
;;       5 x86-ymm-reg (x86-reg)
;;         6 ^ x86-new-ymm-reg (x64-rex-r-1 x64-rex-b-1 x86-ymm-reg) ^ see above ^
;;         6 ^ x86-old-ymm-reg (x64-rex-r-0 x64-rex-b-0 x86-ymm-reg) ^ see above ^
;;       5 x86-zmm-reg (x86-reg)
;;         6 ^ x86-new-zmm-reg (x64-rex-b-1 x64-rex-r-1 x86-zmm-reg) ^ see above ^
;;         6 ^ x86-old-zmm-reg (x64-rex-b-0 x64-rex-r-0 x86-zmm-reg) ^ see above ^
;;   3 x86-addressing-form (addressing-form)                                                                 `r/m` (required), `is-old-reg nil`, `is-new-reg nil`,
;;         ...                                                                                               `needs-sib nil`, `needs-rex nil`, `works-with-rex` nil.
;;     4 ^ x86-reg (reg x86-addressing-form) ^ see above ^

(defclass modrm-mod-b00 ()
  ((modrm-mod
     :reader modrm-mod
     :allocation :class
     :initform #b00)))

(defclass modrm-mod-b01 ()
  ((modrm-mod
     :reader modrm-mod
     :allocation :class
     :initform #b01)))

(defclass modrm-mod-b10 ()
  ((modrm-mod
     :reader modrm-mod
     :allocation :class
     :initform #b10)))

(defclass modrm-mod-b11 ()
  ((modrm-mod
     :reader modrm-mod
     :allocation :class
     :initform #b11)))

(defclass needs-sib ()
  ((needs-sib
     :reader needs-sib
     :allocation :class
     :initform t
     :documentation "[bp+disp], [ebp+disp], [rbp+disp], [base+index] and [base+scale*index] addressing forms do need SIB. there is no [bp], [ebp] or [rbp] w/o disp")))

(defclass x64-rex-r-0 ()
  ((rex-r
     :reader rex-r
     :allocation :class
     :initform 0
     :documentation "REX.R = 0")))

(defclass x64-rex-r-1 ()
  ((rex-r
     :reader rex-r
     :allocation :class
     :initform 1
     :documentation "REX.R = 1")))

(defclass x64-rex-b-0 ()
  ((rex-b
     :reader rex-b
     :allocation :class
     :initform 0
     :documentation "REX.B = 0")))

(defclass x64-rex-b-1 ()
  ((rex-b
     :reader rex-b
     :allocation :class
     :initform 1
     :documentation "REX.B = 1")))

;; subclasses of `argument` begin here (they have hierarchy level 2).
(defclass addressing-form (argument)
  ((is-reg
     :reader is-reg
     :allocation :class
     :initform nil)
   (is-memory-addressing
     :reader is-memory-addressing
     :allocation :class
     :initform nil)
   (is-abs-addressing
     :reader is-abs-addressing
     :allocation :class
     :initform nil)
   (is-pc-relative
     :reader is-pc-relative
     :allocation :class
     :initform nil)
   (is-reg-indirect
     :reader is-reg-indirect
     :allocation :class
     :initform nil)
   (displacement-size
     :reader displacement-size
     :allocation :class
     :initform 0
     :documentation "default displacement size is 0 bits (no displacement)")))

;; subclasses of `addressing-form` begin here (they have hierarchy level 3).
(defclass arm-addressing-form (addressing-form)
  ())

(defclass memory-addressing (addressing-form)
  ((is-memory-addressing
     :reader is-memory-addressing
     :allocation :class
     :initform t
     :documentation "any kind of memory addressing in any CPU architecture")))

(defclass reg (addressing-form)
  ((is-reg
     :reader is-reg
     :allocation :class
     :initform t)
   (reg-name
     :reader reg-name
     :initarg :reg-name
     :documentation "any CPU-register for any CPU architecture")))

(defclass x86-addressing-form (addressing-form)
  ((r/m
     :reader r/m
     :initarg :r/m
     :initform (error "r/m must be specified")
     :documentation "r/m bits of ModRM byte")
   (is-old-reg
     :reader is-old-reg
     :allocation :class
     :initform nil
     :documentation "old registers are all registers which can be accessed without REX")
   (is-new-reg
     :reader is-new-reg
     :allocation :class
     :initform nil
     :documentation "new registers are all registers which can _not_ be accessed without REX")
   (is-mmx-reg
     :reader is-mmx-reg
     :allocation :class
     :initform nil)
   (is-xmm-reg
     :reader is-xmm-reg
     :allocation :class
     :initform nil)
   (is-ymm-reg
     :reader is-ymm-reg
     :allocation :class
     :initform nil)
   (is-zmm-reg
     :reader is-zmm-reg
     :allocation :class
     :initform nil)
   (needs-sib
     :reader needs-sib
     :allocation :class
     :initform nil)
   (needs-rex
     :reader needs-rex
     :allocation :class
     :initform nil)))

;; subclasses of `arm-addressing-form` begin here (they have hierarchy level 4).

;; subclasses of `memory-addressing` begin here (they have hierarchy level 4).
(defclass abs-addressing (memory-addressing)
  ((is-abs-addressing
     :reader is-abs-addressing
     :allocation :class
     :initform t
     :documentation "_absolute_ memory addressing with no registers, not RIP-relative")))

(defclass arm-memory-addressing (memory-addressing arm-addressing-form)
  ())

(defclass pc-relative (memory-addressing)
  ((is-pc-relative
     :reader is-pc-relative
     :allocation :class
     :initform t
     :documentation "program counter relative (PC-relative) addressing such as RIP-relative addressing in x86-64 or literal in ARMv8-A")))

(defclass reg-indirect (memory-addressing)
  ((is-reg-indirect
     :reader is-reg-indirect
     :allocation :class
     :initform t)))

(defclass x86-memory-addressing (memory-addressing x86-addressing-form)
  ())

;; subclasses of `reg` begin here (they have hierarchy level 4).
(defclass arm-reg (reg arm-addressing-form)
  ())

(defclass x86-reg (reg x86-addressing-form)
  ((is-x86-reg
     :reader is-x86-reg
     :allocation :class
     :initform t
     :documentation "any x86 register, eg. `al`, `ch`, `dx`, `esp`, `r14w`, `mm6`, `mmx12`, `xmm8` or `zmm15`")))

;; subclasses of `x86-addressing-form` begin here (they have hierarchy level 4).

;; ^ (defclass x86-reg (reg x86-addressing-form) ^ see above ^

;; subclasses of `abs-addressing` begin here (they have hierarchy level 5).
(defclass arm-abs-addressing (abs-addressing arm-memory-addressing)
  ())

(defclass x86-abs-addressing (abs-addressing x86-memory-addressing)
  ())

;; subclasses of `arm-memory-addressing` begin here (they have hierarchy level 5).

;; ^ (defclass arm-abs-addressing (memory-addressing arm-addressing-form) ^ see above ^

(defclass arm-literal-addressing (pc-relative arm-memory-addressing)
  ())

(defclass arm-reg-indirect (reg-indirect arm-memory-addressing)
  ())

;; subclasses of `pc-relative` begin here (they have hierarchy level 5).

;; ^ (defclass arm-literal-addressing (pc-relative arm-memory-addressing) ^ see above ^

(defclass x64-rip-relative (pc-relative x86-memory-addressing)
  ;; RIP-relative does not need SIB nor REX.
  ((displacement-size
     :reader displacement-size
     :allocation :class
     :initform 32
     :documentation "RIP-relative addressing form has always disp32.")))

;; subclasses of `reg-indirect` begin here (they have hierarchy level 5).

;; ^ (defclass arm-reg-indirect (reg-indirect arm-memory-addressing) ^ see above ^

(defclass x86-reg-indirect (reg-indirect x86-memory-addressing)
  ((works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "register indirects can always be used with REX")))

;; subclasses of `x86-memory-addressing` begin here (they have hierarchy level 5).

;; ^ (defclass x86-abs-addressing (abs-addressing x86-memory-addressing) ^ see above ^

;; ^ (defclass x86-reg-indirect (reg-indirect x86-memory-addressing) ^ see above ^

;; ^ (defclass x64-rip-relative (pc-relative x86-memory-addressing) ^ see above ^

;; subclasses of `x86-reg` begin here (they have hierarchy level 5).

(defclass x86-8-bits-reg (x86-reg)
  ((reg-size
     :reader reg-size
     :allocation :class
     :initform 8
     :documentation "register size in bits")
   (allowed-targets
     :reader allowed-targets
     :allocation :class
     :initform (list "reg8" "rm8")
     :documentation "allowed encodings in NASM's `insns.dat` syntax")))

(defclass x86-16-bits-reg (x86-reg)
  ((reg-size
     :reader reg-size
     :allocation :class
     :initform 16
     :documentation "register size in bits")
   (allowed-targets
     :reader allowed-targets
     :allocation :class
     :initform (list "reg16" "rm16")
     :documentation "allowed encodings in NASM's `insns.dat` syntax")))

(defclass x86-32-bits-reg (x86-reg)
  ((reg-size
     :reader reg-size
     :allocation :class
     :initform 32
     :documentation "register size in bits")
   (allowed-targets
     :reader allowed-targets
     :allocation :class
     :initform (list "reg32" "rm32")
     :documentation "allowed encodings in NASM's `insns.dat` syntax")))

(defclass x86-64-bits-reg (x86-reg)
  ((reg-size
     :reader reg-size
     :allocation :class
     :initform 64
     :documentation "register size in bits")
   (allowed-targets
     :reader allowed-targets
     :allocation :class
     :initform (list "reg64" "rm64")
     :documentation "allowed encodings in NASM's `insns.dat` syntax")))

(defclass x86-mmx-reg (x86-reg)
  ;; MMX registers never need REX.
  ((reg-size
     :reader reg-size
     :allocation :class
     :initform 64
     :documentation "register size in bits")
   (is-mmx-reg
     :reader is-mmx-reg
     :allocation :class
     :initform t)
   (allowed-targets
     :reader allowed-targets
     :allocation :class
     :initform (list "mmxreg" "mmxrm" "mmxrm64")
     :documentation "allowed encodings in NASM's `insns.dat` syntax")
   (works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "MMX registers can always be used with REX")
   (is-any-rex-r-ok
     :reader is-any-rex-r-ok
     :allocation :class
     :initform t
     :documentation "REX.R can be 0 or 1, no difference")
   (is-any-rex-b-ok
     :reader is-any-rex-b-ok
     :allocation :class
     :initform t
     :documentation "REX.B can be 0 or 1, no difference")
   (rex-r
     :reader rex-r
     :allocation :class
     :initform 0 ; TODO: encode here 1 bit of data (0/1)!
     :documentation "REX.R can be 0 or 1, no difference.")
   (rex-b
     :reader rex-b
     :allocation :class
     :initform 0 ; TODO: encode here 1 bit of data (0/1)!
     :documentation "REX.B can be 0 or 1???")))

(defclass x86-new-reg (x86-reg)
  ((is-new-reg
     :reader is-new-reg
     :allocation :class
     :initform t
     :documentation "new registers are all registers which can _not_ be accessed without REX")))

(defclass x86-old-reg (x64-rex-r-0 x64-rex-b-0 x86-reg)
  ((is-old-reg
     :reader is-old-reg
     :allocation :class
     :initform t
     :documentation "old registers are all registers which can be accessed without REX")))

(defclass x86-xmm-reg (x86-reg)
  ((reg-size
     :reader reg-size
     :allocation :class
     :initform 128
     :documentation "register size in bits")
   (is-xmm-reg
     :reader is-xmm-reg
     :allocation :class
     :initform t)
   (allowed-targets
     :reader allowed-targets
     :allocation :class
     :initform (list "xmmreg" "xmmrm" "xmmrm32" "xmmrm64" "xmmrm128")
     :documentation "allowed encodings in NASM's `insns.dat` syntax")
   (works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "XMM registers can always be used with REX")))

(defclass x86-ymm-reg (x86-reg)
  ((reg-size
     :reader reg-size
     :allocation :class
     :initform 256
     :documentation "register size in bits")
   (is-ymm-reg
     :reader is-ymm-reg
     :allocation :class
     :initform t)
   (allowed-targets
     :reader allowed-targets
     :allocation :class
     :initform (list "ymmreg" "ymmrm256")
     :documentation "allowed encodings in NASM's `insns.dat` syntax")
   (works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "YMM registers can always be used with REX")))

(defclass x86-zmm-reg (x86-reg)
  ((reg-size
     :reader reg-size
     :allocation :class
     :initform 512
     :documentation "register size in bits")
   (is-zmm-reg
     :reader is-ymm-reg
     :allocation :class
     :initform t)
   (allowed-targets
     :reader allowed-targets
     :allocation :class
     :initform (list "zmmreg")
     :documentation "allowed encodings in NASM's `insns.dat` syntax")
   (works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "ZMM registers can always be used with REX")))

;; subclasses of `x86-abs-addressing` begin here (they have hierarchy level 6).
(defclass x86-abs-disp32 (x86-abs-addressing)
  ((displacement-size
     :reader displacement-size
     :allocation :class
     :initform 32
     :documentation "in x32 and x64 the displacement size for absolute addressing is 32 bits")))

;; subclasses of `x64-rip-relative` begin here (they have hierarchy level 6).

(defclass x86-rip-disp32 (x64-rip-relative)
  ((rex-b
     :reader rex-b
     :allocation :class
     :initform nil
     :documentation "REX.B can be 0 or 1")))

(defclass x86-rip-disp32-0 (x64-rex-b-0 x64-rip-relative)
  ())

(defclass x86-rip-disp32-1 (x64-rex-b-1 x64-rip-relative)
  ())

;; subclasses of `x86-reg-indirect` begin here (they have hierarchy level 6).

(defclass x16-reg-indirect (x86-reg-indirect)
  ((allowed-targets
     :reader allowed-targets
     :allocation :class
     :initform (list "mem" "rm8" "rm16")
     :documentation "allowed encodings in NASM's `insns.dat` syntax")))

(defclass x32-reg-indirect (x86-reg-indirect)
  ((allowed-targets
     :reader allowed-targets
     :allocation :class
     :initform (list "mem" "rm8" "rm16" "rm32")
     :documentation "allowed encodings in NASM's `insns.dat` syntax")))

(defclass x64-reg-indirect (x86-reg-indirect)
  ((allowed-targets
     :reader allowed-targets
     :allocation :class
     :initform (list "mem" "mem64" "mem128" "mem256" "mem512" "rm8" "rm16" "rm32" "rm64" "mmxrm" "mmxrm64" "xmmrm" "xmmrm32" "xmmrm64" "xmmrm128" "ymmrm256")
     :documentation "allowed encodings in NASM's `insns.dat` syntax")))

(defclass x86-new-reg-indirect (x64-rex-r-1 x64-rex-b-1 x86-reg-indirect)
  ((needs-rex
     :reader needs-rex
     :allocation :class
     :initform t
     :documentation "[r8], [r9], [r10], [r11], [r14] & [r15] do need REX.")))

(defclass x86-old-reg-indirect (x64-rex-r-0 x64-rex-b-0 x86-reg-indirect)
  ;; [rax], [rcx], [rdx], [rbx], [rsi] & [rdi] do _not_ need REX.
  ())

(defclass x86-reg-indirect-disp0 (x86-reg-indirect)
  ())

(defclass x86-reg-indirect-disp8 (x86-reg-indirect)
  ((displacement-size
     :reader displacement-size
     :allocation :class
     :initform 8
     :documentation "Displacement size 8 bits")))

(defclass x86-reg-indirect-disp32 (x86-reg-indirect)
  ((displacement-size
     :reader displacement-size
     :allocation :class
     :initform 32
     :documentation "Displacement size 32 bits")))

(defclass x86-reg-indirect-needs-sib (needs-sib x86-reg-indirect)
  ;; [rsp], [rbp], [r12] and [r13] and [base+index] forms do need SIB.
  ())

(defclass x86-reg-indirect-does-not-need-sib (x86-reg-indirect)
  ;; [rax], [rcx], [rdx], [rbx], [rsi], [rdi], [r8], [r9], [r10], [r11], [r14] and [r15] do _not_ need SIB.
  ())

;; subclasses of `x86-8-bits-reg` begin here (they have hierarchy level 6).

(defclass x86-new-8-bits-reg (x86-new-reg x86-8-bits-reg)
  ((works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :allocation :class
     :initform t
     :documentation "REX is needed")))

(defclass x86-old-8-bits-high-reg (x86-old-reg x86-8-bits-reg)
  ;; old 8-bit high registers (ah, ch, dh, bh) do not need REX and do not work with REX.
  ())

(defclass x86-old-8-bits-low-reg (x86-old-reg x86-8-bits-reg)
  ;; old 8-bit low register (al, cl, dl, bl) do not need REX but work with REX if needed.
  ((works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "can be used with REX")))

;; subclasses of `x86-16-bits-reg` begin here (they have hierarchy level 6).

(defclass x86-new-16-bits-reg (x64-rex-r-1 x64-rex-b-1 x86-new-reg x86-16-bits-reg)
  ;; new 16-bit registers (r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w) require REX.
  ((works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :allocation :class
     :initform t
     :documentation "REX is needed")))

(defclass x86-old-16-bits-reg (x86-old-reg x86-16-bits-reg)
  ;; old 16-bit-registers (ax, cx, dx, bx, sp, bp, si, di) do not need REX but work with REX if needed.
  ((works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "can be used with REX")))

;; subclasses of `x86-32-bits-reg` begin here (they have hierarchy level 6).

(defclass x86-new-32-bits-reg (x64-rex-r-1 x64-rex-b-1 x86-new-reg x86-32-bits-reg)
  ;; new 32-bit registers (r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d) require REX.
  ((works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :allocation :class
     :initform t
     :documentation "REX is needed")))

(defclass x86-old-32-bits-reg (x86-old-reg x86-32-bits-reg)
  ;; old 32-bit-registers (eax, ecx, edx, ebx, esp, ebp, esi, edi) do not need REX but work with REX if needed.
  ((works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "can be used with REX")))

;; subclasses of `x86-64-bits-reg` begin here (they have hierarchy level 6).

(defclass x86-new-64-bits-reg (x64-rex-r-1 x64-rex-b-1 x86-new-reg x86-64-bits-reg)
  ;; new 64-bit registers (r8, r9, r10, r11, r12, r13, r14, r15) require REX.
  ((works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "can be used with REX")
   (needs-rex
     :reader needs-rex
     :allocation :class
     :initform t
     :documentation "REX is needed")))

(defclass x86-old-64-bits-reg (x86-old-reg x86-64-bits-reg)
  ;; old 64-bit-registers (rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi) do not need REX but work with REX if needed.
  ;; However, REX is anyway usually needed (`o64`), but not always (`o64nw`).
  ((works-with-rex
     :reader works-with-rex
     :allocation :class
     :initform t
     :documentation "can be used with REX")))

;; subclasses of `x86-new-reg` begin here (they have hierarchy level 6).

;; ^ (defclass x86-new-8-bits-reg (x86-new-reg x86-8-bits-reg) ^ see above ^

;; ^ (defclass x86-new-16-bits-reg (x64-rex-r-1 x64-rex-b-1 x86-new-reg x86-16-bits-reg) ^ see above ^

;; ^ (defclass x86-new-32-bits-reg (x64-rex-r-1 x64-rex-b-1 x86-new-reg x86-32-bits-reg) ^ see above ^

;; ^ (defclass x86-new-64-bits-reg (x64-rex-r-1 x64-rex-b-1 x86-new-reg x86-64-bits-reg) ^ see above ^

(defclass x86-new-xmm-reg (x64-rex-r-1 x64-rex-b-1 x86-xmm-reg)
  ((needs-rex
     :reader needs-rex
     :allocation :class
     :initform t
     :documentation "xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14 & xmm15 _do_ need REX.")))

(defclass x86-new-ymm-reg (x64-rex-r-1 x64-rex-b-1 x86-ymm-reg)
  ((needs-rex
     :reader needs-rex
     :allocation :class
     :initform t
     :documentation "ymm8, ymm9, ymm10, ymm11, ymm12, ymm13, ymm14 & ymm15 _do_ need REX.")))

(defclass x86-new-zmm-reg (x64-rex-b-1 x64-rex-r-1 x86-zmm-reg)
  ((needs-rex
     :reader needs-rex
     :allocation :class
     :initform t
     :documentation "zmm8, zmm9, zmm10, zmm11, zmm12, zmm13, zmm14 & zmm15 _do_ need REX.")))

;; subclasses of `x86-old-reg` begin here (they have hierarchy level 6).

;; ^ (defclass x86-old-8-bits-high-reg (x86-old-reg x86-8-bits-reg) ^ see above ^

;; ^ (defclass x86-old-8-bits-low-reg (x86-old-reg x86-8-bits-reg) ^ see above ^

;; ^ (defclass x86-old-16-bits-reg (x86-old-reg x86-16-bits-reg) ^ see above ^

;; ^ (defclass x86-old-32-bits-reg (x86-old-reg x86-32-bits-reg) ^ see above ^

;; ^ (defclass x86-old-64-bits-reg (x86-old-reg x86-64-bits-reg) ^ see above ^

(defclass x86-old-xmm-reg (x86-old-reg x86-xmm-reg)
  ;; xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6 & xmm7 do not need REX.
  ())

(defclass x86-old-ymm-reg (x64-rex-r-0 x64-rex-b-0 x86-ymm-reg)
  ((needs-rex
     :reader needs-rex
     :allocation :class
     :initform nil
     :documentation "ymm0, ymm1, ymm2, ymm3, ymm4, ymm5, ymm6 & ymm7 do not need REX.")))

(defclass x86-old-zmm-reg (x64-rex-b-0 x64-rex-r-0 x86-zmm-reg)
  ())

;; subclasses of `x16-reg-indirect` begin here (they have hierarchy level 7).

(defclass x16-reg-indirect-does-not-need-sib (x16-reg-indirect)
  ())

(defclass x16-reg-indirect-needs-sib (needs-sib x16-reg-indirect)
  ())

;; subclasses of `x32-reg-indirect` begin here (they have hierarchy level 7).

(defclass x32-reg-indirect-does-not-need-sib (x32-reg-indirect)
  ())

(defclass x32-reg-indirect-needs-sib (needs-sib x32-reg-indirect)
  ())

;; subclasses of `x64-reg-indirect` begin here (they have hierarchy level 7).

(defclass x64-new-reg-indirect (x86-new-reg-indirect x64-reg-indirect)
  ())

(defclass x64-old-reg-indirect (x86-old-reg-indirect x64-reg-indirect)
  ())

(defclass x64-reg-indirect-does-not-need-sib (x64-reg-indirect)
  ())

(defclass x64-reg-indirect-needs-sib (needs-sib x64-reg-indirect)
  ())

;; subclasses of `x86-new-8-bits-reg` begin here (they have hierarchy level 7).

(defclass x86-new-8-bits-reg-rex-r-0 (x64-rex-r-0 x64-rex-b-0 x86-new-8-bits-reg)
  ;;"Only spl, bpl, sil & dil belong to this class."
  ())

(defclass x86-new-8-bits-reg-rex-r-1 (x64-rex-r-1 x64-rex-b-1 x86-new-8-bits-reg)
  ;; r8b, r9b, r10b, r11b, r12b, r13b, r14b & r15b belong to this class.
  ())

;; subclasses of `x64-new-reg-indirect` begin here (they have hierarchy level 8).

(defclass x64-new-reg-indirect-does-not-need-sib (x64-new-reg-indirect)
  ())

(defclass x64-new-reg-indirect-needs-sib (needs-sib x64-new-reg-indirect)
  ())

;; subclasses of `x64-old-reg-indirect` begin here (they have hierarchy level 8).

(defclass x64-old-reg-indirect-does-not-need-sib (x64-old-reg-indirect)
  ())

(defclass x64-old-reg-indirect-needs-sib (needs-sib x64-old-reg-indirect)
  ())

;; subclasses of `x64-reg-indirect-does-not-need-sib` begin here (they have hierarchy level 8).

;; ^ (defclass x64-new-reg-indirect-does-not-need-sib (x64-new-reg-indirect) ^ see above ^

;; ^ (defclass x64-old-reg-indirect-does-not-need-sib (x64-old-reg-indirect) ^ see above ^

;; subclasses of `x64-reg-indirect-needs-sib` begin here (they have hierarchy level 8).

;; ^ (defclass x64-new-reg-indirect-needs-sib (needs-sib x64-new-reg-indirect) ^ see above ^

;; ^ (defclass x64-old-reg-indirect-needs-sib (needs-sib x64-old-reg-indirect) ^ see above ^

;; subclasses of `x64-new-reg-indirect-needs-sib` begin here (they have hierarchy level 9).

(defclass x64-new-reg-indirect-sib-with-disp0 (x64-new-reg-indirect-needs-sib)
  ())

(defclass x64-new-reg-indirect-sib-with-disp8 (x64-new-reg-indirect-needs-sib)
  ((displacement-size
     :reader displacement-size
     :allocation :class
     :initform 8)))

(defclass x64-new-reg-indirect-sib-with-disp32 (x64-new-reg-indirect-needs-sib)
  ((displacement-size
     :reader displacement-size
     :allocation :class
     :initform 32)))

;; generic functions begin here.

(defgeneric modrm-mod (x86-addressing-form)
  (:documentation "mod bits (bits 6 and 7 of ModRM) specify the addressing mode for an operand.
                   0b00 reg-indirect: [rax], [rcx], [rdx], [rbx], SIB, disp32, [rsi], [rdi].
                   0b01 reg-indirect: [rax+disp8], [rcx+disp8], [rdx+disp8], [rbx+disp8], SIB+disp8, [rbp+disp8], [rsi+disp8], [rdi+disp8].
                   0b10 reg-indirect: [rax+dis32], [rcx+dis32], [rdx+dis32], [rbx+dis32], SIB+dis32, [rbp+dis32], [rsi+dis32], [rdi+dis32].
                   0b11 register-direct:   any register-direct, see eg. AMD Architecture Programmers Manual Volume 3, Table 1-10."))

(defgeneric modrm-r/m (x86-addressing-form)
  (:documentation "r/m bits (bits 0 .. 2) of ModRM byte."))

(defgeneric modrm-reg (x86-reg)
  (:documentation "reg bits (bits 3 .. 5) of ModRM byte."))

;; methods begin here.

(defmethod modrm-mod ((x86-addressing-form x86-addressing-form))
  (cond
    ((slot-value x86-addressing-form 'is-reg)
     #b11)
    ((slot-value x86-addressing-form 'needs-sib)
     ;; check the need for SIB first.
     (cond
       ((eql (slot-value x86-addressing-form 'displacement-size) 0)
        #b00)
       ((eql (slot-value x86-addressing-form 'displacement-size) 8)
        #b01)
       ((eql (slot-value x86-addressing-form 'displacement-size) 32)
        #b10)
       (t (error "error in defmethod modrm-mod."))))
    ((slot-value x86-addressing-form 'is-reg-indirect)
     (cond
       ((eql (slot-value x86-addressing-form 'displacement-size) 0)
        #b00)
       ((eql (slot-value x86-addressing-form 'displacement-size) 8)
        #b01)
       ((eql (slot-value x86-addressing-form 'displacement-size) 32)
        #b10)
       (t (error "error in defmethod modrm-mod."))))
    (t (error "error in defmethod modrm-mod."))))

(defmethod modrm-r/m ((x86-addressing-form x86-addressing-form))
  (slot-value x86-addressing-form 'r/m))

(defmethod modrm-reg ((x86-reg x86-reg))
  (slot-value x86-reg 'r/m))
