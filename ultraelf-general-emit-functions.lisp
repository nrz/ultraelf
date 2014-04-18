;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun string-to-8-bit-little-endian (my-string)
  "This function parses a string containing a 8-bit number
   and emits a list containing the corresponding byte."
  (let*
    ((imm8 (parse-integer my-string)))
    (list (logand imm8 #xff))))

(defun string-to-16-bit-little-endian (my-string)
  "This function parses a string containing a 16-bit number
   and emits a list containing the corresponding 2 bytes."
  (let*
    ((imm16 (parse-integer my-string)))
    (list (logand imm16 #xff)
          (logand (ash imm16 -8) #xff))))

(defun string-to-32-bit-little-endian (my-string)
  "This function parses a string containing a 32-bit number
   and emits a list containing the corresponding 4 bytes."
  (let*
    ((imm32 (parse-integer my-string)))
    (list (logand imm32 #xff)
          (logand (ash imm32 -8) #xff)
          (logand (ash imm32 -16) #xff)
          (logand (ash imm32 -24) #xff))))

(defun string-to-64-bit-little-endian (my-string)
  "This function parses a string containing a 64-bit number
   and emits a list containing the corresponding 8 bytes."
  (let*
    ((imm64 (parse-integer my-string)))
    (list (logand imm64 #xff)
          (logand (ash imm64 -8) #xff)
          (logand (ash imm64 -16) #xff)
          (logand (ash imm64 -24) #xff)
          (logand (ash imm64 -32) #xff)
          (logand (ash imm64 -40) #xff)
          (logand (ash imm64 -48) #xff)
          (logand (ash imm64 -56) #xff))))
