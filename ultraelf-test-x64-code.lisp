;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

;;; Code to be tested can also be defined in read-eval loop:
;;; ULTRAELF> (defparameter *test-code-inc-rax* #a inc rax #e)
;;; *TEST-CODE-INC-RAX*
;;; ULTRAELF> (create-syntax-tree *test-code-inc-rax*)
;;; (LIST '("inc" "rax"))
;;; 21
;;; ULTRAELF> (assemble-x64-and-print-hex *test-code-inc-rax*)
;;; "(48 FF C0)"
;;;
;;; Test code containing multiple instructions can also be defined
;;; in the read-eval loop, just press Enter between instructions.
;;;
;;; It is not necessary to use DEFPARAMETER, you can give the assembly
;;; code as input to instructions.
;;; ULTRAELF> (create-syntax-tree #a inc rax #e)
;;; (LIST '("inc" "rax"))
;;; 21
;;; ULTRAELF> (assemble-x64-and-print-hex #a inc rax #e)
;;; "(48 FF C0)"

(in-package :ultraelf)

(defparameter *hello-world-x64*
  #a
  [bits 64]

  section .text
  global _start

  NR_write equ 1
  NR_exit  equ 60

  _start:                    ; linker entry point.

  lea     rsi,[message]      ; memory address of the message.
  mov     edx,message_length
  mov     eax,NR_write
  syscall

  xor     edi,edi
  mov     eax,NR_exit         ; number of syscall (60)
  syscall

  section .data

  align 8
  message        db 'Hello world!', 0x0a
  message_length equ ($-message)
  #e)

(defparameter *example-code-x64*
  #a
  mul   rax           ; rdx:rax = rax^2.
  (
   mov   rbp,rsp        ; create the stack frame
   lea   rdi,[ rbx + 4*rax + testmsg1 ] ; load effective address.
   )
  #e)

(defparameter *example-code-x64-with-lisp*
  #a
  inc r10     ; increment register r10.
  inc r11     ; increment register r10.
  #l
  (let*
    ((lisp-code-output-string ""))
    (loop for current-instruction in (list "inc" "dec")
          do (loop for current-arg in (list "r13" "r14" "r15")
                   do (setf lisp-code-output-string
                            (concatenate 'string
                                         lisp-code-output-string
                                         current-instruction " " current-arg
                                         (coerce (list #\Newline) 'string)))))
    lisp-code-output-string)
  #e)
