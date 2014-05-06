;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.
;;;
;;; compiling and loading:
;;; 1. CL-USER> (load (compile-file "ultraelf.asd"))
;;; 2. CL-USER> (compile-ultraelf)
;;;
;;; example usage:
;;; 3. ULTRAELF> (assemble-x64-and-print-hex *test-code-x64*)

(in-package :cl-user)

(defmacro compile-ultraelf ()
  (asdf:oos 'asdf:load-op 'ultraelf)
  (in-package :ultraelf))

(defpackage :ultraelf-asd
  (:use :cl :asdf))

(in-package :ultraelf-asd)

(defsystem :ultraelf
  :serial t
  :description "UltraELF system"
  :author "Antti Nuortimo"
  :components ((:file "packages")
               (:file "ultraelf-compiling-macros")
               (:file "ultraelf-string")                     ; string functions.
               (:file "ultraelf-printing")                   ; printing functions.
               (:file "ultraelf-addressing-form-classes")    ; addressing form classes.
               (:file "ultraelf-addressing-form-lists")      ; lists of registers etc. belonging to each addressing form class.
               (:file "ultraelf-create-register-instances")  ; lists of registers belonging to different register classes.
               (:file "ultraelf-elf-classes")                ; ELF classes.
               (:file "ultraelf-asm-reader")                 ; Lisp assembly reader.
               (:file "ultraelf-general-emit-functions")     ; general emit functions, needed for assembling and pseudo-ops.
               (:file "ultraelf-asm-pseudo-ops")             ; asm pseudo-ops.
               (:file "ultraelf-modrm")                      ; x86 ModRM byte register values hash table.
               (:file "ultraelf-sib")                        ; x86 SIB byte scale values hash table.
               (:file "ultraelf-sreg2")                      ; x86 SREG2 segment register values hash table.
               (:file "ultraelf-sreg3")                      ; x86 SREG3 segment register values hash table.
               (:file "ultraelf-register-types")             ; x86 register type hash table.
               (:file "ultraelf-x64-rex")                    ; x64 "emit REX" functions.
               (:file "ultraelf-x86-general-emit-functions") ; general x86 emit-code functions.
               (:file "ultraelf-x86-instructions")           ; instruction-specific x86 emit-code functions.
               (:file "ultraelf-instructions-hash-tables")   ; x86 instructions hash tables.
               (:file "ultraelf-alt-code")                   ; alternative code used for metamorphic engine.
               (:file "ultraelf-assembling-functions")       ; general assembling functions.
               (:file "ultraelf-test-x64-code")))            ; x64 test code, used for testing.
