;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.
;;;
;;; compiling and loading:
;;; 1. CL-USER> (load (compile-file "ultraelf.asd"))
;;; 2. CL-USER> (compile-ultraelf)
;;; or
;;; 3. CL-USER> (c-u)
;;;
;;; (compile-ultraelf) and (c-u) are 2 separate but identical macros.
;;;
;;; example usage:
;;; 4. ULTRAELF> (assemble-x64-and-print-hex *example-code-x64-with-lisp*)
;;; 5. ULTRAELF> (assemble-x64-and-print-hex #a mov ax,bx #e)

(in-package :cl-user)

(defmacro compile-ultraelf ()
  (asdf:oos 'asdf:load-op 'ultraelf)
  (in-package :ultraelf))

(defmacro c-u ()
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
               (:file "ultraelf-string")                           ; string functions.
               (:file "ultraelf-printing")                         ; printing functions.
               (:file "ultraelf-regex")                            ; regular expression functions.
               (:file "ultraelf-numbers")                          ; number-handling functions.
               (:file "ultraelf-x64-rex")                          ; x64 "emit REX" functions.
               (:file "ultraelf-argument-classes")                 ; instruction argument classes except addressing form classes.
               (:file "ultraelf-argument-lists")                   ; lists of arguments belonging to each argument class (currently for each string instruction).
               (:file "ultraelf-create-argument-instances")        ; create instance for each argument (currently for each string instruction). uses emit REX functions.
               (:file "ultraelf-addressing-form-classes")          ; addressing form classes.
               (:file "ultraelf-addressing-form-lists")            ; lists of registers etc. belonging to each addressing form class.
               (:file "ultraelf-create-addressing-form-instances") ; create instance for each addressing form, including each register.
               (:file "ultraelf-elf-classes")                      ; ELF classes.
               (:file "ultraelf-asm-reader")                       ; Lisp assembly reader.
               (:file "ultraelf-general-emit-functions")           ; general emit functions, needed for assembling and pseudo-ops.
               (:file "ultraelf-asm-pseudo-ops")                   ; asm pseudo-ops.
               (:file "ultraelf-sib")                              ; x86 SIB byte scale values hash table.
               (:file "ultraelf-sreg2")                            ; x86 SREG2 segment register values hash table.
               (:file "ultraelf-sreg3")                            ; x86 SREG3 segment register values hash table.
               (:file "ultraelf-arm-instructions")                 ; instruction-specific ARM emit-code functions.
               (:file "ultraelf-x86-general-emit-functions")       ; general x86 emit-code functions.
               (:file "ultraelf-x86-instructions")                 ; instruction-specific x86 emit-code functions.
               (:file "ultraelf-x87-instructions")                 ; instruction-specific x87 emit-code functions.
               (:file "ultraelf-instructions-hash-tables")         ; x86 instructions hash tables.
               (:file "ultraelf-alt-code")                         ; alternative code used for metamorphic engine.
               (:file "ultraelf-assembling-functions")             ; general assembling functions.
               (:file "ultraelf-test-x64-code"))                   ; x64 test code, used for testing.
  :depends-on (:parse-number))
