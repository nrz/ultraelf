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
  :components ((:file "ultraelf-essentials")                       ; a package containing only some absolutely basic functions from `:cl`, to `:use` in others.
               (:file "ultraelf-package")                          ; functions, variables and classes not going into one of the architecture-specific packages go here.
               (:file "ultraelf-x16")                              ; instructions and arguments available in x16 (16-bit flavor of x86) and few basic essentials.
               (:file "ultraelf-x32")                              ; instructions and arguments available in x32 (32-bit flavor of x86) and few basic essentials.
               (:file "ultraelf-x64")                              ; instructions and arguments available in x64 (64-bit flavor of x86) and few basic essentials.
               (:file "ultraelf-arm")                              ; instructions and arguments available in ARM and few basic essentials.
               (:file "ultraelf-compiling-macros")                 ; `defmacro compile-ultraelf` & `defmacro c-u`.
               (:file "ultraelf-init")                             ; some `defparameter` variable definitions.
               (:file "ultraelf-sequence")                         ; sequence functions.
               (:file "ultraelf-string")                           ; string functions.
               (:file "ultraelf-printing")                         ; printing functions.
               (:file "ultraelf-regex")                            ; regular expression functions.
               (:file "ultraelf-numbers")                          ; number-handling functions.
               (:file "ultraelf-x64-rex")                          ; x64 "emit REX" functions.
               (:file "ultraelf-architecture-classes")             ; architecture classes.
               (:file "ultraelf-instruction-classes")              ; instruction classes.
               (:file "ultraelf-x86-instruction-classes")          ; x86 instruction classes.
               (:file "ultraelf-x16-instruction-classes")          ; x16 instruction classes and methods.
               (:file "ultraelf-x32-instruction-classes")          ; x32 instruction classes and methods.
               (:file "ultraelf-x64-instruction-classes")          ; x64 instruction classes and methods.
               (:file "ultraelf-arm-instruction-classes")          ; ARM instruction classes and methods.
               (:file "ultraelf-argument-classes")                 ; instruction argument classes except addressing form classes.
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
               (:file "ultraelf-create-instruction-instances")     ; create instance for each x64 instruction+operands combination. converted from NASM's `insns.dat`.
               (:file "ultraelf-x87-instructions")                 ; instruction-specific x87 emit-code functions.
               (:file "ultraelf-instruction-hash-tables")          ; instruction hash tables for different architectures (currently x64).
               (:file "ultraelf-alt-code")                         ; alternative code used for metamorphic engine.
               (:file "ultraelf-assembling-functions")             ; general assembling functions.
               (:file "ultraelf-x16-assembling-functions")         ; x16 assembling functions.
               (:file "ultraelf-x32-assembling-functions")         ; x32 assembling functions.
               (:file "ultraelf-x64-assembling-functions")         ; x64 assembling functions.
               (:file "ultraelf-arm-assembling-functions")         ; ARM assembling functions.
               (:file "ultraelf-test-x64-code"))                   ; x64 test code, used for testing.
  :depends-on (:parse-number))
