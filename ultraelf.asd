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
  :components ((:file "ultra-essentials")                       ; a package containing only some absolutely basic functions from `:cl`, to `:use` in others.
               (:file "ultra-package")                          ; functions, variables and classes not going into one of the architecture-specific packages go here.
               (:file "ultra-x86")                              ; instructions and arguments available to all x86 flavors (x16, x32, and x64).
               (:file "ultra-x86-modern")                       ; instructions and arguments available to both x32 and x64 flavors.
               (:file "ultra-x16")                              ; instructions and arguments available in x16 (16-bit flavor of x86) and few basic essentials.
               (:file "ultra-x32")                              ; instructions and arguments available in x32 (32-bit flavor of x86) and few basic essentials.
               (:file "ultra-x64")                              ; instructions and arguments available in x64 (64-bit flavor of x86) and few basic essentials.
               (:file "ultra-arm")                              ; instructions and arguments available in ARM and few basic essentials.
               (:file "ultra-compiling-macros")                 ; `defmacro compile-ultraelf` & `defmacro c-u`.
               (:file "ultra-x64-assembling-macros")            ; `defmacro emit-and-update-instruction-length `.
               (:file "ultra-init")                             ; some `defparameter` variable definitions.
               (:file "ultra-sequence")                         ; sequence functions.
               (:file "ultra-string")                           ; string functions.
               (:file "ultra-printing")                         ; printing functions.
               (:file "ultra-regex")                            ; regular expression functions.
               (:file "ultra-numbers")                          ; number-handling functions.
               (:file "ultra-x64-rex")                          ; x64 "emit REX" functions.
               (:file "ultra-architecture-classes")             ; architecture classes.
               (:file "ultra-instruction-classes")              ; instruction classes.
               (:file "ultra-x86-instruction-classes")          ; x86 instruction classes.
               (:file "ultra-x16-instruction-classes")          ; x16 instruction classes and methods.
               (:file "ultra-x32-instruction-classes")          ; x32 instruction classes and methods.
               (:file "ultra-x64-instruction-classes")          ; x64 instruction classes and methods.
               (:file "ultra-arm-instruction-classes")          ; ARM instruction classes and methods.
               (:file "ultra-argument-classes")                 ; instruction argument classes except addressing form classes.
               (:file "ultra-addressing-form-classes")          ; addressing form classes.
               (:file "ultra-addressing-form-lists")            ; lists of registers etc. belonging to each addressing form class.
               (:file "ultra-create-addressing-form-instances") ; create instance for each addressing form, including each register.
               (:file "ultra-elf-classes")                      ; ELF classes.
               (:file "ultra-asm-reader")                       ; Lisp assembly reader.
               (:file "ultra-general-emit-functions")           ; general emit functions, needed for assembling and pseudo-ops.
               (:file "ultra-asm-pseudo-ops")                   ; asm pseudo-ops.
               (:file "ultra-sib")                              ; x86 SIB byte scale values hash table.
               (:file "ultra-sreg2")                            ; x86 SREG2 segment register values hash table.
               (:file "ultra-sreg3")                            ; x86 SREG3 segment register values hash table.
               (:file "ultra-arm-instructions")                 ; instruction-specific ARM emit-code functions.
               (:file "ultra-x86-general-emit-functions")       ; general x86 emit-code functions.
               (:file "ultra-create-instruction-instances")     ; create instance for each x64 instruction+operands combination. converted from NASM's `insns.dat`.
               (:file "ultra-alt-code")                         ; alternative code used for metamorphic engine.
               (:file "ultra-assembling-functions")             ; general assembling functions.
               (:file "ultra-x16-assembling-functions")         ; x16 assembling functions.
               (:file "ultra-x32-assembling-functions")         ; x32 assembling functions.
               (:file "ultra-x64-assembling-functions")         ; x64 assembling functions.
               (:file "ultra-arm-assembling-functions")         ; ARM assembling functions.
               (:file "ultra-test-x64-code"))                   ; x64 test code, used for testing.
  :depends-on (:parse-number :screamer))
