;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.
;;;
;;; compiling and loading:
;;; 1. CL-USER> (load (compile-file "ultraelf.asd"))
;;; 2. CL-USER> (compile-ultraelf)
;;; 3. CL-USER> (in-package :ultraelf)
;;;
;;; example usage:
;;; 4. ULTRAELF> (assemble-x64-and-print-hex *test-code-x64*)

(in-package :cl-user)

(defun compile-ultraelf ()
  (asdf:oos 'asdf:load-op 'ultraelf))

(defpackage :ultraelf-asd
  (:use :cl :asdf))

(in-package :ultraelf-asd) 

(defsystem :ultraelf
  :serial t
  :description "UltraELF system"
  :author "Antti Nuortimo"
  :components ((:file "packages")
               (:file "ultraelf-asm-reader")
               (:file "ultraelf-modrm")
               (:file "ultraelf-sib")
               (:file "ultraelf-sreg2")
               (:file "ultraelf-sreg3")
               (:file "ultraelf-register-types")
               (:file "ultraelf-register-lists")
               (:file "ultraelf-x64-rex")
               (:file "ultraelf-x86-instructions")
               (:file "ultraelf-instructions-hash-tables")
               (:file "ultraelf-alt-code")
               (:file "ultraelf-1")
               (:file "ultraelf-test-x64-code")))
