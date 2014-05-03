;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defclass elf-file ()
  ((elf-header
     :reader elf-header
     :documentation "ELF header as a whole")
   (program-header-table
     :accessor program-header-table
     :documentation "required for loadable file, optional for relocatable file")
   (sections
     :accessor sections)
   (segments
     :accessor segments)
   (section-header-table
     :accessor section-header-table
     :documentation "required for relocatable file, optional for loadable file")
   (magic
     :accessor magic
     :initform (list #x7f #x45 #x4c #x46)
     :documentation "ELF64 magic values: #x7f 'ELF'")
   (file-class
     :accessor file-class
     :initform (list 2)
     :documentation "1 for 32-bit objects, 2 for 64-bit objects")
   (data-encoding
     :accessor data-encoding
     :initform (list 1)
     :documentation "1 for little-endian object file data structures, 2 for big-endian object file data structures")
   (file-version
     :accessor file-version
     :initform (list 1)
     :documentation "1 for current ELF file version")
   (os-abi-identification
     :accessor os-abi-identification
     :initform (list 0)
     :documentation "0 for System V ABI, 1 for HP-UX, 255 for standalone (embedded) application")
   (abi-version
     :accessor abi-version
     :initform (list 0)
     :documentation "0 for System V ABI, third version. Meaning of values depends on os-abi-identification.")
   (filler-bytes
     :accessor filler-bytes
     :initform (list 0 0 0 0 0 0 0)
     :documentation "filler bytes")))
