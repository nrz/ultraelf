;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun test-parser ()
  "This function tests the functioning of `assemble-x64-and-print-hex`."
  (rt:rem-all-tests)

  ;; Tests for basic syntax tree creation.
  (rt:deftest test-create-syntax-tree-foo (create-syntax-tree #a foo #e) (list '("foo")))
  (rt:deftest test-create-syntax-tree-foo-bar (create-syntax-tree #a foo bar #e) (list '("foo" "bar")))
  (rt:deftest |test-create-syntax-tree-foo,bar| (create-syntax-tree #a foo,bar #e) (list '("foo" "bar")))
  (rt:deftest |test-create-syntax-tree-foo-bar,baz| (create-syntax-tree #a foo bar,baz #e) (list '("foo" "bar" "baz")))
  (rt:deftest |test-create-syntax-tree-foo,bar-baz| (create-syntax-tree #a foo,bar baz #e) (list '("foo" "bar" "baz")))
  (rt:deftest |test-create-syntax-tree-foo,bar,baz| (create-syntax-tree #a foo,bar,baz #e) (list '("foo" "bar" "baz")))
  (rt:deftest |test-create-syntax-tree-foo1-bar1-foo2-bar2| (create-syntax-tree #a foo1 bar1 #a foo2 bar2 #e) (list '("foo1" "bar1") '("foo2" "bar2")))
  (rt:deftest |test-create-syntax-tree-foo1,bar1-foo2,bar2-foo3,bar3| (create-syntax-tree #a foo1,bar1 #a foo2,bar2 #a foo3,bar3 #e) (list '("foo1" "bar1") '("foo2" "bar2") '("foo3" "bar3")))

  ;; Tests for processing of memory address syntax.

  (rt:do-tests))
