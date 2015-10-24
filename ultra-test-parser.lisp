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
  (rt:deftest test-create-syntax-tree-foo-extra-space-bar (create-syntax-tree #a foo  bar #e) (list '("foo" "bar")))
  (rt:deftest test-create-syntax-tree-foo-more-extra-space-bar (create-syntax-tree #a foo   bar #e) (list '("foo" "bar")))
  (rt:deftest |test-create-syntax-tree-foo-bar,baz| (create-syntax-tree #a foo bar,baz #e) (list '("foo" "bar" "baz")))
  (rt:deftest |test-create-syntax-tree-foo-bar,extra-space-baz| (create-syntax-tree #a foo bar, baz #e) (list '("foo" "bar" "baz")))
  (rt:deftest |test-create-syntax-tree-foo-bar,more-extra-space-baz| (create-syntax-tree #a foo bar,  baz #e) (list '("foo" "bar" "baz")))
  (rt:deftest |test-create-syntax-tree-foo1-bar1-foo2-bar2| (create-syntax-tree #a foo1 bar1 #a foo2 bar2 #e) (list '("foo1" "bar1") '("foo2" "bar2")))
  (rt:deftest |test-create-syntax-tree-foo1-bar1-foo2-bar2-foo3-bar3| (create-syntax-tree #a foo1 bar1 #a foo2 bar2 #a foo3 bar3 #e) (list '("foo1" "bar1") '("foo2" "bar2") '("foo3" "bar3")))

  ;; Tests for syntax tree creation using assembly source code with Common Lisp macros.
  (rt:deftest test-create-syntax-tree-hash-l-foo (create-syntax-tree #a #l "foo" #e) (list '("foo")))
  (rt:deftest test-create-syntax-tree-foo1-hash-l-foo2 (create-syntax-tree #a foo1 #l "foo2" #e) (list '("foo1") '("foo2")))
  (rt:deftest test-create-syntax-tree-foo1-hash-l-foo2-hash-a-foo3 (create-syntax-tree #a foo1 #l "foo2" #a foo3 #e) (list '("foo1") '("foo2") '("foo3")))
  (rt:deftest test-create-syntax-tree-foo1-bar1-concatenate-string-foo2-bar2-foo3-bar3 (create-syntax-tree #a foo1 bar1 #l (concatenate 'string "foo2 bar2") #a foo3 bar3 #e) (list '("foo1" "bar1") '("foo2" "bar2") '("foo3" "bar3")))

  ;; Tests for syntax tree creation using assembly source code saved in variable.
  (rt:deftest test-code-with-lisp-number-1 (create-syntax-tree *test-code-with-lisp-number-1*) (list '("foo1" "bar1") '("foo2" "bar2") '("foo3" "bar3")))
  (rt:deftest test-code-with-lisp-number-2 (create-syntax-tree *test-code-with-lisp-number-2*) (list '("foo1" "bar1") '("foo2" "bar2") '("foo3" "bar3") '("foo4" "bar4")))
  (rt:deftest test-code-with-lisp-number-3 (create-syntax-tree *test-code-with-lisp-number-3*) (list '("foo1" "bar1") '("foo2" "bar2") '("foo3" "bar3") '("foo4" "bar4") '("foo5" "bar5")))

  ;; Tests for processing of memory address syntax.
  (rt:deftest |test-create-syntax-tree-foo-[bar1-bar2]| (create-syntax-tree #a foo [bar1 bar2] #e) (list '("foo" "[bar1 bar2]")))
  (rt:deftest |test-create-syntax-tree-foo,[bar1-bar2]| (create-syntax-tree #a foo,[bar1 bar2] #e) (list '("foo" "[bar1 bar2]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar1-bar2-bar3]| (create-syntax-tree #a foo [bar1 bar2 bar3] #e) (list '("foo" "[bar1 bar2 bar3]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar1-extra-space-bar2]| (create-syntax-tree #a foo [bar1  bar2] #e) (list '("foo" "[bar1 bar2]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar1-more-extra-space-bar2]| (create-syntax-tree #a foo [bar1   bar2] #e) (list '("foo" "[bar1 bar2]")))

  (rt:do-tests))
