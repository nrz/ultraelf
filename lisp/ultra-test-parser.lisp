;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf)

(defun test-parser ()
  "This function tests the functioning of `assemble-x64-and-print-hex`."
  (rt:rem-all-tests)

  ;; Tests for basic syntax tree creation.
  (rt:deftest test-create-syntax-tree (create-syntax-tree #a #e) (list))
  (rt:deftest test-create-syntax-tree-foo (create-syntax-tree #a foo #e) (list '("foo")))
  (rt:deftest test-create-syntax-tree-foo-bar (create-syntax-tree #a foo bar #e) (list '("foo" "bar")))
  (rt:deftest test-create-syntax-tree-foo-extra-space-bar (create-syntax-tree #a foo  bar #e) (list '("foo" "bar")))
  (rt:deftest test-create-syntax-tree-foo-more-extra-space-bar (create-syntax-tree #a foo   bar #e) (list '("foo" "bar")))
  (rt:deftest |test-create-syntax-tree-foo-bar,baz| (create-syntax-tree #a foo bar,baz #e) (list '("foo" "bar" "baz")))
  (rt:deftest |test-create-syntax-tree-foo-bar,extra-space-baz| (create-syntax-tree #a foo bar, baz #e) (list '("foo" "bar" "baz")))
  (rt:deftest |test-create-syntax-tree-foo-bar,more-extra-space-baz| (create-syntax-tree #a foo bar,  baz #e) (list '("foo" "bar" "baz")))
  (rt:deftest |test-create-syntax-tree-foo1-bar1-foo2-bar2| (create-syntax-tree #a foo1 bar1 #a foo2 bar2 #e) (list '("foo1" "bar1") '("foo2" "bar2")))
  (rt:deftest |test-create-syntax-tree-foo1-bar1-foo2-bar2-foo3-bar3| (create-syntax-tree #a foo1 bar1 #a foo2 bar2 #a foo3 bar3 #e) (list '("foo1" "bar1") '("foo2" "bar2") '("foo3" "bar3")))

  ;; Tests for basic syntax cree creation with newlines in input.
  (rt:deftest test-create-syntax-tree-newline-hash-a-foo-bar-hash-e (create-syntax-tree
                                                                      #a foo bar #e) (list '("foo" "bar")))
  (rt:deftest test-create-syntax-tree-newline-hash-a-foo-bar-newline-hash-e (create-syntax-tree
                                                                              #a foo bar
                                                                              #e) (list '("foo" "bar")))
  (rt:deftest test-create-syntax-tree-hash-a-foo-newline-bar-hash-e (create-syntax-tree #a foo
                                                                                        bar #e) (list '("foo") '("bar")))
  (rt:deftest test-create-syntax-tree-hash-a-foo-newline-bar-newline-hash-e (create-syntax-tree #a foo
                                                                                                bar
                                                                                                #e) (list '("foo") '("bar")))
  (rt:deftest test-create-syntax-tree-hash-a-newline-foo-newline-bar-hash-e (create-syntax-tree #a
                                                                                                foo
                                                                                                bar #e) (list '("foo") '("bar")))
  (rt:deftest test-create-syntax-tree-newline-hash-a-foo-newline-bar-hash-e (create-syntax-tree
                                                                              #a foo
                                                                              bar #e) (list '("foo") '("bar")))
  (rt:deftest test-create-syntax-tree-newline-hash-a-newline-foo-newline-bar-hash-e (create-syntax-tree
                                                                                      #a
                                                                                      foo
                                                                                      bar #e) (list '("foo") '("bar")))
  (rt:deftest test-create-syntax-tree-newline-hash-a-newline-foo-newline-bar-newline-hash-e (create-syntax-tree
                                                                                              #a
                                                                                              foo
                                                                                              bar
                                                                                              #e) (list '("foo") '("bar")))

  ;; Tests for syntax tree creation using assembly source code with Common Lisp macros.
  (rt:deftest test-create-syntax-tree-hash-a-hash-l-foo (create-syntax-tree #a #l "foo" #e) (list '("foo")))
  (rt:deftest test-create-syntax-tree-hash-l-foo (create-syntax-tree #l "foo" #e) (list '("foo")))
  (rt:deftest test-create-syntax-tree-hash-a-hash-l-foo-hash-a (create-syntax-tree #a #l "foo" #a #e) (list '("foo")))
  (rt:deftest test-create-syntax-tree-hash-l-foo-hash-a (create-syntax-tree #l "foo" #a #e) (list '("foo")))
  (rt:deftest test-create-syntax-tree-hash-a-foo1-hash-l-foo2 (create-syntax-tree #a foo1 #l "foo2" #e) (list '("foo1") '("foo2")))
  (rt:deftest test-create-syntax-tree-hash-a-foo1-hash-l-foo2-hash-a-foo3 (create-syntax-tree #a foo1 #l "foo2" #a foo3 #e) (list '("foo1") '("foo2") '("foo3")))
  (rt:deftest test-create-syntax-tree-hash-a-foo1-bar1-concatenate-string-foo2-bar2-foo3-bar3 (create-syntax-tree #a foo1 bar1 #l (concatenate 'string "foo2 bar2") #a foo3 bar3 #e) (list '("foo1" "bar1") '("foo2" "bar2") '("foo3" "bar3")))
  (rt:deftest test-create-syntax-tree-hash-a-hash-l-foo1-hash-l-foo2 (create-syntax-tree #a #l "foo1" #l "foo2" #e) (list '("foo1") '("foo2")))
  (rt:deftest test-create-syntax-tree-hash-l-foo1-hash-l-foo2 (create-syntax-tree #l "foo1" #l "foo2" #e) (list '("foo1") '("foo2")))
  (rt:deftest test-create-syntax-tree-hash-l-foo1-hash-l-foo2-hash-a (create-syntax-tree #l "foo1" #l "foo2" #a #e) (list '("foo1") '("foo2")))

  ;; Tests for syntax tree creation using assembly source code saved in variable.
  (rt:deftest test-code-with-lisp-number-1 (create-syntax-tree *test-code-with-lisp-number-1*) (list '("foo1" "bar1") '("foo2" "bar2") '("foo3" "bar3")))
  (rt:deftest test-code-with-lisp-number-2 (create-syntax-tree *test-code-with-lisp-number-2*) (list '("foo1" "bar1") '("foo2" "bar2") '("foo3" "bar3") '("foo4" "bar4")))
  (rt:deftest test-code-with-lisp-number-3 (create-syntax-tree *test-code-with-lisp-number-3*) (list '("foo1" "bar1") '("foo2" "bar2") '("foo3" "bar3") '("foo4" "bar4") '("foo5" "bar5")))

  ;; Tests for processing of memory address syntax.
  (rt:deftest |test-create-syntax-tree-foo-[bar1-bar2]| (create-syntax-tree #a foo [bar1 bar2] #e) (list '("foo" "[bar1 bar2]")))
  (rt:deftest |test-create-syntax-tree-foo,[bar1-bar2]| (create-syntax-tree #a foo,[bar1 bar2] #e) (list '("foo" "[bar1 bar2]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar1-bar2-bar3]| (create-syntax-tree #a foo [bar1 bar2 bar3] #e) (list '("foo" "[bar1 bar2 bar3]")))
  (rt:deftest |test-create-syntax-tree-foo-[extra-space-bar1-bar2]| (create-syntax-tree #a foo [ bar1 bar2] #e) (list '("foo" "[bar1 bar2]")))
  (rt:deftest |test-create-syntax-tree-foo-[more-extra-space-bar1-bar2]| (create-syntax-tree #a foo [  bar1 bar2] #e) (list '("foo" "[bar1 bar2]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar1-extra-space-bar2]| (create-syntax-tree #a foo [bar1  bar2] #e) (list '("foo" "[bar1 bar2]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar1-more-extra-space-bar2]| (create-syntax-tree #a foo [bar1   bar2] #e) (list '("foo" "[bar1 bar2]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar1-bar2-extra-space]| (create-syntax-tree #a foo [bar1 bar2 ] #e) (list '("foo" "[bar1 bar2]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar1-bar2-more-extra-space]| (create-syntax-tree #a foo [bar1 bar2  ] #e) (list '("foo" "[bar1 bar2]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar+123]| (create-syntax-tree #a foo [bar+123] #e) (list '("foo" "[bar+123]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar1+bar2]| (create-syntax-tree #a foo [bar1+bar2] #e) (list '("foo" "[bar1+bar2]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar1+bar2+123]| (create-syntax-tree #a foo [bar1+bar2+123] #e) (list '("foo" "[bar1+bar2+123]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar-extra-space-+123]| (create-syntax-tree #a foo [bar +123] #e) (list '("foo" "[bar+123]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar+-extra-space-123]| (create-syntax-tree #a foo [bar+ 123] #e) (list '("foo" "[bar+123]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar-extra-space-+-extra-space-123]| (create-syntax-tree #a foo [bar + 123] #e) (list '("foo" "[bar+123]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar-extra-space-+bar]| (create-syntax-tree #a foo [bar +bar] #e) (list '("foo" "[bar+bar]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar+-extra-space-bar]| (create-syntax-tree #a foo [bar+ bar] #e) (list '("foo" "[bar+bar]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar-extra-space-+-extra-space-bar]| (create-syntax-tree #a foo [bar + bar] #e) (list '("foo" "[bar+bar]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar+]| (create-syntax-tree #a foo [bar+] #e) (list '("foo" "[bar+]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar-extra-space-+]| (create-syntax-tree #a foo [bar +] #e) (list '("foo" "[bar+]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar+-extra-space]| (create-syntax-tree #a foo [bar+ ] #e) (list '("foo" "[bar+]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar-extra-space-+-extra-space]| (create-syntax-tree #a foo [bar + ] #e) (list '("foo" "[bar+]")))
  (rt:deftest |test-create-syntax-tree-foo-[+bar]| (create-syntax-tree #a foo [+bar] #e) (list '("foo" "[+bar]")))
  (rt:deftest |test-create-syntax-tree-foo-[extra-space-+bar]| (create-syntax-tree #a foo [ +bar] #e) (list '("foo" "[+bar]")))
  (rt:deftest |test-create-syntax-tree-foo-[+-extra-space-bar]| (create-syntax-tree #a foo [+ bar] #e) (list '("foo" "[+bar]")))
  (rt:deftest |test-create-syntax-tree-foo-[extra-space-+-extra-space-bar]| (create-syntax-tree #a foo [ + bar] #e) (list '("foo" "[+bar]")))
  (rt:deftest |test-create-syntax-tree-foo-[+bar+]| (create-syntax-tree #a foo [+bar+] #e) (list '("foo" "[+bar+]")))
  (rt:deftest |test-create-syntax-tree-foo-[++bar]| (create-syntax-tree #a foo [++bar] #e) (list '("foo" "[++bar]")))
  (rt:deftest |test-create-syntax-tree-foo-[+-extra-space-+bar]| (create-syntax-tree #a foo [+ +bar] #e) (list '("foo" "[++bar]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar++]| (create-syntax-tree #a foo [bar++] #e) (list '("foo" "[bar++]")))
  (rt:deftest |test-create-syntax-tree-foo-[bar+-extra-space-+]| (create-syntax-tree #a foo [bar+ +] #e) (list '("foo" "[bar++]")))
  (rt:deftest |test-create-syntax-tree-foo-bar1-[bar2]| (create-syntax-tree #a foo bar1 [bar2] #e) (list '("foo" "bar1" "[bar2]")))
  (rt:deftest |test-create-syntax-tree-foo-bar1-[bar2+]| (create-syntax-tree #a foo bar1 [bar2+] #e) (list '("foo" "bar1" "[bar2+]")))
  (rt:deftest |test-create-syntax-tree-foo-bar1-[+bar2]| (create-syntax-tree #a foo bar1 [+bar2] #e) (list '("foo" "bar1" "[+bar2]")))
  (rt:deftest |test-create-syntax-tree-foo-bar1-[+bar2+]| (create-syntax-tree #a foo bar1 [+bar2+] #e) (list '("foo" "bar1" "[+bar2+]")))

  ;; Tests for processing of comments (Lisp-style, C-style, C++-style).
  (rt:deftest |test-create-syntax-tree-foo-lisp-comment-bar| (create-syntax-tree #a foo ; bar
                                                                                 #e) (list '("foo")))
  (rt:deftest |test-create-syntax-tree-foo-c-style-on-1-line-comment-bar| (create-syntax-tree #a foo /* bar */ #e) (list '("foo")))
  (rt:deftest |test-create-syntax-tree-foo-c-style-on-1-line-comment-bar-then-baz| (create-syntax-tree #a foo/*bar*/baz #e) (list '("foo" "baz")))
  (rt:deftest |test-create-syntax-tree-foo-c-style-on-1-line-space-comment-bar-then-baz| (create-syntax-tree #a foo /*bar*/baz #e) (list '("foo" "baz")))
  (rt:deftest |test-create-syntax-tree-foo-c-style-on-1-line-comment-bar-then-space-baz| (create-syntax-tree #a foo/*bar*/ baz #e) (list '("foo" "baz")))
  (rt:deftest |test-create-syntax-tree-foo-c-style-on-1-line-space-comment-bar-then-space-baz| (create-syntax-tree #a foo /*bar*/ baz #e) (list '("foo" "baz")))
  (rt:deftest |test-create-syntax-tree-foo-c-style-on-2-lines-comment-bar| (create-syntax-tree #a foo /* bar
                                                                                               baz */ #e) (list '("foo")))
  (rt:deftest |test-create-syntax-tree-foo-c-style-box-comment-bar| (create-syntax-tree #a foo
                                                                                        /******
                                                                                        * bar *
                                                                                        ******/ #e) (list '("foo")))
  (rt:deftest |test-create-syntax-tree-foo-c-style-box-comment-bar-baz| (create-syntax-tree #a foo
                                                                                            /******
                                                                                            * bar *
                                                                                            * baz *
                                                                                            ******/ #e) (list '("foo")))
  (rt:deftest |test-create-syntax-tree-foo-c++-style-comment-bar| (create-syntax-tree #a foo // bar
                                                                                      #e) (list '("foo")))

  (rt:do-tests))
