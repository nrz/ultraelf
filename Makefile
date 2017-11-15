TARGET = ultraelf

edit:
	vim lisp/ultraelf.asd && ps x | grep sbcl | cut -b 1-5 | xargs -I{} kill {}
vim:
	vim lisp/ultraelf.asd && ps x | grep sbcl | cut -b 1-5 | xargs -I{} kill {}
gvim:
	gvim lisp/ultraelf.asd && ps x | grep sbcl | cut -b 1-5 | xargs -I{} kill {}
clean:
	find ./lisp -type f -name '*.fasl' -exec rm -v {} \;
