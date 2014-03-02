TARGET = ultraelf

edit:
	gvim ultraelf.lisp; ps x | grep sbcl | cut -b 1-5 | xargs -I{} kill {}
