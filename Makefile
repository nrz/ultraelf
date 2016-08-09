TARGET = ultraelf

edit:
	vim ultraelf.asd && ps x | grep sbcl | cut -b 1-5 | xargs -I{} kill {}
vim:
	vim ultraelf.asd && ps x | grep sbcl | cut -b 1-5 | xargs -I{} kill {}
gvim:
	gvim ultraelf.asd && ps x | grep sbcl | cut -b 1-5 | xargs -I{} kill {}
clean:
	find . -type f -name '*.fasl' -exec rm -v {} \;
