TARGET = ultraelf

edit:
	gvim ultraelf.asd; ps x | grep sbcl | cut -b 1-5 | xargs -I{} kill {}
