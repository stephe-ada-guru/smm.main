# Books documentation

all : books.html books.info

%.html : %.texinfo
	makeinfo --html --no-split -o $*.html $<

%.info : %.texinfo
	makeinfo $<

#Local Variables:
#eval: (ada-parse-prj-file "../books.prj")
#mode: makefile
#End:
# end of file
