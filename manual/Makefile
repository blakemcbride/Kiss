%.dvi : %.tex
	tex $<
	texindex $*.fn
	tex $<

%.pdf : %.dvi
	dvipdfm $*

%.ps : %.dvi
	dvips -T Letter -O 0in,-0.8in $*

all: man/index.html # Kiss.pdf

Kiss.pdf : Kiss.tex intro.tex setup.tex orientation.tex
	pdftex Kiss.tex

man/index.html : Kiss.tex intro.tex setup.tex orientation.tex
	makeinfo --html Kiss.tex

clean:
	rm -f *.cp *.fn *.fns *.ky *.pg *.toc *.tp *.vr *.aux *.log
	rm -f *~ *.bak

realclean: clean
	rm -f *.dvi *.ps *.pdf


