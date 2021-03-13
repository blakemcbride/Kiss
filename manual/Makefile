
TEXFILES= Kiss.tex intro.tex setup.tex orientation.tex developing.tex \
          backendapi.tex frontendapi.tex commandline.tex


%.dvi : %.tex
	tex $<
	texindex $*.fn
	tex $<

%.pdf : %.dvi
	dvipdfm $*

%.ps : %.dvi
	dvips -T Letter -O 0in,-0.8in $*

all: man/index.html jsdoc/index.html # Kiss.pdf

Kiss.pdf : $(TEXFILES)
	pdftex Kiss.tex
	pdftex Kiss.tex
	pdftex Kiss.tex

man/index.html :  $(TEXFILES)
	makeinfo --html Kiss.tex

jsdoc/index.html: ../src/main/webapp/kiss/*.js ../src/main/webapp/kiss/component/components.js
	rm -rf jsdoc
	jsdoc -t /usr/local/lib/node_modules/docdash -c conf.json -d jsdoc $^

clean:
	rm -f *.cp *.fn *.fns *.ky *.pg *.toc *.tp *.vr *.aux *.log
	rm -f *~ *.bak

realclean: clean
	rm -f *.dvi *.ps *.pdf


