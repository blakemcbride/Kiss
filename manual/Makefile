
TEXFILES= Kiss.tex intro.tex setup.tex orientation.tex developing.tex \
          backendapi.tex frontendapi.tex commandline.tex splitsystem.tex desktop.tex


%.dvi : %.tex
	tex $<
	texindex $*.fn
	tex $<

%.pdf : %.dvi
	dvipdfm $*

%.ps : %.dvi
	dvips -T Letter -O 0in,-0.8in $*

all: man/index.html jsdoc/index.html GroovyOverview.html # Kiss.pdf GroovyOverview.pdf

man/index.html :  $(TEXFILES)
	makeinfo --html Kiss.tex -o man

jsdoc/index.html: ../src/main/frontend/kiss/*.js ../src/main/frontend/kiss/component/components.js
	rm -rf jsdoc
	jsdoc -t /usr/local/lib/node_modules/docdash -c conf.json --readme JsDocHead.md -d jsdoc $^

Kiss.pdf : $(TEXFILES)
	pdftex Kiss.tex
	pdftex Kiss.tex
	pdftex Kiss.tex

GroovyOverview.html : GroovyOverview.tex
	makeinfo --html --no-split GroovyOverview.tex

GroovyOverview.pdf : GroovyOverview.tex
	pdftex GroovyOverview.tex
	pdftex GroovyOverview.tex
	pdftex GroovyOverview.tex

clean:
	rm -f *.cp *.fn *.fns *.ky *.pg *.toc *.tp *.vr *.aux *.log
	rm -f *~ *.bak

realclean: clean
	rm -f *.dvi *.ps *.pdf


updateserver: Kiss.pdf
	scp Kiss.pdf root@blakemcbride.us://srv/www/blakemcbride.us/software/kiss

