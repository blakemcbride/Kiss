
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

# -----------------------------------------------------------------------------
# Find JSDoc executable (prefer local, fallback to PATH global)
# -----------------------------------------------------------------------------
JSDOC := $(shell node -e 'const fs=require("fs"); const local="./node_modules/.bin/jsdoc"; process.stdout.write(fs.existsSync(local)?local:"jsdoc");')

# -----------------------------------------------------------------------------
# Find Docdash template directory (try local first, then Node resolver, then npm root -g)
# -----------------------------------------------------------------------------
DOCDASH := $(shell node -e 'const fs=require("fs"); const path=require("path"); const cp=require("child_process"); function isDir(p){try{return fs.statSync(p).isDirectory()}catch(e){return false}} function out(p){process.stdout.write(p||"")} let d=path.resolve("node_modules/docdash"); if(isDir(d)){out(d); process.exit(0)} try{const pkg=require.resolve("docdash/package.json"); d=pkg.replace(/package\.json$$/,""); if(isDir(d)){out(d); process.exit(0)}}catch(e){} try{const root=cp.execSync("npm root -g",{encoding:"utf8"}).trim(); d=path.join(root,"docdash"); if(isDir(d)){out(d); process.exit(0)}}catch(e){} out("");')

JSDOC_SOURCES := \
  ../src/main/frontend/kiss/AGGrid.js \
  ../src/main/frontend/kiss/DateTimeUtils.js \
  ../src/main/frontend/kiss/DateUtils.js \
  ../src/main/frontend/kiss/DOMUtils.js \
  ../src/main/frontend/kiss/Editor.js \
  ../src/main/frontend/kiss/MutableString.js \
  ../src/main/frontend/kiss/NumberUtils.js \
  ../src/main/frontend/kiss/Server.js \
  ../src/main/frontend/kiss/TimeUtils.js \
  ../src/main/frontend/kiss/Utils.js \
  ../src/main/frontend/kiss/component/components.js

jsdoc/index.html: $(JSDOC_SOURCES) conf.json JsDocHead.md
	rm -rf jsdoc
	@if [ -z "$(DOCDASH)" ]; then \
		echo "ERROR: docdash not found. Install with: npm install --save-dev docdash jsdoc"; \
		exit 1; \
	fi
	$(JSDOC) -t "$(DOCDASH)" -c conf.json --readme JsDocHead.md -d jsdoc $^

.PHONY: jsdoc
jsdoc: jsdoc/index.html

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

