MAIN = kiss-book
LATEX = pdflatex
XELATEX = xelatex
MKIDX = makeindex

all: $(MAIN).pdf cover.pdf

$(MAIN).pdf: $(MAIN).tex chapters/*.tex
	$(LATEX) $(MAIN).tex
	$(MKIDX) $(MAIN).idx 2>/dev/null || true
	$(LATEX) $(MAIN).tex
	$(LATEX) $(MAIN).tex

cover.pdf: cover.tex figures/Kiss-logo.jpg
	$(XELATEX) cover.tex

clean:
	rm -f *.aux *.log *.idx *.ind *.ilg *.toc *.out *.pdf *.lof *.lot
	rm -f chapters/*.aux

.PHONY: all clean
