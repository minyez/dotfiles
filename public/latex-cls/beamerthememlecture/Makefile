PROJNAME = example
MAINFN = example
MAKE = gmake
LATEXMK = latexmk
LATEX = $(shell \
		  awk '/TeX TS-program =/ {printf("%s", $$5)}' $(PROJNAME).tex \
		  )
# subdirectories to include in the tarball
SUBDIRS =
# all tex sources to let watch work
SOURCES = $(wildcard *.sty) $(wildcard *.tex) $(wildcard $(patsubst %,%/*.tex,$(SUBDIRS)))
# files to cleanup
AUXSUFFICES = blg,aux,log,out,odt,dvi,dls,fdb_latexmk,synctex.gz,bbl,toc,run.xml,vrb,nav,snm,bcf,fls

.PHONY: default clean veryclean cleanall

default: $(PROJNAME).pdf

$(PROJNAME).pdf: $(MAINFN).pdf
	cp -a $^ $@

$(MAINFN).pdf: $(SOURCES)
	$(LATEXMK) -pdf -pdflatex=$(LATEX) -bibtex -f $(MAINFN).tex \
		-latexoption="-synctex=1 -interaction=nonstopmode -shell-escape "

veryclean: clean
	rm -f $(PROJNAME).pdf

cleanall: veryclean

clean:
	rm -f *.{$(AUXSUFFICES)} *-blx.bib
	rm -f */*.{$(AUXSUFFICES)} */*-blx.bib
	rm -rf */_minted-*

