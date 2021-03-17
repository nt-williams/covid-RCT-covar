pdf: main.pdf

all: pdf

main.pdf: main.tex references.bib
	pdflatex main
	bibtex main
	pdflatex main
	pdflatex main

clean:
	rm -f *.aux *.bbl *.blg *.log *.xml *.bcf
