pdf: main.pdf

all: pdf

main.pdf: main.tex references.bib
	pdflatex main
	biber main
	pdflatex main
	pdflatex main

clean:
	rm -f *.aux *.bbl *.blg *.log *.xml *.bcf
