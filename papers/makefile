pdf: main.pdf

all: pdf

main.pdf: main.tex references.bib
	biber main
	pdflatex main
	bibtex main
	pdflatex main
	pdflatex main
