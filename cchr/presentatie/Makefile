#~/lea.ulyssis.org/www/maart.pdf: maart.pdf
#	cp maart.pdf ~/lea.ulyssis.org/www/maart.pdf

default: verdediging.pdf

maart.pdf: maart.dvi
	dvipdf maart.dvi

maart.dvi: maart.tex
	latex maart.tex && latex maart.tex

december.pdf: december.dvi
	dvipdf december.dvi

december.dvi: december.tex
	latex december.tex && latex december.tex

verdediging.pdf: verdediging.tex fig/*.pdf
	pdflatex verdediging.tex && pdflatex verdediging.tex
