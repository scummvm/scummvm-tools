latexcmd=latex
document=doc
dvi2ps=dvips
ps2pdf=ps2pdf
index=makeindex
RM=rm -f

cleanfiles= *.toc *.dvi *.log *.aux *.bbl *.swp *.ent *.out Thumbs.db *.ps *.blg *.lox *.lol *.idx *.ind *.ilg

all:
	-$(RM) $(cleanfiles)
	$(latexcmd) $(document)
	$(index) $(document)
	$(latexcmd) $(document)
	$(latexcmd) $(document)
	$(dvi2ps) $(document).dvi
	$(ps2pdf) $(document).ps
	-$(RM) $(cleanfiles)
