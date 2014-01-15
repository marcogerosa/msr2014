# 
# makefile para a compilação do documento
#
# Sáb Jun 23 21:13:37 BRT 2007
#

BASE_NAME = sig-alternate

LATEX     = latex
PDFLATEX  = pdflatex
BIBTEX    = bibtex

pdf: $(BASE_NAME).pdf
ps: $(BASE_NAME).ps

$(BASE_NAME).pdf: $(BASE_NAME).tex 
	$(PDFLATEX) $<
	$(BIBTEX) $(BASE_NAME) 
	$(PDFLATEX) $<
	$(PDFLATEX) $<
	
$(BASE_NAME).ps: $(BASE_NAME).tex 
	$(LATEX) $<
	
clean:
	rm -f $(BASE_NAME)*.ps $(BASE_NAME)*.dvi *.log \
	      *.aux *.blg *.toc *.brf *.ilg *.ind \
	      missfont.log $(BASE_NAME)*.bbl $(BASE_NAME)*.pdf $(BASE_NAME)*.out \
		  $(BASE_NAME)*.lof $(BASE_NAME)*.lot 
