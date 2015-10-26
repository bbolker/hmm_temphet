### Take PantherDF, hack it and store it independently by cats :
Cat%.Rout: ArchivePantherData.csv pantherDataFrame.R
	R CMD BATCH pantherDataFrame.R

Cat%.RData: ArchivePantherData.csv pantherDataFrame.R
	R CMD BATCH pantherDataFrame.R

%.tex: %.Rnw
	echo "library('knitr'); knit(\"$*.Rnw\")" | R --slave

%.pdf: %.tex paper.bib
	texi2dvi -p $*.tex

clean:
	rm -f *.bbl *.blg *.log *.aux *.loc *~
