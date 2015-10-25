Cat%.RData: ArchivePantherData.csv pantherDataFrame.R
	R CMD BATCH pantherDataFrame.R

%.tex: %.Rnw
	echo "library('knitr'); knit(\"$*.Rnw\")" | R --slave

%.pdf: %.tex paper.bib
	texi2dvi -p $*.tex



