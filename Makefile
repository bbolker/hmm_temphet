### Take PantherDF, hack it and store it independently by cats :
cat%.Rout: ArchivePantherData.csv pantherDataFrame.R
	R CMD BATCH pantherDataFrame.R

cat%.RData: ArchivePantherData.csv pantherDataFrame.R
	    R CMD BATCH pantherDataFrame.R

### Fitting HMMs

fithomocat%.RData: cat%.RData fithomo.R
		   R CMD BATCH fithomo.R

fitsincat%.RData: cat%.RData fitsin.R
		   R CMD BATCH fitsin.R

fitquadcat%.RData: cat%.RData fitquad.R
		   R CMD BATCH fitquad.R

fitblockcat%.RData: cat%.RData fitblock.R
		   R CMD BATCH fitblock.R



%.tex: %.Rnw
	echo "library('knitr'); knit(\"$*.Rnw\")" | R --slave

%.pdf: %.tex paper.bib
	texi2dvi -p $*.tex

clean:
	rm -f *.bbl *.blg *.log *.aux *.loc *~
