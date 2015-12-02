###1 Take PantherDF, hack it and store it independently by cats :

catsdat: ArchivePantherData.csv pantherDataFrame.R
	    R CMD BATCH pantherDataFrame.R

fitcat1.RData: catsdat cat1.RData fitfunctions.R cat1seeds.R
	       R CMD BATCH cat1seeds.R

fitcat2.RData: catsdat cat2.RData fitfunctions.R cat2seeds.R
	       R CMD BATCH cat2seeds.R

fitcat14.RData: catsdat cat14.RData fitfunctions.R cat14seeds.R
	       R CMD BATCH cat14seeds.R

fitcat15.RData: catsdat cat14.RData fitfunctions.R cat15seeds.R
		R CMD BATCH cat15seeds.R

### Fitting HMMs

###3.1 Combining all fit depmix objects into dataset

bicplotcat%.RData: fithomocat%.RData fitsincat%.RData fitquadcat%.RData fitblockcat%.RData fitfmmcat%.RData fitfmmsincat%.RData fithourlycat%.RData bicplots.R hmmhomo.RData hmmsin.RData hmmquad.RData hmmblock.RData fithourly.RData fitfmm.RData fitfmmsin.RData
		   R CMD BATCH bicplots.R

%.tex: %.Rnw
	echo "library('knitr'); knit(\"$*.Rnw\")" | R --slave

%.pdf: %.tex paper.bib
	texi2dvi -p $*.tex

clean:
	rm -f *.bbl *.blg *.log *.aux *.loc *~
