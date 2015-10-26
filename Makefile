###1 Take PantherDF, hack it and store it independently by cats :
cat%.Rout: ArchivePantherData.csv pantherDataFrame.R
	R CMD BATCH pantherDataFrame.R

cat%.RData: ArchivePantherData.csv pantherDataFrame.R
	    R CMD BATCH pantherDataFrame.R

###2 Fitting HMMs

fithomocat%.RData: cat%.RData fithomo.R
		   R CMD BATCH fithomo.R

fitsincat%.RData: cat%.RData fitsin.R
		   R CMD BATCH fitsin.R

fitquadcat%.RData: cat%.RData fitquad.R
		   R CMD BATCH fitquad.R

fitblockcat%.RData: cat%.RData fitblock.R
		   R CMD BATCH fitblock.R

fitfmmcat%.RData: cat%.RData fitfmm.R
		  R CMD BATCH fitfmm.R

fitfmmsincat%.RData: cat%.RData fitfmmsin.R
		     R CMD BATCH fitfmmsin.R

fithourlycat%.RData: cat%.RData fithourly.R
		     R CMD BATCH fithourly.R

###3.1 Combining all fit depmix objects into dataset

bicplotcat%.RData: fithomocat%.RData fitsincat%.RData fitquadcat%.RData fitblockcat%.RData fitfmmcat%.RData fitfmmsincat%.RData fithourlycat%.RData bicplots.R hmmhomo.RData hmmsin.RData hmmquad.RData hmmblock.RData fithourly.RData fitfmm.RData fitfmmsin.RData
		   R CMD BATCH bicplots.R

%.tex: %.Rnw
	echo "library('knitr'); knit(\"$*.Rnw\")" | R --slave

%.pdf: %.tex paper.bib
	texi2dvi -p $*.tex

clean:
	rm -f *.bbl *.blg *.log *.aux *.loc *~
