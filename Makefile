###1 Take PantherDF, hack it and store it independently by cats :

current:  target

target:	  fit.sample.cat1.Rout
	  less fit.sample.cat1.Rout

catsdat.Rout:  ArchivePantherData.csv pantherDataFrame.R
	       $(run-R)


%.Rout:	  catsdat.Rout %.RData
sample.%.Rout:	catsdat.Rout %.RData sample.R
		$(run-R)

fit.%.Rout:	%.Rout fitfunctions.R mikesim.R simfunctions.R %seeds.R
		$(run-R)

fitcat1.RData: catsdat cat1.RData fitfunctions.R cat1seeds.R
	       R CMD BATCH cat1seeds.R

fitcat2.RData: catsdat cat2.RData fitfunctions.R cat2seeds.R
	       R CMD BATCH cat2seeds.R

fitcat14.RData: catsdat cat14.RData fitfunctions.R cat14seeds.R
	       R CMD BATCH cat14seeds.R

fitcat15.RData: catsdat cat14.RData fitfunctions.R cat15seeds.R
		R CMD BATCH cat15seeds.R

cat%mods.RData:	cat%.RData wrap.R
		R CMD BATCH wrap.R

### Fitting HMMs

###3.1 Combining all fit depmix objects into dataset

bicplotcat%.RData: fithomocat%.RData fitsincat%.RData fitquadcat%.RData fitblockcat%.RData fitfmmcat%.RData fitfmmsincat%.RData fithourlycat%.RData bicplots.R hmmhomo.RData hmmsin.RData hmmquad.RData hmmblock.RData fithourly.RData fitfmm.RData fitfmmsin.RData
		   R CMD BATCH bicplots.R



%.tex: %.Rnw
	echo "library('knitr'); knit(\"$*.Rnw\")" | R --slave

%.pdf: %.tex paper.bib
	texi2dvi -p $*.tex

%: %.pdf
	evince paper3.pdf 

clean:
	rm -f *.bbl *.blg *.log *.aux *.loc *~







#######Make stuff

gitroot = ../
-include local.mk
-include $(gitroot)/local.mk
ms = $(gitroot)/makestuff

-include $(ms)/git.mk

-include $(ms)/visual.mk
-include $(ms)/wrapR.mk

