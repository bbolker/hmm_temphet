###1 Take PantherDF, hack it and store it independently by cats :

current:	target

target:	plot.sample.cat1.Rout
	evince plot.sample.cat1.Rout.pdf

catsdat.Rout:	ArchivePantherData.csv pantherDataFrame.R
	$(run-R)


%.Rout:	  catsdat.Rout %.RData
sample.%.Rout:	catsdat.Rout %.RData sample.R
	$(run-R)


fit.%.Rout:	%seeds.R
fit.sample.%.Rout:	%seeds.R
fit.%.Rout:	%.Rout fitfunctions.R mikesim.R simfunctions.R %seeds.R
	$(run-R)

plot.%.Rout:	plotsimfunctions.R
plot.sample.%.Rout:	plotsimfunctions.R
plot.%.Rout:		fit.%.Rout plotsimfunctions.R %simplots.R
			$(run-R)



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

