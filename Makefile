###1 Take PantherDF, hack it and store it independently by cats :

current:  target

target:	cat1.plot.Rout
	evince cat1.plot.Rout.pdf

catsdat:       ArchivePantherData.csv pantherDataFrame.R
	       R CMD BATCH pantherDataFrame.R

%.df.Rout:	catsdat %.RData df.R
		$(run-R)

%.fit.Rout:	%.df.Rout fitfunctions.R mikesim.R simfunctions.R %seeds.R
		$(run-R)

%.summary.Rout:	%.fit.Rout summary.R %summary.R
		$(run-R)

%.plot.Rout:	catsdat %sumdat.RData %.RData plotsimfunctions.R %simplots.R
		$(run-R)

%.BIC.Rout:	%.RData wrap.R
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

ms = makestuff/

-include $(ms)/git.mk
-include $(ms)/visual.mk

-include $(ms)/wrapR.mk
-include $(ms)/oldlatex.mk

makestuff:
	git clone https://github.com/mli2830/makestuff.git