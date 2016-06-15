###1 Take PantherDF, hack it and store it independently by cats :

R := /usr/bin/env Rscript

current:  paper3.pdf.now

cat.%.df: dataframe.R ArchivePantherData.csv %.cat
	$(R) $^

cat.1.%: %.R cat.1.RDS mikesim.R 
	$(R) $^ 

cat.2.%: %.R cat.2.RDS mikesim.R 
	$(R) $^ 

cat.14.%: %.R cat.14.RDS mikesim.R 
	$(R) $^ 

cat.15.%: %.R cat.15.RDS mikesim.R 
	$(R) $^ 


%.fit.Rout: %.df.Rout fitfunctions.R mikesim.R simfunctions.R %seeds.R
	$(run-R)

%.summary.Rout: %.fit.Rout summary.R %summary.R
	$(run-R)

%.plot.Rout: %sumdat.RData %.RData plotsimfunctions.R %simplots.R
	$(run-R)

%.BIC.Rout: %.RData wrap.R
	$(run-R)

%.weibull.Rout: %.df.Rout weibull.R
	$(run-R)

%.weibullVM.Rout: %.df.Rout weibullVM.R
	$(run-R)

%.LNVM.Rout: %.df.Rout LNVM.R
	$(run-R)

%.Weibull_het.Rout: %.df.Rout Weibull_het.R
	$(run-R)

%.LNVM_het.Rout: %.df.Rout LNVM_het.R
	$(run-R)

simtest.Rout: mikesim.R simfunctions.R simtest.R 101.txt
	$(run-R)

sim_%.Rout: mikesim.R simfunctions.R simtest.R %.txt
	$(run-R)

plotsimtest.Rout: plotsimtest.R
	$(run-R)

%.tex: %.Rnw
	echo "library('knitr'); knit(\"$*.Rnw\")" | R --slave

%.pdf: %.tex paper.bib
	texi2dvi -p $*.tex

%.pdf.now: %.pdf
	evince paper3.pdf

clean:
	rm -f *.bbl *.blg *.log *.aux *.loc *~ *.txt

move_sum: 
	mv *.sum.RDS ./summary_stats/

move_catsims:
	mv *.sim.RDS ./cat_sims/





#######Make stuff

ms = makestuff/

-include $(ms)/git.mk
-include $(ms)/visual.mk

-include $(ms)/wrapR.mk
-include $(ms)/oldlatex.mk

makestuff:
	git clone https://github.com/dushoff/makestuff.git
