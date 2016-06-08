%.pdf: %.Rnw
	echo "knitr::knit2pdf(\"$*.Rnw\")" | R --slave

%.tex: %.Rnw
	echo "knitr::knit(\"$*.Rnw\")" | R --slave



