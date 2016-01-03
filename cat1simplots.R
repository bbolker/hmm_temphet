library(ggplot2)
library(dplyr)
library(plyr)
library(depmixS4)
library(reshape2)


print(bicplot(sumdat))
print(avgplot(simdat))
print(acfplot(simdat))
print(msd56(fitsin5s,fithomo6s))
print(avgplot(vitcat(cat,fithomo3s)))
print(acfplot(vitcat(cat,fithomo3s))+ggtitle("Viterbi vs Observed ACF"))
