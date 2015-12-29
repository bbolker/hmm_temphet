## creating simdf 
library(reshape2)
source('plotsimfunctions.R')
#load("cat15alldat.RData") my windows machine
#load("fitcat15.RData")
tempdf <- data.frame(obs = cat$LogDist,
                     fmm3 = simfmm3$obs,
                     fmmsin4 = simfmmsin4$obs,
                     hmm6 = simhomo6$obs,
                     hmmhourly3 = simhourly3$obs,
                     hmmblock4 = simblock4$obs,
                     hmmquad5 = simquad5$obs,
                     hmmsin5 = simsin5$obs,
                     time = cat$Time)

print(bicplot(sumdat))
print(avgplot(tempdf))
print(acfplot(tempdf))
print(msd56(fitsin5s,fithomo6s))
print(avgplot(vitcat(cat,fithomo3s)))
print(avgplot(vitcat(cat,fitsin3s)))
print(avgplot(vitcat5(cat,fithomo5s)))
print(avgplot(vitcat5(cat,fitsin5s)))
print(acfplot(vitcat(cat,fithomo3s))+ggtitle("Viterbi vs Observed ACF"))
print(acfplot(vitcat5(cat,fitsin5s))+ggtitle("Viterbi vs Observed ACF"))

