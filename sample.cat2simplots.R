## creating simdf 
library(reshape2)
#load("cat2alldat.RData") my windows machine
#load("fitcat2.RData")
tempdf <- data.frame(obs = cat$LogDist,
                     fmm4 = simfmm4$obs,
                     fmmsin4 = simfmmsin4$obs,
                     hmm5 = simhomo5$obs,
                     hmmhourly3 = simhourly3$obs,
                     hmmblock4 = simblock4$obs,
                     hmmquad4 = simquad4$obs,
                     hmmsin4 = simsin4$obs,
                     time = cat$Time)

print(bicplot(sumdat))
print(avgplot(tempdf))
print(acfplot(tempdf))
print(msd45(fitsin4s,fithomo5s))
print(avgplot(vitcat(cat,fithomo3s)))
print(acfplot(vitcat(cat,fithomo3s))+ggtitle("Viterbi vs Observed ACF"))
