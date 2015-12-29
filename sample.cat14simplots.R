## creating simdf 
library(reshape2)
#load("cat14alldat.RData") my windows machine
#load("fitcat14.RData")
tempdf <- data.frame(obs = cat$LogDist,
                     fmm4 = simfmm4$obs,
                     fmmsin3 = simfmmsin3$obs,
                     hmm5 = simhomo5$obs,
                     hmmhourly3 = simhourly3$obs,
                     hmmblock4 = simblock4$obs,
                     hmmquad5 = simquad5$obs,
                     hmmsin4 = simsin4$obs,
                     time = cat$Time)

print(bicplot(sumdat))
print(avgplot(tempdf))
print(acfplot(tempdf))
print(msd45(fitsin4s,fithomo5s))
print(avgplot(vitcat(cat,fithomo3s)))
print(avgplot(vitcat(cat,fitsin3s)))
print(avgplot(vitcat5(cat,fithomo5s)))
print(avgplot(vitcat5(cat,fitsin5s)))
print(acfplot(vitcat(cat,fithomo3s))+ggtitle("Viterbi vs Observed ACF"))
print(acfplot(vitcat5(cat,fitsin5s))+ggtitle("Viterbi vs Observed ACF"))

