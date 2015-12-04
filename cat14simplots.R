## creating simdf 
library(reshape2)
source('plotsimfunctions.R')
#load("cat14alldat.RData") my windows machine
load("fitcat14.RData")
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
