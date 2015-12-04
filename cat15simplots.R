## creating simdf 
library(reshape2)
source('plotsimfunctions.R')
#load("cat15alldat.RData") my windows machine
load("fitcat15.RData")
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
