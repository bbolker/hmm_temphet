## creating simdf 
library(reshape2)
source("plotsimfunctions.R")
#load("cat1alldat.RData") my windows machine
#load("fitcat1.RData")
tempdf <- data.frame(obs = cat$LogDist,
                     fmm5 = simfmm5$obs,
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
