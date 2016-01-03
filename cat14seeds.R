##cat14 fit seeds 

#FMM ----
fitfmm3s <- fitmix(3,cat,seed=64)
fitfmm4s <- fitmix(4,cat,seed=4)
fitfmm5s <- fitmix(5,cat,seed=54)
fitfmm6s <- fitmix(6,cat,seed=16)

#FMM sin ----
fitfmmsin3 <- fitmixsin(3,cat,seed=3)
fitfmmsin4 <- fitmixsin(4,cat,seed=1)
fitfmmsin5 <- fitmixsin(5,cat,seed=1)
fitfmmsin6 <- fitmixsin(6,cat,seed=1)

#HMM ----
fithomo3s <- fithomo(3,cat,3030)
fithomo4s <- fithomo(4,cat,2)
fithomo5s <- fithomo(5,cat,1)
fithomo6s <- fithomo(6,cat,1)

#HMM sin ----
fitsin3s <- fitsin(3,cat,seed=2830)
fitsin4s <- fitsin(4,cat,seed=2830)
fitsin5s <- fitsin(5,cat,seed=2830)
fitsin6s <- fitsin(6,cat,seed=2830)

#HMM quad ----
fitquad3s <- fitquad(3,cat,seed=3030)
fitquad4s <- fitquad(4,cat,seed=3030)
fitquad5s <- fitquad(5,cat,seed=3030)
fitquad6s <- fitquad(6,cat,seed=3030)

#HMM block ----
fitblock3s <- fitblock(3,cat,seed=2830)
fitblock4s <- fitblock(4,cat,seed=2830)
fitblock5s <- fitblock(5,cat,seed=2830)
fitblock6s <- fitblock(6,cat,seed=2830)

#HMM hourly ----
fithourly3 <- fithourly(3,cat,seed=1)
fithourly4 <- fithourly(4,cat,seed=1)


simfmm4 <- simmix(4,cat,fitfmm4s)
simfmmsin3 <- simmixsin(3,cat,fitfmmsin3)
simhomo5 <- simhomo(5,cat,fithomo5s)
simhourly3 <- simhourly(3,cat,fithourly3)
simblock4 <- simblock(4,cat,fitblock4s)
simquad5 <- simquad(5,cat,fitquad5s)
simsin4 <- simsin(4,cat,fitsin4s)

simdat <- data.frame(obs = cat$LogDist,
                     fmm4 = simfmm4$obs,
                     fmmsin3 = simfmmsin3$obs,
                     hmm5 = simhomo5$obs,
                     hmmhourly3 = simhourly3$obs,
                     hmmblock4 = simblock4$obs,
                     hmmquad5 = simquad5$obs,
                     hmmsin4 = simsin4$obs,
                     time = cat$Time)
