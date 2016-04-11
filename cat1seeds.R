##cat1 fit seeds 

#FMM ----
fitfmm3s <- fitmix(3,cat,seed=43)
fitfmm4s <- fitmix(4,cat,seed=9)
fitfmm5s <- fitmix(5,cat,seed=3)
fitfmm6s <- fitmix(6,cat,seed=1)
fitfmm7s <- fitmix(7,cat,seed=1)

#FMM sin ----
fitfmmsin3 <- fitmixsin(3,cat,seed=3)
fitfmmsin4 <- fitmixsin(4,cat,seed=1)
fitfmmsin5 <- fitmixsin(5,cat,seed=1)
fitfmmsin6 <- fitmixsin(6,cat,seed=1)
fitfmmsin7 <- fitmixsin(7,cat,seed=1)

#HMM ----
fithomo3s <- fithomo(3,cat,3)
fithomo4s <- fithomo(4,cat,2)
fithomo5s <- fithomo(5,cat,1)
fithomo6s <- fithomo(6,cat,1)
fithomo7s <- fithomo(7,cat,1)

#HMM sin ----
fitsin3s <- fitsin(3,cat,seed=2830)
fitsin4s <- fitsin(4,cat,seed=2830)
fitsin5s <- fitsin(5,cat,seed=2830)
fitsin6s <- fitsin(6,cat,seed=2830)
fitsin7s <- fitsin(7,cat,seed=2830)

#HMM quad ----
fitquad3s <- fitquad(3,cat,seed=3030)
fitquad4s <- fitquad(4,cat,seed=3030)
fitquad5s <- fitquad(5,cat,seed=3030)
fitquad6s <- fitquad(6,cat,seed=3030)
fitquad7s <- fitquad(7,cat,seed=3030)

#HMM block ----
fitblock3s <- fitblock(3,cat,seed=2830)
fitblock4s <- fitblock(4,cat,seed=2830)
fitblock5s <- fitblock(5,cat,seed=2830)
fitblock6s <- fitblock(6,cat,seed=2830)
fitblock7s <- fitblock(7,cat,seed=2830)

#HMM hourly ----
fithourly3 <- fithourly(3,cat,seed=1)
fithourly4 <- fithourly(4,cat,seed=1)

simfmm5 <- simmix(5,cat,fitfmm5s)
simfmmsin4 <- simmixsin(4,cat,fitfmmsin4)
simhomo6 <- simhomo(6,cat,fithomo6s)
simhourly3 <- simhourly(3,cat,fithourly3)
simblock4 <- simblock(4,cat,fitblock4s)
simquad5 <- simquad(5,cat,fitquad5s)
simsin5 <- simsin(5,cat,fitsin5s)

simdat <- data.frame(obs = cat$LogDist,
                    fmm5 = simfmm5$obs,
                    fmmsin4 = simfmmsin4$obs,
                    hmm6 = simhomo6$obs,
                    hmmhourly3 = simhourly3$obs,
                    hmmblock4 = simblock4$obs,
                    hmmquad5 = simquad5$obs,
                    hmmsin5 = simsin5$obs,
                    time = cat$Time)
