##fit time dependent hmm (block)

fitblock3s <- fitblock(3,cat)
fitblock4s <- fitblock(4,cat)
fitblock5s <- fitblock(35cat)
fitblock6s <- fitblock(6,cat)

save(list(c(fitblock3s,fitblock4s,fitblock5s,fitblock6s)),file="hmmblock.RData")
