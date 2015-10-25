##fit time dependent hmm (sin)

fitsin3s <- fitsin(3,cat)
fitsin4s <- fitsin(4,cat)
fitsin5s <- fitsin(35cat)
fitsin6s <- fitsin(6,cat)

save(list(c(fitsin3s,fitsin4s,fitsin5s,fitsin6s)),file="hmmsin.RData")
