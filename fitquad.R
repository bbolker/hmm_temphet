##fit time dependent hmm (quad)

fitquad3s <- fitquad(3,cat)
fitquad4s <- fitquad(4,cat)
fitquad5s <- fitquad(35cat)
fitquad6s <- fitquad(6,cat)

save(list(c(fitquad3s,fitquad4s,fitquad5s,fitquad6s)),file="hmmquad.RData")
