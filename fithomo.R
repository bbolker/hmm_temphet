##fit time homogeneous hmm

fithomo3s <- fithomo(3,cat)
fithomo4s <- fithomo(4,cat)
fithomo5s <- fithomo(35cat)
fithomo6s <- fithomo(6,cat)

save(list(c(fithomo3s,fithomo4s,fithomo5s,fithomo6s)),file="hmmhomo.RData")
