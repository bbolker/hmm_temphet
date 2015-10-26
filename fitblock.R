##fit time dependent hmm (block)

library(depmixS4)

fitblock <- function(state,cat,seed=2830){
    model <- depmix(LogDist~1,
                    data=cat,
                    nstate=state,
                    transition=~factor(Block),
                    family=gaussian())
    set.seed(seed)
    fitmodel <- fit(model)
    return(fitmodel)
}


fitblock3s <- fitblock(3,cat)
fitblock4s <- fitblock(4,cat)
fitblock5s <- fitblock(5,cat)
fitblock6s <- fitblock(6,cat)

save(list(c(fitblock3s,fitblock4s,fitblock5s,fitblock6s)),file="hmmblock.RData")
