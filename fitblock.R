##fit time dependent hmm (block)

library(depmixS4)

fitblock <- function(state,cat,seed=NULL){
  if(!is.null(seed))set.seed(seed)
    model <- depmix(LogDist~1,
                    data=cat,
                    nstate=state,
                    transition=~factor(Block),
                    family=gaussian())
    set.seed(seed)
    fitmodel <- fit(model)
    return(fitmodel)
}


fitblock3s <- fitblock(3,cat,seed=2830)
fitblock4s <- fitblock(4,cat,seed=2830)
fitblock5s <- fitblock(5,cat,seed=2830)
fitblock6s <- fitblock(6,cat,seed=2830)

save(fitblock3s,fitblock4s,fitblock5s,fitblock6s,file="hmmblock.RData")
