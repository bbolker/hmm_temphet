##fit time homogeneous hmm
library(depmixS4)

fithomo <- function(state,cat,seed=NULL){
  if(!is.null(seed))set.seed(seed)
    model <- depmix(LogDist~1,
                    data=cat,
                    nstate=state,
                    transition=~1,
                    family=gaussian())
    fitmodel <- fit(model)
    return(fitmodel)
}


fithomo3s <- fithomo(3,cat,3)
fithomo4s <- fithomo(4,cat,2)
fithomo5s <- fithomo(5,cat,1)
fithomo6s <- fithomo(6,cat,1)

save(fithomo3s,fithomo4s,fithomo5s,fithomo6s,file="hmmhomo.RData")
