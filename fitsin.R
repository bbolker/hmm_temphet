##fit time dependent hmm (sin)

library(depmixS4)

fitsin <- function(state,cat,seed=NULL){
  if(!is.null(seed))set.seed(seed)
    model <- depmix(LogDist~1,
                    data=cat,
                    nstate=state,
                    transition=~cos(2*pi*Time/24)+ sin(2*pi*Time/24),
                    family=gaussian())
    set.seed(seed)
    fitmodel <- fit(model)
    return(fitmodel)
}


fitsin3s <- fitsin(3,cat,seed=4)
fitsin4s <- fitsin(4,cat,seed=4)
fitsin5s <- fitsin(5,cat,seed=4)
fitsin6s <- fitsin(6,cat,seed=4)

save(fitsin3s,fitsin4s,fitsin5s,fitsin6s,file="hmmsin.RData")
