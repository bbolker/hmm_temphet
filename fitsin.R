##fit time dependent hmm (sin)

library(depmixS4)

fitsin <- function(state,cat,seed=2830){
    model <- depmix(LogDist~1,
                    data=cat,
                    nstate=state,
                    transition=~cos(2*pi*Time/24)+ sin(2*pi*Time/24),
                    family=gaussian())
    set.seed(seed)
    fitmodel <- fit(model)
    return(fitmodel)
}


fitsin3s <- fitsin(3,cat)
fitsin4s <- fitsin(4,cat)
fitsin5s <- fitsin(5,cat)
fitsin6s <- fitsin(6,cat)

save(fitsin3s,fitsin4s,fitsin5s,fitsin6s,file="hmmsin.RData")
