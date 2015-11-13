library(depmixS4)

fitmixsin <- function(state,cat,seed=NULL){
  if(!is.null(seed))set.seed(seed)
    model <- mix(LogDist~1,
                 data=cat,
                 prior=~cos(2*pi*Time/24)+ sin(2*pi*Time/24),
                 nstate=state,
                 family=gaussian(),
                 initdata=cat)
    set.seed(seed)
    fitmodel <- fit(model)
    return(fitmodel)
}

fitfmmsin3 <- fitmixsin(3,cat,seed=3)
fitfmmsin4 <- fitmixsin(4,cat,seed=1)
fitfmmsin5 <- fitmixsin(5,cat,seed=1)
fitfmmsin6 <- fitmixsin(6,cat.seed=1)

save(fitfmmsin3,fitfmmsin4,fitfmmsin5,fitfmmsin6,file="fitfmmsin.RData")
