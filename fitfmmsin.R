library(depmixS4)

fitmixsin <- function(state,cat,seed=2830){
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

fitfmmsin3 <- fitmixsin(3,cat)
fitfmmsin4 <- fitmixsin(4,cat)
fitfmmsin5 <- fitmixsin(5,cat)
fitfmmsin6 <- fitmixsin(6,cat)

save(fitfmmsin3,fitfmmsin4,fitfmmsin5,fitfmmsin6,file="fitfmmsin.RData")
