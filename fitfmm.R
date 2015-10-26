library(depmixS4)

fitmix <- function(state,cat,seed=2830){
    model <- mix(LogDist~1,
                 data=cat,
                 prior=~1,
                 nstate=state,
                 family=gaussian(),
                 initdata=cat)
    set.seed(seed)
    fitmodel <- fit(model)
    return(fitmodel)
}

fitfmm3 <- fitmix(3,cat)
fitfmm4 <- fitmix(4,cat)
fitfmm5 <- fitmix(5,cat)
fitfmm6 <- fitmix(6,cat)

save(fitfmm3,fitfmm4,fitfmm5,fitfmm6,file="fitfmm.RData")
