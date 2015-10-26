library(depmixS4)

fithourly <- function(state,cat,seed=2830){
    model <- depmix(LogDist~1,
                    data=cat,
                    nstate=state,
                    transition=~factor(Time),
                    family=gaussian())
    set.seed(seed)
    fitmodel <- fit(model)
    return(fitmodel)
}

fithourly3 <- fithourly(3,cat)
fithourly4 <- fithourly(4,cat)

save(fithourly3,fithourly4,file="fithourly.RData")
