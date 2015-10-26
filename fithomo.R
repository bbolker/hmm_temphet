##fit time homogeneous hmm
library(depmixS4)

fithomo <- function(state,cat){
    model <- depmix(LogDist~1,
                    data=cat,
                    nstate=state,
                    transition=~1,
                    family=gaussian())
    fitmodel <- fit(model)
    return(fitmodel)
}


fithomo3s <- fithomo(3,cat)
fithomo4s <- fithomo(4,cat)
fithomo5s <- fithomo(5,cat)
fithomo6s <- fithomo(6,cat)

save(fithomo3s,fithomo4s,fithomo5s,fithomo6s,file="hmmhomo.RData")
