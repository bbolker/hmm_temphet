##fit time dependent hmm (quad)
library(depmixS4)

fitquad <- function(state,cat,seed=NULL){
  if(!is.null(seed))set.seed(seed)
    model <- depmix(LogDist~1,
                    data=cat,
                    nstate=state,
                    transition=~I(Time/24)+I((Time/24)^2),
                    family=gaussian())
    set.seed(seed)
    fitmodel <- fit(model)
    return(fitmodel)
}



fitquad3s <- fitquad(3,cat,seed=3030)
fitquad4s <- fitquad(4,cat,seed=3030)
fitquad5s <- fitquad(5,cat,seed=3030)
fitquad6s <- fitquad(6,cat,seed=3030)

save(fitquad3s,fitquad4s,fitquad5s,fitquad6s,file="hmmquad.RData")
