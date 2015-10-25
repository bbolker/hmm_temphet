library(depmixS4)
library(dplyr)
source("mikesim.R")

mod <- function(cat,state){
    model <- depmix(LogDist~1,
                    data=cat,
                    nstate=state,
                    transition=~1,
                    family=gaussian()
                    )
    return(model)
}

###Fit and simulate time-homogeneous HMMs
fithomo <- function(cat,state,model=mod(cat,state)){
    model <- mod
    fitmodel <- fit(model)
    return(fitmodel)
}

fitsin <- function(cat,state,model=mod(cat,state)){
    model <- update(mod,.transition =
                            ~cos(2*pi*Time/24)+ sin(2*pi*Time/24))
    fitmodel <- fit(model)
    return(fitmodel)
}

