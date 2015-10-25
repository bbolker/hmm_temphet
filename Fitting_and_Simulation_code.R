##fit and sim functions
library(depmixS4)
library(dplyr)
source("mikesim.R")

###Fit and simulate time-homogeneous HMMs
fithomo <- function(state,cat){
  model <- depmix(LogDist~1,
                  data=cat,
                  nstate=state,
                  transition=~1,
                  family=gaussian())
  fitmodel <- fit(model)
  return(fitmodel)
}

simhomo <- function(state,cat,fit){
  model <- depmix(LogDist~1,
                  data=cat,
                  nstate=state,
                  transition=~1,
                  family=gaussian())
  model<-setpars(model,getpars(fit))
  sim <- simhmm(model)
  df <- data.frame(obs= sim@response[[1]][[1]]@y,states=sim@states)
  return(df)
}

#########################################################################

## Fit and Sim of Hourly-transition HMMs

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

simhourly <- function(state,cat,fit){
  model <- depmix(LogDist~1,
                  data=cat,
                  nstate=state,
                  transition=~factor(Time),
                  family=gaussian())
  model<-setpars(model,getpars(fit))
  sim <- simhmm(model)
  df <- data.frame(obs= sim@response[[1]][[1]]@y,states=sim@states)
  return(df)}
########################################################################

##fit and simulate time-dependent transition HMMs (sin)

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

simsin <- function(state,cat,fit){
  model <- depmix(LogDist~1,
                  data=cat,
                  nstate=state,
                  transition=~cos((2*pi*Time)/24)+ sin((2*pi*Time)/24),
                  family=gaussian())
  model<-setpars(model,getpars(fit))
  sim <- simhmm(model)
  df <- data.frame(obs= sim@response[[1]][[1]]@y,states=sim@states)
  return(df)}
#######################################################################

##fit and simulate time-dependent transition HMMs (quadratic)

fitquad <- function(state,cat,seed=2830){
  model <- depmix(LogDist~1,
                  data=cat,
                  nstate=state,
                  transition=~I(Time/24)+I((Time/24)^2),
                  family=gaussian())
  set.seed(seed)
  fitmodel <- fit(model)
  return(fitmodel)
}

simquad <- function(state,cat,fit){
  model <- depmix(LogDist~1,
                  data=cat,
                  nstate=state,
                  transition=~I(Time/24)+I((Time/24)^2),
                  family=gaussian())
  model<-setpars(model,getpars(fit))
  sim <- simhmm(model)
  df <- data.frame(obs= sim@response[[1]][[1]]@y,states=sim@states)
  return(df)}

###########################################

##fit and simulate time-dependent transition HMMs (block)

fitblock <- function(state,cat,seed=2830){
  model <- depmix(LogDist~1,
                  data=cat,
                  nstate=state,
                  transition=~factor(Block),
                  family=gaussian())
  set.seed(seed)
  fitmodel <- fit(model)
  return(fitmodel)
}

simblock <- function(state,cat,fit){
  model <- depmix(LogDist~1,
                  data=cat,
                  nstate=state,
                  transition=~factor(Block),
                  family=gaussian())
  model<-setpars(model,getpars(fit))
  sim <- simhmm(model)
  df <- data.frame(obs= sim@response[[1]][[1]]@y,states=sim@states)
  return(df)}

###########################################################

##fit and simulate FMMs

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

simmix <- function(state,cat,fit){
  model <- mix(LogDist~1,
               data=cat,
               prior=~1,
               nstate=state,
               family=gaussian(),
               initdata=cat)
  model<-setpars(model,getpars(fit))
  sim <- simhmm(model)
  df <- data.frame(obs= sim@response[[1]][[1]]@y,states=sim@states)
  return(df)
}

##fit and simulate time-dependent FMM (Sin)

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

simmixsin <- function(state,cat,fit){
  model <- mix(LogDist~1,
               data=cat,
               prior=~cos((2*pi*Time)/24)+ sin((2*pi*Time)/24),
               nstate=state,
               family=gaussian(),
               initdata=cat)
  model<-setpars(model,getpars(fit))
  sim <- simulate(model)
  df <- data.frame(obs= sim@response[[1]][[1]]@y,states=sim@states)
  return(df)
}

    
