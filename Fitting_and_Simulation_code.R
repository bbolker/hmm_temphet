##fit and sim functions
library(depmixS4)
library(dplyr)
source("mikesim.R")

###Fit and simulate time-homogeneous HMMs
fithomo <- function(state,catnum){
  cat <- subset(dat,cat==catnum)
  model <- depmix(distance~1,
                  data=cat,
                  nstate=state,
                  transition=~1,
                  family=gaussian())
  fitmodel <- fit(model)
  return(fitmodel)
}

simhomo <- function(state,catnum,fit){
  cat <- subset(dat,cat==catnum)
  model <- depmix(distance~1,
                  data=cat,
                  nstate=state,
                  transition=~1,
                  family=gaussian())
  model<-setpars(model,getpars(fit))
  sim <- simhmm(model)
  df <- data.frame(obs= sim@response[[1]][[1]]@y,states=sim@states)
  return(df)
}

q()
#########################################################################

## Fit and Sim of Hourly-transition HMMs

fithourly <- function(state,catnum,seed=2830){
  cat <- subset(HPD,CatID==catnum)
  model <- depmix(distance~1,
                  data=cat,
                  nstate=state,
                  transition=~factor(Hour),
                  family=gaussian())
  set.seed(seed)
  fitmodel <- fit(model)
  return(fitmodel)
}

simhourly <- function(state,catnum,fit){
  cat <- subset(HPD,CatID==catnum)
  model <- depmix(distance~1,
                  data=cat,
                  nstate=state,
                  transition=~factor(Hour),
                  family=gaussian())
  model<-setpars(model,getpars(fit))
  sim <- simhmm(model)
  df <- data.frame(obs= sim@response[[1]][[1]]@y,states=sim@states)
  return(df)}
########################################################################

##fit and simulate time-dependent transition HMMs (sin)

fitsin <- function(state,catnum,seed=2830){
  cat <- subset(HPD,CatID==catnum)
  model <- depmix(distance~1,
                  data=cat,
                  nstate=state,
                  transition=~cos(2*pi*Hour/24)+ sin(2*pi*Hour/24),
                  family=gaussian())
  set.seed(seed)
  fitmodel <- fit(model)
  return(fitmodel)
}

simsin <- function(state,catnum,fit){
  cat <- subset(HPD,CatID==catnum)
  model <- depmix(distance~1,
                  data=cat,
                  nstate=state,
                  transition=~cos((2*pi*Hour)/24)+ sin((2*pi*Hour)/24),
                  family=gaussian())
  model<-setpars(model,getpars(fit))
  sim <- simhmm(model)
  df <- data.frame(obs= sim@response[[1]][[1]]@y,states=sim@states)
  return(df)}
#######################################################################

##fit and simulate time-dependent transition HMMs (quadratic)

fitquad <- function(state,catnum,seed=2830){
  cat <- subset(HPD,CatID==catnum)
  model <- depmix(distance~1,
                  data=cat,
                  nstate=state,
                  transition=~I(newtime)+I(newtime^2),
                  family=gaussian())
  set.seed(seed)
  fitmodel <- fit(model)
  return(fitmodel)
}

simquad <- function(state,catnum,fit){
  cat <- subset(HPD,CatID==catnum)
  model <- depmix(distance~1,
                  data=cat,
                  nstate=state,
                  transition=~I(newtime)+I(newtime^2),
                  family=gaussian())
  model<-setpars(model,getpars(fit))
  sim <- simhmm(model)
  df <- data.frame(obs= sim@response[[1]][[1]]@y,states=sim@states)
  return(df)}

###########################################

##fit and simulate time-dependent transition HMMs (block)

fitblock <- function(state,catnum,seed=2830){
  cat <- subset(HPD,CatID==catnum)
  model <- depmix(distance~1,
                  data=cat,
                  nstate=state,
                  transition=~factor(period),
                  family=gaussian())
  set.seed(seed)
  fitmodel <- fit(model)
  return(fitmodel)
}

simblock <- function(state,catnum,fit){
  cat <- subset(HPD,CatID==catnum)
  model <- depmix(distance~1,
                  data=cat,
                  nstate=state,
                  transition=~factor(period),
                  family=gaussian())
  model<-setpars(model,getpars(fit))
  sim <- simhmm(model)
  df <- data.frame(obs= sim@response[[1]][[1]]@y,states=sim@states)
  return(df)}

###########################################################

##fit and simulate FMMs

fitmix <- function(state,catnum,seed=2830){
  cat <- subset(HPD,CatID==catnum)
  model <- mix(distance~1,
               data=cat,
               prior=~1,
               nstate=state,
               family=gaussian(),
               initdata=cat)
  set.seed(seed)
  fitmodel <- fit(model)
  return(fitmodel)
}

simmix <- function(state,catnum,fit){
  cat <- subset(HPD,CatID==catnum)
  model <- mix(distance~1,
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

fitmixsin <- function(state,catnum,seed=2830){
  cat <- subset(HPD,CatID==catnum)
  model <- mix(distance~1,
               data=cat,
               prior=~cos(2*pi*Hour/24)+ sin(2*pi*Hour/24),
               nstate=state,
               family=gaussian(),
               initdata=cat)
  set.seed(seed)
  fitmodel <- fit(model)
  return(fitmodel)
}

simmixsin <- function(state,catnum,fit){
  cat <- subset(HPD,CatID==catnum)
  model <- mix(distance~1,
               data=cat,
               prior=~cos((2*pi*Hour)/24)+ sin((2*pi*Hour)/24),
               nstate=state,
               family=gaussian(),
               initdata=cat)
  model<-setpars(model,getpars(fit))
  sim <- simulate(model)
  df <- data.frame(obs= sim@response[[1]][[1]]@y,states=sim@states)
  return(df)
}

