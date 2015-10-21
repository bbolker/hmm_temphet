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
  sim <- simhmm(model)
  df <- data.frame(obs= sim@response[[1]][[1]]@y,states=sim@states)
  return(df)
}


###########################################

###Gaussian Simulation test

##simulatte function 


linkinvfun <- function(family,eta,base=1) {
  linkinv <- family$linkinv
  if ("base" %in% names(formals(linkinv))) {
    ## zero hack
    if (is.matrix(eta)) eta[,base] <- 0 else eta[base] <- 0
    prob <- linkinv(eta,base=1)
  } else prob <- linkinv(eta)
  return(prob)
}

## generate simulated values given a response and a linear predictor
sim <- function(resp,eta) {
  res <- list(fitted.values=eta,
              family=resp@family)
  if (res$family$family=="multinomial") {
    nstate <- length(eta)
    prob <- linkinvfun(res$family,eta)
    sample(nstate,size=1,prob=prob)
  } else {
    if (res$family$family=="gaussian") {
      res <- c(res,list(deviance=resp@parameters$sd^2,
                        df.residual=1))
    }
    ## handles the Poisson, binomial, ... cases
    ## (probably doesn't handle Gamma correctly)
    stats:::simulate.lm(res)
  }
}

simFun <- function(mod,data=NULL) {
  ## FIXME: shouldn't require data if no covariate dependence
  if (is.null(data)) stop("need to specify data for simulation")
  nodep <- !is(mod,"depmix")
  curstate <- 1
  base <- 1 ## for now?
  ntimes <- if (nodep) {
    length(mod@ntimes)
  } else {
    mod@ntimes   ## number of times
  }
  if (length(ntimes)>1)
    stop("can't handle multi-response cases yet")
  ## FIXME: this shouldn't be too hard, since all the time
  ##  series have the same parameters -- just run the simulation
  ##  one time for each chunk of the data frame, then stick them
  ##  together
  nresp <- mod@nresp #number of response type
  nstates <- mod@nstates #number of states
  state <- numeric(ntimes) 
  obs <- matrix(0,nrow=ntimes,ncol=nresp) ## make a ntimes x nresp matrix
  ## start in state 1 for now
  state[1] <- 1
  if (!nodep) {
    ## intercept of 1s and the formula for transition 
    Xtrans <- model.matrix(mod@transition[[1]]@formula,data)
  } else {
    Xprior <- model.matrix(mod@prior@formula,data)
  }
  rforms <- lapply(mod@response[[1]],"slot","formula") #responses formula
  Xresp <- lapply(rforms,model.matrix,data=data)
  for (i in 1:(ntimes)) {
    ## simulate observations for _current_ step
    for (j in 1:nresp) {
      eta <-
        Xresp[[j]][i,] %*%
        mod@response[[state[i]]][[j]]@parameters$coefficients
      sval <- sim(mod@response[[1]][[j]],eta)
      ## cat(i,j,sval[[1]],"\n")
      obs[i,j] <- sval[[1]]
    }
    ## simulate state for _next_ step
    if (!nodep) { if(length(Xtrans[i,])==1){eta <- mod@transition[[state[i]]]@parameters$coefficients
                                            probs <- linkinvfun(mod@transition[[state[i]]]@family,
                                                                eta)}
                  else{
                    eta <- Xtrans[i,] %*%
                      mod@transition[[state[i]]]@parameters$coefficients
                    probs <- linkinvfun(mod@transition[[state[i]]]@family,
                                        eta)}
    } else { if (length(Xprior[i,])==1){eta <-mod@prior@parameters$coefficients
                                        probs <- linkinvfun(mod@prior@family,eta)}
             else{
               eta <- Xprior[i,] %*%
                 mod@prior@parameters$coefficients
               probs <- linkinvfun(mod@prior@family,eta)
             }}
    ## FIXME: doesn't handle non-equal priors for depmix models
    newstate <- sample(nstates,size=1,prob=probs)
    ## cat(i,newstate,"\n")        
    state[i+1] <- newstate
  }
  list(state=state,obs=obs)
}

######################
marks <- rep(1,12000)
period=(rep(rep(c("day","night"),each=12),500))
dat <- data.frame(marks,period)

#setting up the transition
mm <- mlogit()
trvec <- c(mm$linkinv(c(0,-3),base=1),
           0,4,
           mm$linkinv(c(0,3),base=1),
           0,-4)

truemodel2T <- depmix(marks~1,
                      data=dat,
                      nstates=2,
                      family=gaussian(),
                      trstart= trvec,
                      transition=~factor(period),
                      respstart=c(3,1,4.5,1))
set.seed(2830)
sim1 <- simFun(truemodel2T,dat)

plot(sim1$obs)

model2T <- depmix(sim1$obs~1,
                  data=dat,
                  nstates=2,
                  family=gaussian(),
                  transition=~factor(period),
                  respstart=c(3,0.7,5,0.8))
set.seed(2830)
fit2T <- fit(model2T,emc=em.control(rand=FALSE))

fit2T

model3T <- depmix(sim1$obs~1,
                  data=dat,
                  nstates=3,
                  family=gaussian(),
                  transition=~factor(period),
                  respstart=c(3,0.5,4,0.5,5,0.5))
set.seed(2830)
fit3T <- fit(model3T, emc=em.control(rand=FALSE))

model2H <- depmix(sim1$obs~1,
                  data=dat,
                  family=gaussian(),
                  nstates=2,
                  respstart=c(3,0.7,5,0.8))
set.seed(2830)
fit2H <- fit(model2H,emc=em.control(rand=FALSE))


model3H <- depmix(sim1$obs~1,
                  data=dat,
                  family=gaussian(),
                  nstates=3,
                  respstart=c(3,0.5,3.7,0.5,4.6,0.5))
set.seed(2830)
fit3H <- fit(model3H,emc=em.control(rand=FALSE))


model4H <- depmix(sim1$obs~1,
                  data=dat,
                  family=gaussian(),
                  nstates=4,
                  respstart=c(3,0.5,3.5,0.5,4,0.5,4.5,0.5))
set.seed(2830)
fit4H <- fit(model4H,emc=em.control(rand=FALSE))


#####

