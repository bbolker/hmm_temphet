library(dplyr)
library(plyr)
library(ggplot2)
library(depmixS4)
library(circular)


# VM setup ----
setClass("vonMises", contains="response")
## library(VGAM)  ## this may mess up multinomial()


setGeneric("vonMises", function(y, pstart = NULL, fixed = NULL, ...) 
  standardGeneric("vonMises"))

setMethod("vonMises", 
          signature(y = "ANY"), 
          function(y, pstart = NULL, fixed = NULL, ...) {
            y <- matrix(y, length(y))
            x <- matrix(1) 
            parameters <- list()
            npar <- 2
            if(is.null(fixed)) fixed <- as.logical(rep(0, npar))
            if(!is.null(pstart)) {
              if(length(pstart) != npar) stop("length of 'pstart' must be ", npar)
              parameters$mu <- pstart[1]
              parameters$kappa <- pstart[2]
            }
            mod <- new("vonMises", parameters = parameters, fixed = fixed,
                       x = x, y = y, npar = npar)
            mod
          }
)

setMethod("dens","vonMises",
          function(object,log=FALSE) {
            dvonmises(object@y, mu = predict(object), 
                      kappa = object@parameters$kappa, 
                      log = log)
          }
)

setMethod("getpars","response",
          function(object,which="pars",...) {
            switch(which,
                   "pars" = {
                     parameters <- numeric()
                     parameters <- unlist(object@parameters)
                     pars <- parameters
                   },
                   "fixed" = {
                     pars <- object@fixed
                   }
            )
            return(pars)
          }
)

setMethod("setpars","vonMises",
          function(object, values, which="pars", ...) {
            npar <- npar(object)
            if(length(values)!=npar) stop("length of 'values' must be",npar)
            # determine whether parameters or fixed constraints are being set
            nms <- names(object@parameters)
            switch(which,
                   "pars"= {
                     object@parameters$mu <- values[1]
                     object@parameters$kappa <- values[2]
                   },
                   "fixed" = {
                     object@fixed <- as.logical(values)
                   }
            )
            names(object@parameters) <- nms
            return(object)
          }
)

setMethod("predict","vonMises", 
          function(object) {
            ret <- object@parameters$mu
            return(ret)
          }
)


##Everything above ""should"" be fine

##*****************************
dvonmisesrad <- circular:::DvonmisesRad
setMethod("fit", "vonMises",
          function(object, w) {
            y <- object@y
            nas <- is.na(rowSums(object@y))
            start <- with(object@parameters,
                          c(mu=mu,logk=log(kappa)))
            objfun <- function(pars) {
              L <- -dvonmisesrad(as.matrix(object@y[!nas,]),pars[1],exp(pars[2]),log=TRUE)
              sum(w[!nas]*L)/sum(w[!nas])
            }
            opt <- optim(fn=objfun,par=start,method="Nelder-Mead")
            pars <- unname(c(opt$par[1],exp(opt$par[2])))
            object <- setpars(object,pars)
            object
          }
          
)

# weibull setup ----
setClass("weibull", contains="response")
library(MASS)
library(stats)
data(speed)
setGeneric("weibull", function(y, pstart = NULL, fixed = NULL, ...) 
  standardGeneric("weibull"))

setMethod("weibull", 
          signature(y = "ANY"), 
          function(y, pstart = NULL, fixed = NULL, ...) {
            y <- matrix(y, length(y))
            x <- matrix(1) 
            parameters <- list()
            npar <- 2
            if(is.null(fixed)) fixed <- as.logical(rep(0, npar))
            if(!is.null(pstart)) {
              if(length(pstart) != npar) stop("length of 'pstart' must be ", npar)
              parameters$shape <- pstart[1]
              parameters$scale <- pstart[2]
            }
            mod <- new("weibull", parameters = parameters, fixed = fixed,
                       x = x, y = y, npar = npar)
            mod
          }
)

setMethod("dens","weibull",
          function(object, log=FALSE) {
            dweibull(object@y, 
                     shape = object@parameters$shape, 
                     scale = object@parameters$scale, 
                     log = log)
          }
)

setMethod("getpars","response",
          function(object,which="pars",...) {
            switch(which,
                   "pars" = {
                     parameters <- numeric()
                     parameters <- unlist(object@parameters)
                     pars <- parameters
                   },
                   "fixed" = {
                     pars <- object@fixed
                   }
            )
            return(pars)
          }
)

setMethod("setpars","weibull",
          function(object, values, which="pars", ...) {
            npar <- npar(object)
            if(length(values)!=npar) stop("length of 'values' must be",npar)
            # determine whether parameters or fixed constraints are being set
            nms <- names(object@parameters)
            switch(which,
                   "pars"= {
                     object@parameters$shape <- values[1]
                     object@parameters$scale <- values[2]
                   },
                   "fixed" = {
                     object@fixed <- as.logical(values)
                   }
            )
            names(object@parameters) <- nms
            return(object)
          }
)

setMethod("fit", "weibull",
          function(object, w) {
            y <- object@y
            nas <- is.na(rowSums(y))
            start <- with(object@parameters,
                          c(logshape=log(shape),logscale=log(scale)))
            objfun <- function(pars) {
              L <- -dweibull(c(na.omit(y)),
                             exp(pars[1]),exp(pars[2]),log=TRUE)
              sum(w[!nas]*L)/sum(w[!nas])
            }
            opt <- optim(fn=objfun,par=start,method="Nelder-Mead")
            pars <- unname(exp(opt$par))
            #            print(pars)
            object <- setpars(object,pars)
            object
          }
          
)

# making bic plot ----

fitlist <- list(WVMHMM3s,WVMHMM4s,WVMHMM5s,WVMHMM6s,
                LNVM3s,LNVM4s,LNVM5s,LNVM6s,
                weibull3s,weibull4s,weibull5s,weibull6s,
                fithomo3s,fithomo4s,fithomo5s,fithomo6s,
                WVMsin3,WVMsin4,WVMsin5,WVMsin6,
                LNVMsin3,LNVMsin4,LNVMsin5,LNVMsin6,
                Wsin3,Wsin4,Wsin5,Wsin6,
                fitsin3s,fitsin4s,fitsin5s,fitsin6s)

sumdf <- function(lst){
  BIC2 <- ldply(lst,BIC)
  nstates <-ldply(lst,nstates)
  model <- c('A WVM-HMM','A WVM-HMM','A WVM-HMM','A WVM-HMM',
             'A LNVM-HMM','A LNVM-HMM','A LNVM-HMM','A LNVM-HMM',
             'B Weibull-HMM','B Weibull-HMM','B Weibull-HMM','B Weibull-HMM',
             'B LN-HMM','B LN-HMM', 'B LN-HMM', 'B LN-HMM',
             'C WVM-Time_het_sin','C WVM-Time_het_sin','C WVM-Time_het_sin','C WVM-Time_het_sin',
             'C LNVM-Time_het_sin','C LNVM-Time_het_sin','C LNVM-Time_het_sin','C LNVM-Time_het_sin',
             'D W-Time_het_sin','D W-Time_het_sin','D W-Time_het_sin','D W-Time_het_sin',
             'D LN-Time_het_sin','D LN-Time_het_sin','D LN-Time_het_sin','D LN-Time_het_sin')
  temp <- data.frame(BICS=BIC2$V1,nstates=nstates$V1,model=model)
  return(temp)
}

bicplot <- function(df){
  temp <- ggplot(df, aes(x=nstates,y=deltaBIC,colour=model))+facet_wrap(~ model,ncol=4)+
    scale_y_continuous()+
    scale_size_continuous( name="Num of Parameters") +
    #    geom_point(aes(size=parameters))+
    labs(y=expression("$\\Delta$ BIC"),
         x="Number of States") +
    geom_line(aes(colour=model),size=0.5) +theme_bw()
  return(temp)
}

ll <- sumdf(fitlist)

ll$deltaBIC[1:4] <- ll$BICS[1:4] - min(ll$BICS[1:4])
ll$deltaBIC[5:8] <- ll$BICS[5:8] - min(ll$BICS[5:8])
ll$deltaBIC[9:12] <- ll$BICS[9:12] - min(ll$BICS[9:12])
ll$deltaBIC[13:16] <- ll$BICS[13:16] - min(ll$BICS[13:16])
ll$deltaBIC[17:20] <- ll$BICS[17:20] - min(ll$BICS[17:20])
ll$deltaBIC[21:24] <- ll$BICS[21:24] - min(ll$BICS[21:24])
ll$deltaBIC[25:28] <- ll$BICS[25:28] - min(ll$BICS[25:28])
ll$deltaBIC[29:32] <- ll$BICS[29:32] - min(ll$BICS[29:32])
bicplot(ll)
