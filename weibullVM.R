## WEIBULL VM 

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


# depmix model setup ----
load("cat1.RData")
dist <- pmax(cat$Distance,1e-3)
rModels <- list()
rModels[[1]] <- list()
rModels[[1]][[1]] <- weibull(dist, pstart = c(0.5,0.5),data=cat)
rModels[[1]][[2]] <- vonMises(cat$Turningangle, pstart = c(0, 1),data=cat)

rModels[[2]] <- list()
rModels[[2]][[1]] <- weibull(dist, pstart = c(1.5,1.5),data=cat)
rModels[[2]][[2]] <- vonMises(cat$Turningangle, pstart = c(1, 1),data=cat)

rModels[[3]] <- list()
rModels[[3]][[1]] <- weibull(dist, pstart = c(1,4),data=cat)
rModels[[3]][[2]] <- vonMises(cat$Turningangle, pstart = c(3, 1))

trstart <- rep(1/3,9)
transition <- list()
transition[[1]] <- transInit(~ 1, nst = 3, data=cat,
                             pstart = rep(1/3, 3))
transition[[2]] <- transInit(~ 1, nst = 3,data=cat,
                             pstart = rep(1/3, 3))
transition[[3]] <- transInit(~ 1, nst = 3,data=cat,
                             pstart = rep(1/3, 3))
inMod <- transInit(~ 1, ns = 3, pstart = rep(1/3, 3),
                   data = data.frame(1))
mod3 <- makeDepmix(response = rModels, transition = transition,
                   prior=inMod,homogeneous = FALSE)
fm3 <- fit(mod3, verbose = TRUE, emc=em.control(rand=FALSE))
summary(fm3)
