## depmixS4 weibull setup ----

library(depmixS4)
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
          function(object) {
            dweibull(object@y, 
                    shape = object@parameters$shape, 
                    scale = object@parameters$scale, 
                    log = FALSE)
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
            nas <- is.na(rowSums(object@y))
            start <- with(object@parameters,
                          c(shape=shape,scale=scale))
            objfun <- function(pars) {
#              y.fudge <- ifelse(y==0,1e-3,y)
              L <- -dweibull(as.matrix(object@y[!nas]),pars[1],pars[2],log=TRUE)
              sum(w[!nas]*L)/sum(w[!nas])
            }
            opt <- optim(fn=objfun,par=start,method="Nelder-Mead")
            pars <- unname(c(opt$par[1],opt$par[2]))
#            print(pars)
            object <- setpars(object,pars)
            object
          }
          
)

# testing weibull ----

aa <- rweibull(1000,1,1.5)
bb <- rweibull(1000,3,3) 
dist <- sample(c(aa,bb),1000,replace = FALSE)
cat <- data.frame(dist=dist)
#load("cat1.RData")
#cat <- head(cat,3000)
#dist <- cat$LogDist
rModels <- list()
rModels[[1]] <- list()
rModels[[1]][[1]] <- weibull(dist, pstart = c(0.5,0.5),data=cat)
rModels[[2]] <- list()
rModels[[2]][[1]] <- weibull(dist, pstart = c(1.5,1.5),data=cat)
rModels[[3]] <- list()
rModels[[3]][[1]] <- weibull(dist, pstart = c(1,4),data=cat)

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

mod <- makeDepmix(response = rModels, transition = transition,
                   prior=inMod,homogeneous = FALSE)

fmmod <- fit(mod, verbose = TRUE, emc=em.control(rand=TRUE))
summary(fmmod)





