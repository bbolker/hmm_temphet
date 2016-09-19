library(depmixS4)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)

load('.cat.15.hmm.RData')

source("stateseqLL.R")

sumdf <- function(lst){
  LL <- ldply(lst,logLik)
  SSLL <- ldply(lst,ssll)
  AIC <- ldply(lst,AIC)
  BIC <- ldply(lst,BIC)
  ICL <- ldply(lst,ICL)
  nstates <-ldply(lst,nstates)
  para <- ldply(lst,freepars)
  n <- ldply(lst,ntimes)
  model <- c('HMM','HMM','HMM','HMM','HMM'
             # ,'HMMsin','HMMsin','HMMsin','HMMsin'
  )
  temp <- data.frame(LL=LL$V1
                     , SSLL=SSLL$V1
                     ,AIC=AIC$V1
                     ,BIC=BIC$V1
                     ,ICL=ICL$V1
                     ,nstates=nstates$V1
                     ,parameters=para$V1
                     ,numobs = n$V1
                     ,model
  )


}

fitlist <- list(fithmm3s,fithmm4s,fithmm5s,fithmm6s,fithmm7s
                # ,hmmsin2s,hmmsin3s,hmmsin4s,hmmsin5s
)

dat <- sumdf(fitlist)

mdat <- melt(dat,id=c("nstates","LL","SSLL","parameters","numobs","model"))
mdat2 <- melt(dat,id=c("nstates","AIC","BIC","ICL","parameters","numobs","model"))

(ggplot(mdat,aes(x=nstates,y=value,color=variable))
+geom_line()
+geom_point()
+theme_bw()
+ggtitle("IC comparisons")
)

(ggplot(mdat2,aes(x=nstates,y=value,color=variable))
+geom_line()
+geom_point()
+theme_bw()
+ggtitle("Likelihood comparison")
)

