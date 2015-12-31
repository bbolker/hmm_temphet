library(dplyr)
library(plyr)

fitlist <- list(fitfmm3s,fitfmm4s,fitfmm5s,fitfmm6s,
                fitfmmsin3,fitfmmsin4,fitfmmsin5,fitfmmsin6,
                fithomo3s,fithomo4s,fithomo5s,fithomo6s,
                fithourly3,fithourly4,
                fitblock3s,fitblock4s,fitblock5s,fitblock6s,
                fitquad3s,fitquad4s,fitquad5s,fitquad6s,
                fitsin3s,fitsin4s,fitsin5s,fitsin6s)

sumdf <- function(lst){
  BIC <- ldply(lst,BIC)
  nstates <-ldply(lst,nstates)
  para <- ldply(lst,npar)
  model <- c('FMM','FMM','FMM','FMM',
             'FMM + THsin','FMM + THsin','FMM + THsin','FMM + THsin',
             'HMM','HMM','HMM','HMM',
             'HMM + THhourly','HMM + THhourly',
             'HMM + THblock','HMM + THblock','HMM + THblock','HMM + THblock',
             'HMM + THquad','HMM + THquad','HMM + THquad','HMM + THquad',
             'HMM + THsin','HMM + THsin','HMM + THsin','HMM + THsin')
  
  type <- c('FMM','FMM','FMM','FMM',
            'FMM + TH','FMM + TH','FMM + TH','FMM + TH',
            'HMM','HMM','HMM','HMM',
            'HMM + TH','HMM + TH',
            'HMM + TH','HMM + TH','HMM + TH','HMM + TH',
            'HMM + TH','HMM + TH','HMM + TH','HMM + TH',
            'HMM + TH','HMM + TH','HMM + TH','HMM + TH')
  
  deltaBIC <- BIC-min(BIC)
  temp <- data.frame(BICS=BIC$V1,deltaBIC=deltaBIC$V1,nstates=nstates$V1,parameters=para$V1,model,type)
  return(temp)
}


catsummary <- sumdf(fitlist)

save(list=c("sumdf"),file="cat1sumdat.RData")
