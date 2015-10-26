library(dplyr)
library(plyr)
library(ggplot2)

fitlist <- list(fitfmm3,fitfmm4,fitfmm5,fitfmm6,
               fitfmmsin3,fitfmmsin4,fitfmmsin5,fitfmmsin6,
               fithomo3s,fithomo4s,fithomo5s,fithomo6s,
               fithourly3,fithourly4,
               fitblock3s,fitblock4s,fitblock5s,fitblock6s,
               fitquad3s,fit130quad4s,fitquad5s,fitquad6s,
                fitsin3s,fit130sin4s,fitsin5s,fitsin6s)

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


bicplot <- function(df){
    temp <- ggplot(df, aes(x=nstates,y=deltaBIC,colour=model))+facet_wrap(~ type,ncol=4)+
        scale_y_continuous()+
            scale_size_continuous( name="Num of Para") +
                geom_point(aes(size=parameters))+
                    labs(y=expression("$\\Delta$ BIC")) +
                        geom_line(aes(colour=model),size=0.5) +theme_bw()
    return(temp)
}


sumdat <- sumdf(fitlist)
bicplot(sumdat)

