library(dplyr)
library(plyr)
library(ggplot2)
library(depmixS4)
library(tikzDevice)

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

bicplot <- function(df){
  temp <- ggplot(df, aes(x=nstates,y=deltaBIC,colour=model))+facet_wrap(~ type,ncol=4)+
    scale_y_continuous()+
    scale_size_continuous( name="Num of Parameters") +
    geom_point(aes(size=parameters))+
    labs(y=expression(paste(Delta, "BIC")),
         x="Number of States") +
    geom_line(aes(colour=model),size=0.5) +theme_bw() +
    ggtitle("Adjusted BIC Model Comparisons")
  return(temp)
}

sumdat <- sumdf(fitlist)

avgplot <- function(df){
  simdf <- df %>% select(-obs) %>% group_by(time) %>% summarise_each(funs(mean))
  obsdf <- df %>% select(c(obs,time)) %>% filter(!is.na(obs)) %>% group_by(time) %>% summarise_each(funs(mean)) %>% select(obs)
  avgdf <- cbind(simdf,obsdf)
  avgdf2 <- melt(avgdf,'time')
  tempplot<-ggplot(avgdf2,aes(x=time,y=value,colour=variable)) + 
    geom_point() +geom_line()+ xlab("Time of Day") + ylab("Step lengths (log10)")+
    scale_x_continuous(breaks=0:23) + theme_bw() + 
    ggtitle("Average Step Lengths at Different Hours of the Day")
  return(tempplot)
}

acffun <- function(sim){
  a <- acf(sim,na.action=na.pass,plot=FALSE)
  df <- data.frame(ACF=c(a$acf),lag=a$lag)
  return(df)
}

acfplot <- function(df) {
  temp <- df %>% select(-time)%>%ldply(.,acffun)
  tempplot <- ggplot(temp,aes(lag,ACF,colour=.id))+
    geom_point()+geom_line() +theme_bw() +
    ggtitle("ACF plot of BIC Selected Models")
  return(tempplot)
}
