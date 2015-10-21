library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)

sumdf <- function(lst){
  BIC <- ldply(lst,BIC)
  nstates <-ldply(lst,nstates)
  para <- ldply(lst,npar)
  model <- c('LSM','LSM','LSM','LSM',
             'LSM + THsin','LSM + THsin','LSM + THsin','LSM + THsin',
             'HMM','HMM','HMM','HMM',
             'HMM + THhourly','HMM + THhourly',
             'HMM + THblock','HMM + THblock','HMM + THblock','HMM + THblock',
             'HMM + THquad','HMM + THquad','HMM + THquad','HMM + THquad',
             'HMM + THsin','HMM + THsin','HMM + THsin','HMM + THsin')
  
  type <- c('LSM','LSM','LSM','LSM',
            'LSM + TH','LSM + TH','LSM + TH','LSM + TH',
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
    scale_size_continuous(expression(paste("    # of \n parameters"))) + 
    geom_point(aes(size=parameters))+ 
    labs(y=expression(paste(Delta,'BIC'))) + 
    geom_line(aes(colour=model),size=0.5) +theme_bw()
  return(temp)
}

bicplot2 <- function(df,t1, t2=null){
  tempdf <- df %>% filter((type == t1) | (type==t2)) %>% mutate(deltabic = BICS - min(BICS)) 
  temp <- ggplot(tempdf, aes(x=nstates,y=deltabic,colour=model))+
    scale_y_continuous()+
    scale_size_continuous(expression(paste("    # of \n parameters"))) + 
    geom_point(aes(size=parameters))+ 
    labs(y=expression(paste(Delta,'BIC'))) + 
    geom_line(aes(colour=model),size=0.5) +theme_bw()
  
  }

bicplot1 <- function(df,t1){
  tempdf <- df %>% filter(type == t1) %>% mutate(deltabic = BICS - min(BICS)) 
  temp <- ggplot(tempdf, aes(x=nstates,y=deltabic,colour=model))+
    scale_y_continuous()+
    scale_size_continuous(expression(paste("     # of \n parameters")))+ 
    geom_point(aes(size=parameters))+ 
    labs(y=expression(paste(Delta,'BIC'))) + 
    geom_line(aes(colour=model),size=0.5) +theme_bw()
  
}


avgplot <- function(df){
  simdf <- df %>% select(-obs) %>% group_by(time) %>% summarise_each(funs(mean))
  obsdf <- df %>% select(c(obs,time)) %>% filter(obs>0) %>% group_by(time) %>% summarise_each(funs(mean)) %>% select(obs)
  avgdf <- cbind(simdf,obsdf)
  avgdf2 <- melt(avgdf,'time')
  tempplot<-ggplot(avgdf2,aes(x=time,y=value,colour=variable)) + 
    geom_point() +geom_line()+
    scale_x_continuous(breaks=0:23) + theme_bw()
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
    geom_point()+geom_line() +theme_bw()
  return(tempplot)
}
  
