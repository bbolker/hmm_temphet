library(ggplot2)
library(depmixS4)
library(tikzDevice)


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
  simdf <- df %>% dplyr:::select(-obs) %>% group_by(time) %>% summarise_each(funs(mean))
  obsdf <- df %>% dplyr:::select(c(obs,time)) %>% filter(!is.na(obs)) %>% group_by(time) %>% summarise_each(funs(mean)) %>% dplyr:::select(obs)
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
  temp <- df %>% dplyr:::select(-time)%>%ldply(.,acffun)
  tempplot <- ggplot(temp,aes(lag,ACF,colour=.id))+
    geom_point()+geom_line() +theme_bw() +
    ggtitle("ACF plot of BIC Selected Models")
  return(tempplot)
}

msd56 <- function(mod5,mod6) {
  temp5 <- tail(getpars(mod5),10)
  temp6 <- tail(getpars(mod6),12)
  
  dat1 <- data.frame(log_mean=temp5[c(1,3,5,7,9)],
                     log_sd=temp5[c(2,4,6,8,10)], states="5") 
  dat2 <- data.frame(log_mean=temp6[c(1,3,5,7,9,11)],
                     log_sd=temp6[c(2,4,6,8,10,12)],states="6") 
  
  dat1 <- dat1[order(dat1$log_mean),]
  dat2 <- dat2[order(dat2$log_mean),]
  
  dat <- rbind(dat1,dat2)
  tempplot <- ggplot(dat,aes(log_mean,log_sd,color=states))+
    geom_point()+geom_line() +theme_bw() +
    ggtitle("State Parameters")
  return(tempplot)
}


msd45 <- function(mod4,mod5) {
  temp4 <- tail(getpars(mod4),8)
  temp5 <- tail(getpars(mod5),10)
  
  dat1 <- data.frame(log_mean=temp4[c(1,3,5,7)],
                     log_sd=temp4[c(2,4,6,8)], states="4") 
  dat2 <- data.frame(log_mean=temp5[c(1,3,5,7,9)],
                     log_sd=temp5[c(2,4,6,8,10)],states="5") 
  
  dat1 <- dat1[order(dat1$log_mean),]
  dat2 <- dat2[order(dat2$log_mean),]
  
  dat <- rbind(dat1,dat2)
  tempplot <- ggplot(dat,aes(log_mean,log_sd,color=states))+
    geom_point()+geom_line() +theme_bw() +
    ggtitle("State Parameters")
  return(tempplot)
}

vitcat <- function(catt,mod){
  params <- tail(getpars(mod),6)
  vitdist <- numeric(ntimes(mod))
  states <- viterbi(mod)$state
  for(i in 1:ntimes(mod)){
    if(states[i] == 1){
      vitdist[i] <- rnorm(1,params[1],params[2])
    }
    if(states[i] == 2){
      vitdist[i] <- rnorm(1,params[3],params[4])
    }
    if(states[i] == 3){
      vitdist[i] <- rnorm(1,params[5],params[6])
    }
  }
  dat <- data.frame(viterbi=vitdist,time=catt$Time, obs=catt$LogDist)
  return(dat)
}

vitcat5 <- function(catt,mod){
  params <- tail(getpars(mod),10)
  vitdist <- numeric(ntimes(mod))
  states <- viterbi(mod)$state
  for(i in 1:ntimes(mod)){
    if(states[i] == 1){
      vitdist[i] <- rnorm(1,params[1],params[2])
    }
    if(states[i] == 2){
      vitdist[i] <- rnorm(1,params[3],params[4])
    }
    if(states[i] == 3){
      vitdist[i] <- rnorm(1,params[5],params[6])
    }
    if(states[i] == 4){
      vitdist[i] <- rnorm(1,params[7],params[8])
    }
    if(states[i] == 5){
      vitdist[i] <- rnorm(1,params[9],params[10])
    }
  }
  dat <- data.frame(viterbi=vitdist,time=catt$Time, obs=catt$LogDist)
  return(dat)
}

