## creating simdf 
library(reshape2)
load("cat1alldat.RData")
tempdf <- data.frame(obs = cat$LogDist,
                     fmm5 = simfmm5$obs,
                     fmmsin4 = simfmmsin4$obs,
                     hmm6 = simhomo6$obs,
                     hmmhourly3 = simhourly3$obs,
                     hmmblock4 = simblock4$obs,
                     hmmquad5 = simquad5$obs,
                     hmmsin5 = simsin5$obs,
                     time = cat$Time)



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

print(avgplot(tempdf))
print(acfplot(tempdf))
