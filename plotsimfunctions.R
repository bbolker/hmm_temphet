
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
