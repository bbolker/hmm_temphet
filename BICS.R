library(depmixS4)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)


loglik <- ldply(list(fit130homo3,fit130homo4,fit130homo5,fit130homo6,
                     fit130hourly3,fit130hourly4,
                     fit130block3,fit130block4,fit130block5,fit130block6,
                     fit130quad3,fit130quad4,fit130quad5,fit130quad6,
                     fit130sin3,fit130sin4,fit130sin5,fit130sin6),logLik)


loglik <- loglik - min(loglik)
state <- c(3,4,5,6,3,4,3,4,5,6,3,4,5,6,3,4,5,6)
type <- c('Homogeneous','Homogeneous','Homogeneous','Homogeneous','Hourly','Hourly',
          'Block','Block','Block','Block','Quadratic','Quadratic','Quadratic','Quadratic','Sinusoidal',
          'Sinusoidal','Sinusoidal','Sinusoidal')
bics <- ldply(list(fit130homo3,fit130homo4,fit130homo5,fit130homo6,
                   fit130hourly3,fit130hourly4,
                   fit130block3,fit130block4,fit130block5,fit130block6,
                   fit130quad3,fit130quad4,fit130quad5,fit130quad6,
                   fit130sin3,fit130sin4,fit130sin5,fit130sin6),BIC)
lengthpars <- function(model){
  return(length(getpars(model)))
}

parameters <- ldply(list(fit130homo3,fit130homo4,fit130homo5,fit130homo6,
                         fit130hourly3,fit130hourly4,
                         fit130block3,fit130block4,fit130block5,fit130block6,
                         fit130quad3,fit130quad4,fit130quad5,fit130quad6,
                         fit130sin3,fit130sin4,fit130sin5,fit130sin6),lengthpars)
bics <- bics - min(bics)
simdf <- data.frame(loglik,state,bics,type,parameters)

bicsred <- ldply(list(fit130homo3,fit130homo4,fit130homo5,fit130homo6,
                   fm3,fm4,fm5,fm6),BIC)

bicsred[1:4,] <- bicsred[1:4,]- min(bicsred[1:4,])
bicsred[5:8,] <- bicsred[5:8,]- min(bicsred[5:8,])

parametersred <- ldply(list(fit130homo3,fit130homo4,fit130homo5,fit130homo6,
                         fm3,fm4,fm5,fm6),lengthpars)

typered <- c('LN','LN','LN','LN','LNvM','LNvM','LNvM','LNvM')
statesred <- c(3,4,5,6,3,4,5,6)
simdfred <- data.frame(statesred,bicsred,typered,parametersred)





getACF <- function(a) {
  data.frame(ACF=c(a$acf),lag=a$lag)
}
cat130 <- subset(HPD,HPD$CatID==130)
a1 <- acf(cat130$distance,na.action=na.pass,plot=FALSE)
a2 <- acf(sim130homo6,plot=FALSE)
a3 <- acf(sim130hourly3,plot=FALSE)
a4 <- acf(sim130block4,plot=FALSE)
a5 <- acf(sim130quad5,plot=FALSE)
a6 <- acf(sim130sin5, plot=FALSE)

allm.acf <- ldply(list(obs=a1,sim6H=a2,simHourly=a3, 
                       sim4Block=a4,sim5Quad=a5,sim5Sin=a6),getACF)
allmacf <- ggplot(allm.acf,aes(lag,ACF,colour=.id))+scale_colour_brewer(palette="Set1")+
  geom_point()+geom_line() + ggtitle('Cat130 Observation vs Simulation ACF')+theme_bw()

cat130 <- subset(HPD,HPD$CatID==130)

simavgdf <- data.frame(sim6H=sim130homo6,simhourly=sim130hourly3,
                       sim4B=sim130block4,sim5Q=sim130quad5,
                       sim5S=sim130sin5,time=cat130$Hour)

obsavgdf <- data.frame(obs=cat130$distance,time=cat130$Hour)

avgsimdf <- simavgdf %>% group_by(time) %>% summarise_each(funs(mean))
avgobsdf <- obsavgdf %>% filter(obs>0) %>% group_by(time) %>% summarise_each(funs(mean))

avgdf <- cbind(avgsimdf,avgobsdf)
time2 <-  rep(c(0:23),6)
type2 <- rep(c('6H','Hourly','4B','5Q','5S','obs'),each=24)
avgdist <- c(avgdf$sim6H,avgdf$simhourly,avgdf$sim4B,avgdf$sim5Q,avgdf$sim5S,avgdf$obs)

avgdf2 <- data.frame(avgdist,time2,type2)


