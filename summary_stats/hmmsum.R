library(dplyr)
library(ggplot2)
library(reshape2)
library(plyr)

sumdf <- data.frame()
for(i in mods){
  temp <- i
  sumdf <- rbind(sumdf,temp[[1]])
}

sumdf <- (sumdf
          %>% mutate(deltaBIC = BICS-min(BICS)
                     , kBIC = BICS/(-2))
)

multimoddf <- do.call(rbind,multimods)


multimoddf <- (multimoddf 
               %>% group_by(model) 
               %>% dplyr:::mutate(deltaBIC = BICS-min(BICS))
)

(adj_BIC_plot <- ggplot(sumdf, aes(x=nstates,y=deltaBIC,colour=model))+
  facet_wrap(~ type,ncol=4)+
  scale_size_continuous( name="# of parameters") +
  scale_x_continuous(expand=c(0,0.5),breaks=2:7)+
  geom_point(aes(size=parameters))+
  scale_colour_brewer(palette="Dark2")+
  labs(y=expression(paste(Delta, "BIC")),
       x="Number of States") +
  geom_line())



###

simdf <- simdat %>% dplyr::select(-obs) %>%
  group_by(time) %>% summarise_each(funs(mean))
obsdf <- simdat %>% dplyr::select(c(obs,time)) %>%
  filter(!is.na(obs)) %>% group_by(time) %>%
  summarise_each(funs(mean))

avgdf2 <- melt(simdf,'time')
ggplot(avgdf2,aes(x=time,y=value,colour=variable)) + 
  geom_point() +geom_line()+ xlab("Time of Day") +
  ylab("Step lengths (log10)")+
  scale_x_continuous(breaks=0:23)+
  geom_line(data=obsdf,
            aes(y=obs),lwd=2.5,alpha=0.3,colour="black")

acffun <- function(sim){
  a <- acf(sim,na.action=na.pass,plot=FALSE)
  df <- data.frame(ACF=c(a$acf),lag=a$lag)
  return(df)
}

temp <- simdat %>% dplyr::select(-time)%>%ldply(.,acffun)
ggplot(temp,aes(lag,ACF,colour=.id))+
  geom_point()+geom_line()

alldf <- list(HMMsumdf = sumdf
              , multimoddf = multimoddf
              , avgplotdf = avgdf2
              , acfdf = temp
              , msdlist = msd)

#saveRDS(alldf, file="cat.1.df.RDS")


