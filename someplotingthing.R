source('plotfuns.R')

sumdf130 <- sumdf(fit130)

cat130simdf <- data.frame(cat130simdf,tempbest,tempdat2)

bicplot(sumdf130) + labs(x="Number of States") + scale_colour_discrete()
bicplot2(sumdf130,'HMM','HMM + TH') + labs(x="Number of States") + scale_colour_discrete() +facet_wrap(~ type,ncol=2)
bicplot1(sumdf130,'FMM') + labs(x="Number of States") + 
  scale_colour_discrete() +facet_wrap(~ type,ncol=2)


head(sumdf130)

bicplot(sumdf131) + labs(x = "Number of States")
bicplot(sumdf94) + labs(x = "Number of States")
bicplot(sumdf48) + labs(x = "Number of States")

acfplot(cat130simdf%>%select(c(time,obs,vitobs)))

avgplot((cat130simdf%>%select(c(time,obs,vitobs))))

temp130 <- cat130simdf %>% mutate(resvit=obs-vitobs,
                                  resmean=obs-mean,
                                  resvitfmm=obs-vitobs.1,
                                  vitobsFMM=vitobs.1)

acfplot(temp130%>%select(c(time,obs,resvit,resmean,resvitfmm)))


acfplot(cat130simdf%>%select(c(time,FMM3,FMMsin6,HMM6,HMMsin5,obs)))
acfplot(cat130simdf%>%select(c(time,FMM3,obs)))
acfplot(cat130simdf%>%select(c(time,FMMsin4,obs)))
acfplot(cat130simdf%>%select(c(time,HMM6,obs)))
acfplot(cat130simdf%>%select(c(time,HMMsin5,obs)))


avgplot(cat130simdf%>%select(c(time,FMM3,FMMsin6,HMM6,HMMsin5,obs)))+labs(y=expression("log Step Length"),x=expression("Time of Day"))+ggtitle("Average Step Length by Time of Day")
avgplot(cat130simdf%>%select(c(time,HMMsin5,obs,HMMhourly4)))+labs(y=expression("log Step Length"),x=expression("Time of Day"))

##vit acf and avg
avgplot(temp130%>%select(c(time,vitobsFMM,obs)))+labs(y=expression(paste("log" [10], " Step Length")),x=expression("Time of Day"))
acfplot(temp130%>%select(c(time,vitobsFMM,obs))) + labs(x='Hour Lag')

head(tempdf2) 


###avg/acf
avgplot(tempdf130%>%select(c(time,FMM3,FMMsin6,HMM6,HMMsin5,obs)))+labs(y=expression(paste("Step Length log" [10], " meters") ),x=expression("Time of Day"))

acfplot(tempdf130%>%select(c(time,FMM3,FMMsin6,HMM6,HMMsin5,obs))) + labs(x='Hour Lag')



tempdf130 <- data.frame(cat130simdf, new=sim130sin5$obs) %>% mutate(HMMsin5=sim130sin5$obs)
tempdf131 <- data.frame(cat131simdf,new=sim131sin5$obs)  %>% mutate(HMMsin5=sim131sin5$obs)
tempdf48 <- data.frame(cat48simdf,new=sim48sin4$obs)  %>% mutate(HMMsin4=sim48sin4$obs)
tempdf94 <- data.frame(cat94simdf,new=sim94sin5$obs)  %>% mutate(HMMsin5=sim94sin5$obs)

avgplot(tempdf131%>%select(c(time,FMM4,FMMsin5,HMM5,HMMsin5,obs)))+labs(y=expression(paste("Step Length log" [10], " meters") ),x=expression("Time of Day"))

acfplot(tempdf131%>%select(c(time,FMM4,FMMsin5,HMM5,HMMsin5,obs))) + labs(x='Hour Lag')

avgplot(tempdf48%>%select(c(time,FMM4,FMMsin4,HMM5,HMMsin4,obs)))+labs(y=expression(paste("Step Length log" [10], " meters") ),x=expression("Time of Day"))

acfplot(tempdf48%>%select(c(time,FMM4,FMMsin4,HMM5,HMMsin4,obs))) + labs(x='Hour Lag')

avgplot(tempdf94%>%select(c(time,FMM3,FMMsin4,HMM6,HMMsin5,obs)))+labs(y=expression(paste("Step Length log" [10], " meters") ),x=expression("Time of Day"))

acfplot(tempdf94%>%select(c(time,FMM3,FMMsin4,HMM6,HMMsin5,obs))) + labs(x='Hour Lag')





avgplot(tempdf3%>%select(c(time,obs,new)))

acfplot(cat131simdf%>%select(c(time,FMM4,FMMsin5,HMM5,HMMsin4,obs)))+ labs(y=expression("log Step Length"),x=expression("Time of Day"))
acfplot(cat131simdf%>%select(c(time,FMM4,obs)))
acfplot(cat131simdf%>%select(c(time,FMMsin5,obs)))
acfplot(cat131simdf%>%select(c(time,HMM5,obs)))
acfplot(cat131simdf%>%select(c(time,HMMsin4,obs)))

acfplot(testdf)
avgplot(testdf%>%select(time,obs,sim2))

fun <- function(x){
  return(3*x^2 + 3*x)
}

fun(3)
fun(4)
fun(5)
fun(6)


model <- depmix(H~1,
                data=cat130simdf,
                nstate=3,
                transition=~cos(2*pi*time/24)+ sin(2*pi*time/24),
                family=gaussian())

fitmod <- fit(model)

ps <- rep(1,12000)
Hour <- rep(c(1:24),500)
newtime <- Hour/24
df <- data.frame(ps,Hour,newtime)


model2 <- depmix(ps~1,
                 data=df,
                 nstate=5,
                 transition=~cos((2*pi*Hour)/24)+ sin((2*pi*Hour)/24),
                 family=gaussian())
model3<-setpars(model2,getpars(fitsin5))


simdata <- simFun(model3,df)

simsin5 <- depmix(ps~1,
                  data=df,
                  nstate=5,
                  family=gaussian(),
                  transition=~cos(2*pi*Hour/24)+ sin(2*pi*Hour/24))
simsin5<-setpars(simsin5,getpars(fitsin5))
set.seed(2830)
simsin5 <- simFun(simsin5,df)


df <- data.frame(obs= sim@response[[1]][[1]]@y,states=sim@states)

new130 <- data.frame(cat130simdf,hmm2sin5=)



ps <- rep(1,24000)
Hour <- rep(c(15:24,1:14),1000)
newtime <- Hour/24
df <- data.frame(ps,Hour,newtime)

simsin5 <- depmix(ps~1,
                  data=df,
                  nstate=5,
                  family=gaussian(),
                  transition=~cos(2*pi*Hour/24)+ sin(2*pi*Hour/24))
simsin5<-setpars(simsin5,getpars(fit130sin5))
set.seed(2830)
simsin5 <- simFun(simsin5,df)

temp <- head(simsin5$obs,10286)

tempdf2 <- data.frame(tempdf,tsin5=temp)
save(list=c('tempdf2'),file='temp2.RData')
tempdat <- simFun(temp,cat130)