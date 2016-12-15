library(ggplot2)
library(reshape2)
library(dplyr)

simfiles <- list.files(pattern="RDS")

BICdat <- data.frame()
for(i in simfiles){
  seednum <-unlist(strsplit(i,split="[.]"))[2]
  tempdat <- readRDS(i)
  BICdf <- tempdat %>% mutate(seed=seednum) %>% group_by(model) %>% slice(which.min(BIC))
  BICdat <- rbind(BICdat,BICdf)
}

AICdat <- data.frame()
for(i in simfiles){
  seednum <-unlist(strsplit(i,split="[.]"))[2]
  tempdat <- readRDS(i)
  AICdf <- tempdat %>% mutate(seed=seednum) %>% group_by(model) %>% slice(which.min(AIC))
  AICdat <- rbind(AICdat,AICdf)
}

AICdat2 <- AICdat %>% ungroup() %>% select(-c(LL,AIC,BIC,parameters)) %>% group_by(model) %>% count(nstates,model) %>% mutate(IC = "AIC")
BICdat2 <- BICdat %>% ungroup() %>% select(-c(LL,AIC,BIC,parameters)) %>% group_by(model) %>% count(nstates,model) %>% mutate(IC = "BIC")

ICdat <- rbind(AICdat2,BICdat2)
saveRDS(ICdat,"ICdat.rds")

g1 <- (ggplot(ICdat,aes(x=nstates,y=n,color=model,group=model))
       + geom_point()
       + geom_line()
       + theme_bw()
       + facet_grid(.~IC)
       + ylab("Counts")
       + xlab("Number of States")
       + ggtitle("Simulation Results: Fitting HMMs to Two-State temphet HMM")
) 

g1
# 
# #### fit and sim cat1sinhmm5 
# # system("make catsdat.Rout")
# # system("make cat1.df.Rout")
# # load(".cat1.df.RData")
# # source("fitfunctions.R")
# # source("mikesim.R")
# # source("simfunctions.R")
# # fitsin5s <- fitsin(5,cat,seed=2830)
# # simsin5 <- simsin(5,cat,fitsin5s)
# # 
# # dwelldf <- data.frame(hmmsin5obs=simsin5$obs,hmmsin5states=simsin5$states
# #                       ,time = cat$Time, count=1)
# 
# load("cat1sinhmm5.RData")
# 
# for(i in 1:10283){ # hacks, not bothering with the last "group"
#   j <- 1
#   while(j>0){
#     if(dwelldf[i,2] == dwelldf[i+j,2]){
#       dwelldf[i,4] <- dwelldf[i,4]+1
#       j = j+1}
#     if(dwelldf[i,2] != dwelldf[i+j,2]){
#       j = -1}
#     }
# }
# 
# dwelldf2 <- (dwelldf
#   %>% transmute(States = hmmsin5states
#                 , time=time
#                 , count=count)
#   %>% group_by(States,time)
#   %>% summarise(averageDT=mean(count))
# )
# 
# g2 <- (ggplot(dwelldf2,aes(x=time,y=averageDT,group=States,color=factor(States)))
#   + geom_point()
#   + geom_line()
#   + theme_bw()
# )
# 
# summary(fitsin5s)
