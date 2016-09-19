library(ggplot2)
library(reshape2)
library(dplyr)
# 
# simfiles <- list.files(pattern="RDS")

# group_dat <- data.frame()
# for(i in simfiles){
#   seednum <-unlist(strsplit(i,split="[.]"))[2]
#   tempdat <- readRDS(i)
#   group_df <- tempdat %>% mutate(seed=seednum)
#   group_dat <- rbind(group_dat,group_df)
# }

group_dat <- readRDS("hmmsims.RDS")

BICdat <- group_dat %>% group_by(seed) %>% slice(which.min(BIC))

ICLdat <- group_dat %>% group_by(seed) %>% slice(which.min(ICL))

ICLmikedat <- group_dat %>% group_by(seed) %>% slice(which.min(ICLmike))

AICdat <- group_dat %>% group_by(seed) %>% slice(which.min(AIC))

count_df <- data.frame(IC = rep(c("AIC","BIC","ICL","ICLmike"),each=100)
                       , optstates = c(AICdat$nstates,BICdat$nstates,ICLdat$nstates,ICLmikedat$nstates)
) %>% count(optstates,IC)

g1 <- (ggplot(count_df,aes(x=optstates,y=n,group=IC, color=IC))
       + geom_point()
       + geom_line()
       + theme_bw()
       # + facet_grid(.~IC)
       + ylab("Counts")
       + xlab("Number of States")
       + ggtitle("Simulation Results: Fitting HMMs to Two-State temphet HMM")
) 

g1

