library(ggplot2)
library(reshape2)
library(dplyr)

dat <- readRDS("simtest.RDS")

findS <- function(Smin,S2,S3,S4,S5){
  if(Smin==S2){return(2)}
  if(Smin==S3){return(3)}
  if(Smin==S4){return(4)}
  if(Smin==S5){return(5)}
}

datABICS <- (dat 
  %>% rowwise()
  %>% mutate(minBIC = min(twoS_BIC,threeS_BIC,fourS_BIC,fiveS_BIC)
             , minAIC = min(twoS_AIC,threeS_AIC,fourS_AIC,fiveS_AIC))
  %>% mutate(minBICStates = findS(minBIC,twoS_BIC,threeS_BIC,fourS_BIC,fiveS_BIC)
             , minAICStates = findS(minAIC,twoS_AIC,threeS_AIC,fourS_AIC,fiveS_AIC))
  %>% select(c(seed,minAICStates,minBICStates))
)

print(table(datABICS$minAICStates))
print(table(datABICS$minBICStates))

AICdat <- datABICS %>% count(minAICStates) %>% transmute(States=minAICStates,n=n,type="AIC")
BICdat <- datABICS %>% count(minBICStates) %>% transmute(States=minBICStates,n=n,type="BIC")

ICdat <- rbind(AICdat,BICdat)

g1 <- (ggplot(ICdat,aes(x=States,y=n,color=type,group=type))
       + geom_point()
       + geom_line()
       + theme_bw()
       + ylab("Counts")
       + xlab("Number of States")
       + ggtitle("Simulation Results: Fitting HMMs to Two-State temphet HMM")
) 
