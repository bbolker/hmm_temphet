library(depmixS4)
library(plyr)
library(dplyr)

t <- rep(0:23,500)
y <- rep(1,12000)

tempdat <- data.frame(y=y,t=t)

system.time(mod <- depmix(y~1
                          , data=tempdat
                          , transition=~cos((2*pi*t)/24)+ sin((2*pi*t)/24)
                          , nstate=2
                          , family=gaussian())
)
getpars(mod)

seed = unlist(strsplit(rtargetname,"[.]"))[2]
set.seed(as.numeric(seed))

randpars <- sample(-3:3,length(getpars(mod))-6,replace=TRUE)
newmod <- setpars(mod,c(0.5,0.5,randpars,0,1,2,1))
newmod
sim <- simhmm(newmod)
err <- rnorm(12000,0,sqrt(2)*log10(8))

df <- data.frame(obs= sim@response[[1]][[1]]@y + err,states=sim@states,time=t)
hist(df$obs)
table(df$states)

system.time(hmm2 <- depmix(obs~1
                           , data=df
                           , transition=~1
                           , nstate=2
                           , family=gaussian())
)

system.time(hmm2s <- fit(hmm2,verbose=TRUE))

system.time(hmm3 <- depmix(obs~1
                           , data=df
                           , transition=~1
                           , nstate=3
                           , family=gaussian())
)

system.time(hmm3s <- fit(hmm3,verbose=TRUE))

system.time(hmm4 <- depmix(obs~1
                           , data=df
                           , transition=~1
                           , nstate=4
                           , family=gaussian())
)

system.time(hmm4s <- fit(hmm4,verbose=TRUE))



system.time(hmm5 <- depmix(obs~1
                           , data=df
                           , transition=~1
                           , nstate=5
                           , family=gaussian())
)

system.time(hmm5s <- fit(hmm5,verbose=TRUE))

# system.time(hmmsin2 <- depmix(obs~1
#                            , data=df
#                            , transition=~cos((2*pi*t)/24)+ sin((2*pi*t)/24)
#                            , nstate=2
#                            , family=gaussian())
# )

# system.time(hmmsin2s <- fit(hmmsin2,verbose=FALSE))
# 
# system.time(hmmsin3 <- depmix(obs~1
#                            , data=df
#                            , transition=~cos((2*pi*t)/24)+ sin((2*pi*t)/24)
#                            , nstate=3
#                            , family=gaussian())
# )
# 
# system.time(hmmsin3s <- fit(hmmsin3,verbose=FALSE))
# 
# system.time(hmmsin4 <- depmix(obs~1
#                            , data=df
#                            , transition=~cos((2*pi*t)/24)+ sin((2*pi*t)/24)
#                            , nstate=4
#                            , family=gaussian())
# )
# 
# system.time(hmmsin4s <- fit(hmmsin4,verbose=FALSE))
# 
# 
# 
# system.time(hmmsin5 <- depmix(obs~1
#                           , data=df
#                           , transition=~1
#                           , nstate=5
#                           , family=gaussian())
# )
# 
# system.time(hmmsin5s <- fit(hmmsin5,verbose=FALSE))

v1 <- function(mod){
  vit <- viterbi(mod)
  vs <- sum(vit$state == 1)
  return(vs)
}

v2 <- function(mod){
  vit <- viterbi(mod)
  vs <- sum(vit$state == 2)
  return(vs)
}

v3 <- function(mod){
  vit <- viterbi(mod)
  vs <- sum(vit$state == 3)
  return(vs)
}

v4 <- function(mod){
  vit <- viterbi(mod)
  vs <- sum(vit$state == 4)
  return(vs)
}

v5 <- function(mod){
  vit <- viterbi(mod)
  vs <- sum(vit$state == 5)
  return(vs)
}
sumdf <- function(lst){
  LL <- ldply(lst,logLik)
  AIC <- ldply(lst,AIC)
  BIC <- ldply(lst,BIC)
  nstates <-ldply(lst,nstates)
  para <- ldply(lst,freepars)
  vs1 <- ldply(lst,v1)
  vs2 <- ldply(lst,v2)
  vs3 <- ldply(lst,v3)
  vs4 <- ldply(lst,v4)
  vs5 <- ldply(lst,v5)
  n <- ldply(lst,nobs)
  model <- c('HMM','HMM','HMM','HMM'
             # ,'HMMsin','HMMsin','HMMsin','HMMsin'
  )
  temp <- data.frame(LL=LL$V1
                     ,AIC=AIC$V1
                     ,BIC=BIC$V1
                     ,nstates=nstates$V1
                     ,parameters=para$V1
                     ,numobs = n$V1
                     ,vs1=vs1$V1
                     ,vs2=vs2$V1
                     ,vs3=vs3$V1
                     ,vs4=vs4$V1
                     ,vs5=vs5$V1
                     ,model
  )
  ICLmike <- function(logL,param,ns,v1,v2,v3,v4,v5,num_pts){
    icl <- (-2*logL -  2*(v1*log(max(v1,0.1)/num_pts) + v2*log(max(v2,0.1)/num_pts)
                          + v3*log(max(v3,0.1)/num_pts) + v4*log(max(v4,0.1)/num_pts)
                          + v5*log(max(v5,0.1)/num_pts))
            + 2*ns*log(num_pts) + (ns-1)*(ns+1)*log(num_pts)
    )
    return(icl)
  }
  ICL <- function(logL,param,ns,v1,v2,v3,v4,v5,num_pts){
    icl <- (-2*logL -  2*(v1*log(max(v1,0.1)/num_pts) + v2*log(max(v2,0.1)/num_pts)
                          + v3*log(max(v3,0.1)/num_pts) + v4*log(max(v4,0.1)/num_pts)
                          + v5*log(max(v5,0.1)/num_pts))
            + 2*ns*log(num_pts) + (ns-1)*log(num_pts)
    )
    return(icl)
  }
  temp2 <- (temp %>% rowwise()
            %>% transmute(LL=LL
                          ,AIC=AIC
                          ,BIC=BIC
                          ,nstates=nstates
                          ,parameters=parameters
                          ,ICL=ICL(LL,parameters,nstates,vs1,vs2,vs3,vs4,vs5,numobs)
                          ,ICLmike=ICLmike(LL,parameters,nstates,vs1,vs2,vs3,vs4,vs5,numobs)
            )
  )
  return(temp2)
}

fitlist <- list(hmm2s,hmm3s,hmm4s,hmm5s
                # ,hmmsin2s,hmmsin3s,hmmsin4s,hmmsin5s
)

dat <- sumdf(fitlist)
saveRDS(dat, file=paste("sim",seed,"RDS",sep="."))
