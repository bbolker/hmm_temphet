library(depmixS4)

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

seed = unlist(strsplit(input_files,"[.]"))[1]
set.seed(seed)

randpars <- sample(1:4,length(getpars(mod))-6,replace=TRUE)
newmod <- setpars(mod,c(0.5,0.5,randpars,0,2,4,2))
newmod
sim <- simhmm(newmod)
df <- data.frame(obs= sim@response[[1]][[1]]@y,states=sim@states,time=t)
hist(df$obs)

system.time(simmod1 <- depmix(obs~1
  , data=df
  , transition=~1
  , nstate=2
  , family=gaussian())
)

system.time(fit1 <- fit(simmod1,verbose=FALSE))

system.time(simmod2 <- depmix(obs~1
  , data=df
  , transition=~1
  , nstate=3
  , family=gaussian())
)

system.time(fit2 <- fit(simmod2,verbose=FALSE))

system.time(simmod3 <- depmix(obs~1
  , data=df
  , transition=~1
  , nstate=4
  , family=gaussian())
)

system.time(fit3 <- fit(simmod3,verbose=FALSE))



system.time(simmod4 <- depmix(obs~1
  , data=df
  , transition=~1
  , nstate=5
  , family=gaussian())
)

system.time(fit4 <- fit(simmod4,verbose=FALSE))

print(BIC(fit1))
print(BIC(fit2))
print(BIC(fit3))
print(BIC(fit4))

print(summary(fit1))
print(summary(fit2))
print(summary(fit3))
print(summary(fit4))

dat <- data.frame(seed=seed
                  , twoS_LL = logLik(fit1)
                  , twoS_AIC = AIC(fit1)
                  , twoS_BIC = BIC(fit1)
                  , twoS_mu1 = getpars(fit1)[7]
                  , twoS_mu2 = getpars(fit1)[9]
                  , twoS_sd1 = getpars(fit1)[8]
                  , twoS_sd2 = getpars(fit1)[10]
                  , threeS_LL = logLik(fit2)
                  , threeS_AIC = AIC(fit2)
                  , threeS_BIC = BIC(fit2)
                  , threeS_mu1 = getpars(fit2)[13]
                  , threeS_mu2 = getpars(fit2)[15]
                  , threeS_mu3 = getpars(fit2)[17]
                  , threeS_sd1 = getpars(fit2)[14]
                  , threeS_sd2 = getpars(fit2)[16]
                  , threeS_sd3 = getpars(fit2)[18]
                  , fourS_LL = logLik(fit3)
                  , fourS_AIC = AIC(fit3)
                  , fourS_BIC = BIC(fit3)
                  , fourS_mu1 = getpars(fit3)[21]
                  , fourS_mu2 = getpars(fit3)[23]
                  , fourS_mu3 = getpars(fit3)[25]
                  , fourS_mu4 = getpars(fit3)[27]
                  , fourS_sd1 = getpars(fit3)[22]
                  , fourS_sd2 = getpars(fit3)[24]
                  , fourS_sd3 = getpars(fit3)[26]
                  , fourS_sd4 = getpars(fit3)[28]
                  , fiveS_LL = logLik(fit4)
                  , fiveS_AIC = AIC(fit4)
                  , fiveS_BIC = BIC(fit4)
                  , fiveS_mu1 = getpars(fit4)[31]
                  , fiveS_mu2 = getpars(fit4)[33]
                  , fiveS_mu3 = getpars(fit4)[35]
                  , fiveS_mu4 = getpars(fit4)[37]
                  , fiveS_mu5 = getpars(fit4)[39]
                  , fiveS_sd1 = getpars(fit4)[32]
                  , fiveS_sd2 = getpars(fit4)[34]
                  , fiveS_sd3 = getpars(fit4)[36]
                  , fiveS_sd4 = getpars(fit4)[38]
                  , fiveS_sd5 = getpars(fit4)[40]
                  )

rownames(dat) <- NULL
saveRDS(dat, file=paste("sim",seed,"RDS",sep="."))

# rdsave(seed)