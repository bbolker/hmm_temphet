library(depmixS4)

t <- rep(0:23,5000)
y <- rep(1,120000)

tempdat <- data.frame(y=y,t=t)

system.time(mod <- depmix(y~1
  , data=tempdat
  , transition=~cos((2*pi*t)/24)+ sin((2*pi*t)/24)
  , nstate=2
  , family=gaussian())
)
getpars(mod)
set.seed(108)
randpars <- sample(1:8,length(getpars(mod))-6,replace=TRUE)
newmod <- setpars(mod,c(0.5,0.5,randpars,0,2,6,2))
newmod
sim <- simhmm(newmod)
df <- data.frame(obs= sim@response[[1]][[1]]@y,states=sim@states,time=t)
hist(df$obs)

system.time(simmod1 <- depmix(obs~1
  , data=head(df,5000)
  , transition=~1
  , nstate=2
  , family=gaussian())
)

system.time(fit1 <- fit(simmod1))

system.time(simmod2 <- depmix(obs~1
  , data=head(df,5000)
  , transition=~1
  , nstate=3
  , family=gaussian())
)

system.time(fit2 <- fit(simmod2))

system.time(simmod3 <- depmix(obs~1
  , data=head(df,5000)
  , transition=~1
  , nstate=4
  , family=gaussian())
)

system.time(fit3 <- fit(simmod3))



system.time(simmod4 <- depmix(obs~1
  , data=head(df,5000)
  , transition=~1
  , nstate=5
  , family=gaussian())
)

system.time(fit4 <- fit(simmod4))

print(BIC(fit1))
print(BIC(fit2))
print(BIC(fit3))
print(BIC(fit4))