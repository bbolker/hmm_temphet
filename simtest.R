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

set.seed(unlist(strsplit(input_files,"[.]"))[1])

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