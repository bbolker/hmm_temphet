library(depmixS4)
library(dplyr)

# Fitting FMM function ------
fitmix <- function(state,cat,seed=2830){
  model <- mix(LogDist~1,
               data=cat,
               prior=~1,
               nstate=state,
               family=gaussian(),
               initdata=cat)
  set.seed(seed)
  fitmodel <- fit(model)
  return(fitmodel)
}

cat14 <- dat %>% filter(cat==14)

# Fitting FMM to cat14 ------
fitfmm3s <- fitmix(3,cat14,seed=64)
fitfmm4s <- fitmix(4,cat14,seed=4)
fitfmm5s <- fitmix(5,cat14,seed=54)
fitfmm6s <- fitmix(6,cat14,seed=16)

save(fitfmm3s,fitfmm4s,fitfmm5s,fitfmm6s,file="fmmcat14.RData")

cat15 <- dat %>% filter(cat==15)

# Fitting FMM to cat15 ------
fitfmm3s <- fitmix(3,cat15,seed=3)
fitfmm4s <- fitmix(4,cat15,seed=1)
fitfmm5s <- fitmix(5,cat15,seed=1)
fitfmm6s <- fitmix(6,cat15,seed=1)

save(fitfmm3s,fitfmm4s,fitfmm5s,fitfmm6s,file="fmmcat15.RData")

cat1 <- dat %>% filter(cat==1)

# Fitting FMM to cat1 ------
fitfmm3s <- fitmix(3,cat1,seed=43)
fitfmm4s <- fitmix(4,cat1,seed=9)
fitfmm5s <- fitmix(5,cat1,seed=3)
fitfmm6s <- fitmix(6,cat1,seed=1)

save(fitfmm3s,fitfmm4s,fitfmm5s,fitfmm6s,file="fmmcat1.RData")

# Fitting FMM to cat2 ------

cat2 <- dat %>% filter(cat==2)

fitfmm3s <- fitmix(3,cat2,seed=562)
fitfmm4s <- fitmix(4,cat2,seed=265)
fitfmm5s <- fitmix(5,cat2,seed=11)
fitfmm6s <- fitmix(6,cat2,seed=48)

save(fitfmm3s,fitfmm4s,fitfmm5s,fitfmm6s,file="fmmcat2.RData")

