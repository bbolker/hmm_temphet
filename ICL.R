## ICL 

hmmnum <- function(s){
  return(s^2 + 2*s -1)
}

vs1 <- function(mod){
  vit <- viterbi(mod)
  vs1 <- sum(vit$state == 1)
  if(vs1 == 0){vs1 = 1/2}
  return(vs1)
}

ICL <- function(logL,ns,v1,v2,v3,v4,v5,v6,v7){
  icl <- LL - hmmnum(ns)/2 + log(gamma(nstates/2)) + log(gamma(vs1 + 1/2))
          + log(gamma(vs2 + 1/2)) + log(gamma(vs3 + 1/2))
          + log(gamma(vs4 + 1/2)) + log(gamma(vs5 + 1/2))
          + log(gamma(vs6 + 1/2)) + log(gamma(vs7 + 1/2))
          - ns*log(gamma(1/2)) - log(gamma(num_pts + ns/2))
  return(icl)
}