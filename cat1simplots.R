## creating simdf 
library(reshape2)

print(bicplot(sumdat))
print(avgplot(tempdf))
print(acfplot(tempdf))
print(msd56(fitsin5s,fithomo6s))
print(avgplot(vitcat(cat,fithomo3s)))
print(acfplot(vitcat(cat,fithomo3s))+ggtitle("Viterbi vs Observed ACF"))
