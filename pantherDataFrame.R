library(depmixS4)
library(dplyr)


dat <- read.csv('ArchivePantherData.csv')

block <- function(t){
  if(t %in% 7:16)return("block2")
  if(t %in% 17:20)return("block3")
  return("block1")
}

## creating suitable dataframe for depmixS4
dat <- (dat %>% rowwise() %>% transmute(
    cat = animal_id,
    Sex = Sex,
    Time = as.numeric(Time) - 1, ##as.numeric conversion
    Distance = Steplength.m.,
    LogDist = log10(Distance),
    Block = block(Time)
  )
)

dat$LogDist[is.infinite(dat$LogDist)] <- NA

cats <- c(1,2,14,15)


dat <- (dat %>% 
          filter(cat %in% cats)
)

save(dat, file = "allcats.RData")

for(catnum in cats){
    cat <- subset(dat,dat$cat == catnum)
    save(cat, file = paste0("cat",catnum,".RData",sep=""))
}






    
### output the catdfs
