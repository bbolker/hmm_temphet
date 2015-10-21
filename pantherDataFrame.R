library(depmixS4)
library(dplyr)

dat <- read.csv('ArchivePantherData.csv')

block <- function(t){
  if(t %in% 7:16)return("block2")
  if(t %in% 17:20)return("block3")
  return("block1")
}

## creating suitable dataframe for depmixS4
dat <- (dat %>% transmute(
    cat = animal_id,
    Sex = Sex,
    Time = as.numeric(Time) - 1, ##as.numeric conversion
    Distance = Steplength.m. + 0.01, # log hack
    LogDist = log10(Distance),
    Block = block(Time)
  )
)

tempdat <- (dat %>% 
    group_by(cat) %>% 
      count(cat) %>% 
        filter(n>9000)
)

dat <- (dat %>% 
          filter(cat %in% tempdat$cat)
)

for(i in unique(dat$cat)){
  assign(paste("cat",i,sep=''),subset(dat,dat$cat==i))
}

### output the catdfs