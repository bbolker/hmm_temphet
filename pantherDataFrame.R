library(depmixS4)
library(dplyr)

dat <- read.csv('ArchivePantherData.csv')

## creating suitable dataframe for depmixS4
dat <- (dat %>% transmute(
    cat = animal_id,
    Sex = Sex,
    Time = as.numeric(Time) - 1, ##as.numeric conversion
    Distance = Steplength.m. + 0.01, # log hack
    LogDist = log10(Distance)
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


