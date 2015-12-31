default = 1
num <- nrow(cat)
iter <- 500

if(default==0){
    num <- 500
    iter <- 30
}

cat <- head(cat,num)
