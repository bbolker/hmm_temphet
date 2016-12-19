## final plots

library("ggplot2"); theme_set(theme_bw())
scale_colour_discrete <- function(...,palette="Set1")
  scale_colour_brewer(...,palette=palette)
scale_fill_discrete <- function(...,palette="Set1")
  scale_fill_brewer(...,palette=palette)
zmargin <- theme(panel.margin=grid::unit(0,"lines"))
library("knitr")
opts_chunk$set(fig.width=6,fig.height=4)
## fix complaints about "incompatible color definition" from xcolor ...
## http://tex.stackexchange.com/questions/148188/knitr-xcolor-incompatible-color-definition
knit_hooks$set(document = function(x) {sub('\\usepackage[]{color}', '\\usepackage{xcolor}', x, fixed = TRUE)})
library("depmixS4")
library("plyr")
library("dplyr")
library("reshape2")
library("gridExtra")
library("RColorBrewer")
if (!require("ggalt") || length(find("geom_encircle"))==0) {
  #     cat("installing ggalt from BB repo ...\n")
  try(detach("package:ggalt"),silent=TRUE)
  devtools::install_github("hrbrmstr/ggalt")
  library("ggalt")
}


ICdat <- readRDS("./simsin_results/ICdat.rds")
ICdat <- ICdat %>% filter(IC == "BIC")
ICdat <- ICdat %>% rowwise() %>% mutate(model=factor(model,
                                                     levels=c("HMM","HMMsin"),
                                                     labels=c("Homogeneous","Heterogeneous")))
simplot <- ggplot(ICdat,aes(x=nstates,y=n,color=model,shape=model))+
  geom_line() +
  geom_point(size=3,bg="white") +
  scale_shape_manual(values=c(21,16))+
  scale_colour_brewer(palette="Set1")+
  labs(y="Counts",x="BIC-optimal number of states")+
  ggtitle("2-state time-dependent transitions without GPS Error")
print(simplot)



ICdat <- readRDS("./sim_results/ICdat.rds")
ICdat <- ICdat %>% filter(IC == "BIC")
ICdat <- ICdat %>% rowwise() %>% mutate(model=factor(model,
                             levels=c("HMM","HMMsin"),
                             labels=c("Homogeneous","Heterogeneous")))
simplot <- ggplot(ICdat,aes(x=nstates,y=n,color=model,shape=model))+
  geom_line() +
  geom_point(size=3,bg="white") +
  scale_shape_manual(values=c(21,16))+
  scale_colour_brewer(palette="Set1")+
  labs(y="Counts",x="BIC-optimal number of states")+
  ggtitle("2-state HMM with GPS error")
print(simplot)


ICtimedat <- readRDS("./simtime_results/ICdat.rds")
ICtimedat <- ICtimedat %>% filter(IC == "BIC") %>% ungroup() %>% rowwise()
ICtimedat <- mutate(ICtimedat,
                model=factor(model,
                             levels=c("HMM","HMMsin"),
                             labels=c("Homogeneous","Heterogeneous"))) %>% filter(IC=="BIC")
simtimeplot <- ggplot(ICtimedat,aes(x=nstates,y=n,color=model,shape=model))+
  geom_line() +
  geom_point(size=3,bg="white") +
  scale_shape_manual(values=c(21,16))+
  scale_colour_brewer(palette="Set1")+
  labs(y="Counts",x="BIC-optimal number of states")+
  ggtitle("2-state HMM time-dependent transitions with GPS error")
print(simtimeplot)

####

allmodsumdf2 <- readRDS("./summary_stats/cat.1.df.RDS")$multimoddf
df2 <- transform(allmodsumdf2, #change allmodsumdf to allmodsumdf2
                 distrib=ifelse(grepl("W",model),"Weibull","log-Normal"),
                 turn_angle=ifelse(grepl("VM",as.character(model)),"yes","no"),
                 homog=ifelse(grepl("TH",type),"Time-heterogeneous","Homogeneous"))
slname <- "Step-length\ndistribution"
(ggplot(df2, aes(x=nstates,y=deltaBIC,size=distrib,
                 colour=distrib,lty=turn_angle))
+ geom_line()
+ facet_wrap(~ homog)
+ scale_colour_brewer(name=slname,palette="Set1")
+ scale_size_manual(name=slname,values=c(1.75,0.75))
## scale_size_continuous( name="# parameters") +
#    geom_point(aes(size=parameters))+
+ scale_linetype(name="von Mises\nturning angle?")
+ labs(y=expression(Delta*"BIC"),
       x="Number of States")
+ geom_hline(yintercept=0,colour="gray")
+ ggtitle("Cat 1 HMM comparisons")
+ zmargin)


linetypev <- c(rep(2,2),rep(1,5))
sumdat <- transform(readRDS(input_files[1])[[1]],
                    mod = ifelse(type=="HMM + TH","HMM + TH","HMM")
)
linetypev <- c(rep(2,2),rep(1,5))
(adj_BIC_plot <- ggplot(sumdat, aes(x=nstates
                                    , y=deltaBIC
                                    , colour=model
                                    , linetype=model
                                    , group=model))
+ facet_wrap(~mod,ncol = 2,scales = "free_x")
+ scale_size_continuous(name="# of states")
# + scale_x_continuous(breaks=seq(0,300,by=30))
+ geom_point() #aes(shape=factor(nstates),alpha=nstates))
+ scale_linetype_manual(values=linetypev, name="Model")
+ scale_colour_brewer(palette="Dark2", name="Model")
# + scale_shape_discrete(name="Number of states")
# + scale_alpha_continuous(name="Number of states")
+ labs(y=expression(paste(Delta, "BIC"))
       , x="Number of parameters") 
+ geom_line()
+ ggtitle("Cat 1 BIC comparisons")
+ zmargin)

cat1simdf <- readRDS(input_files[1])[[2]]
cat1avgdf <- avgdf(cat1simdf)

mnames <- c("hmm","hmmblock",
            "hmmsin","hmmquad","obs","vithmm")
mnames2 <- c("LNVMHMM-homogeneous"
             , "LNVMHMM-block"
             , "LNVMHMM-sin"
             , "LNVMHMM-quadratic"
             , "observed"
             , "viterbi homogenous (n=3)")

combdf <- (cat1avgdf
           %>% transmute(time=time,model=variable,steplen=value)
           %>% filter(model %in% mnames)
           %>% mutate(model=factor(model,levels=mnames,labels=mnames2))
)
linetypev <- c(rep(c(2,1),c(1,4))
               ,0
)
colvec <- c(c(brewer.pal(4,"Dark2"),"black")
            ,"black"
)
lwdvec <- c(rep(c(1,4),c(4,1))
            ,4
)
alphavec <- c(rep(c(1,0.2),c(4,1))
              ,1
)

shapevec <- c(rep(32,5),118)

(avg_plot <- ggplot(combdf,aes(x=time,y=steplen,colour=model,
                               lwd=model,
                               alpha=model,linetype=model, shape=model))
+ geom_point()
+ geom_line()
+ xlab("Time of Day") 
+ scale_shape_manual(values=shapevec, name="Model")
+ ylab(expression('Mean step length'~(log[10]*m)))
+ scale_colour_manual(values=colvec, name="Model")
+ scale_alpha_manual(values=alphavec, name="Model")
+ scale_size_manual(values=lwdvec, name="Model")
+ scale_linetype_manual(values=linetypev, name="Model")
+ ggtitle("Cat 1 Average step-length by time")
) 
##########

cat1acfdf <- cat1simdf %>% dplyr:::select(-time)%>%ldply(.,acffun)

combacfdf <- (cat1acfdf
              %>% transmute(lag=lag,model=.id,ACF=ACF)
              %>% filter(model %in% mnames)
              %>% mutate(model=factor(model,levels=mnames,labels=mnames2))
)


(acf_plot <- ggplot(combacfdf,aes(x=lag,y=ACF,colour=model,
                                  lwd=model,alpha=model,linetype=model,shape=model))
+ geom_point()
+ geom_line()
+ xlab("Lag") 
+ ylab("Autocorrelation")
+ scale_shape_manual(values=shapevec, name="Model")
+ scale_colour_manual(values=colvec, name="Model")
+ scale_alpha_manual(values=alphavec, name="Model")
+ scale_size_manual(values=lwdvec, name="Model")
+ scale_linetype_manual(values=linetypev, name="Model")
+ ggtitle("Cat 1 ACF")
)


##############
allmodsumdf2 <- readRDS("./summary_stats/cat.2.df.RDS")$multimoddf
df2 <- transform(allmodsumdf2, #change allmodsumdf to allmodsumdf2
                 distrib=ifelse(grepl("W",model),"Weibull","log-Normal"),
                 turn_angle=ifelse(grepl("VM",as.character(model)),"yes","no"),
                 homog=ifelse(grepl("TH",type),"Time-heterogeneous","Homogeneous"))
slname <- "Step-length\ndistribution"
(ggplot(df2, aes(x=nstates,y=deltaBIC,size=distrib,
                 colour=distrib,lty=turn_angle))
+ geom_line()
+ facet_wrap(~ homog)
+ scale_colour_brewer(name=slname,palette="Set1")
+ scale_size_manual(name=slname,values=c(1.75,0.75))
## scale_size_continuous( name="# parameters") +
#    geom_point(aes(size=parameters))+
+ scale_linetype(name="von Mises\nturning angle?")
+ labs(y=expression(Delta*"BIC"),
       x="Number of States")
+ geom_hline(yintercept=0,colour="gray")
+ ggtitle("Cat 2 HMM comparisons")
+ zmargin)


linetypev <- c(rep(2,2),rep(1,5))
sumdat <- transform(readRDS(input_files[2])[[1]],
                    mod = ifelse(type=="HMM + TH","HMM + TH","HMM")
)
linetypev <- c(rep(2,2),rep(1,5))
(adj_BIC_plot <- ggplot(sumdat, aes(x=nstates
                                    , y=deltaBIC
                                    , colour=model
                                    , linetype=model
                                    , group=model))
+ facet_wrap(~mod,ncol = 2,scales = "free_x")
+ scale_size_continuous(name="# of states")
# + scale_x_continuous(breaks=seq(0,300,by=30))
+ geom_point() #aes(shape=factor(nstates),alpha=nstates))
+ scale_linetype_manual(values=linetypev, name="Model")
+ scale_colour_brewer(palette="Dark2", name="Model")
# + scale_shape_discrete(name="Number of states")
# + scale_alpha_continuous(name="Number of states")
+ labs(y=expression(paste(Delta, "BIC"))
       , x="Number of parameters") 
+ geom_line()
+ ggtitle("Cat 2 BIC comparisons")
+ zmargin)

cat2simdf <- readRDS(input_files[2])[[2]]
cat2avgdf <- avgdf(cat2simdf)

mnames <- c("hmm","hmmblock",
            "hmmsin","hmmquad","obs","vithmm")
mnames2 <- c("LNVMHMM-homogeneous"
             , "LNVMHMM-block"
             , "LNVMHMM-sin"
             , "LNVMHMM-quadratic"
             , "observed"
             , "viterbi homogenous (n=3)")

combdf <- (cat2avgdf
           %>% transmute(time=time,model=variable,steplen=value)
           %>% filter(model %in% mnames)
           %>% mutate(model=factor(model,levels=mnames,labels=mnames2))
)
linetypev <- c(rep(c(2,1),c(1,4))
               ,0
)
colvec <- c(c(brewer.pal(4,"Dark2"),"black")
            ,"black"
)
lwdvec <- c(rep(c(1,4),c(4,1))
            ,4
)
alphavec <- c(rep(c(1,0.2),c(4,1))
              ,1
)

shapevec <- c(rep(32,5),118)

(avg_plot <- ggplot(combdf,aes(x=time,y=steplen,colour=model,
                               lwd=model,
                               alpha=model,linetype=model, shape=model))
+ geom_point()
+ geom_line()
+ xlab("Time of Day") 
+ scale_shape_manual(values=shapevec, name="Model")
+ ylab(expression('Mean step length'~(log[10]*m)))
+ scale_colour_manual(values=colvec, name="Model")
+ scale_alpha_manual(values=alphavec, name="Model")
+ scale_size_manual(values=lwdvec, name="Model")
+ scale_linetype_manual(values=linetypev, name="Model")
+ ggtitle("Cat 2 Average step-length by time")
) 
###

cat2acfdf <- cat2simdf %>% dplyr:::select(-time)%>%ldply(.,acffun)

combacfdf <- (cat2acfdf
              %>% transmute(lag=lag,model=.id,ACF=ACF)
              %>% filter(model %in% mnames)
              %>% mutate(model=factor(model,levels=mnames,labels=mnames2))
)


(acf_plot <- ggplot(combacfdf,aes(x=lag,y=ACF,colour=model,
                                  lwd=model,alpha=model,linetype=model,shape=model))
+ geom_point()
+ geom_line()
+ xlab("Lag") 
+ ylab("Autocorrelation")
+ scale_shape_manual(values=shapevec, name="Model")
+ scale_colour_manual(values=colvec, name="Model")
+ scale_alpha_manual(values=alphavec, name="Model")
+ scale_size_manual(values=lwdvec, name="Model")
+ scale_linetype_manual(values=linetypev, name="Model")
+ ggtitle("Cat 2 ACF")
)

########

allmodsumdf2 <- readRDS("./summary_stats/cat.14.df.RDS")$multimoddf
df2 <- transform(allmodsumdf2, #change allmodsumdf to allmodsumdf2
                 distrib=ifelse(grepl("W",model),"Weibull","log-Normal"),
                 turn_angle=ifelse(grepl("VM",as.character(model)),"yes","no"),
                 homog=ifelse(grepl("TH",type),"Time-heterogeneous","Homogeneous"))
slname <- "Step-length\ndistribution"
(ggplot(df2, aes(x=nstates,y=deltaBIC,size=distrib,
                 colour=distrib,lty=turn_angle))
+ geom_line()
+ facet_wrap(~ homog)
+ scale_colour_brewer(name=slname,palette="Set1")
+ scale_size_manual(name=slname,values=c(1.75,0.75))
## scale_size_continuous( name="# parameters") +
#    geom_point(aes(size=parameters))+
+ scale_linetype(name="von Mises\nturning angle?")
+ labs(y=expression(Delta*"BIC"),
       x="Number of States")
+ geom_hline(yintercept=0,colour="gray")
+ ggtitle("Cat 14 HMM comparisons")
+ zmargin)


linetypev <- c(rep(2,2),rep(1,5))
sumdat <- transform(readRDS(input_files[3])[[1]],
                    mod = ifelse(type=="HMM + TH","HMM + TH","HMM")
)
linetypev <- c(rep(2,2),rep(1,5))
(adj_BIC_plot <- ggplot(sumdat, aes(x=nstates
                                    , y=deltaBIC
                                    , colour=model
                                    , linetype=model
                                    , group=model))
+ facet_wrap(~mod,ncol = 2,scales = "free_x")
+ scale_size_continuous(name="# of states")
# + scale_x_continuous(breaks=seq(0,300,by=30))
+ geom_point() #aes(shape=factor(nstates),alpha=nstates))
+ scale_linetype_manual(values=linetypev, name="Model")
+ scale_colour_brewer(palette="Dark2", name="Model")
# + scale_shape_discrete(name="Number of states")
# + scale_alpha_continuous(name="Number of states")
+ labs(y=expression(paste(Delta, "BIC"))
       , x="Number of parameters") 
+ geom_line()
+ ggtitle("Cat 14 BIC comparisons")
+ zmargin)

cat14simdf <- readRDS(input_files[3])[[2]]
cat14avgdf <- avgdf(cat14simdf)

mnames <- c("hmm","hmmblock",
            "hmmsin","hmmquad","obs","vithmm")
mnames2 <- c("LNVMHMM-homogeneous"
             , "LNVMHMM-block"
             , "LNVMHMM-sin"
             , "LNVMHMM-quadratic"
             , "observed"
             , "viterbi homogenous (n=3)")

combdf <- (cat14avgdf
           %>% transmute(time=time,model=variable,steplen=value)
           %>% filter(model %in% mnames)
           %>% mutate(model=factor(model,levels=mnames,labels=mnames2))
)
linetypev <- c(rep(c(2,1),c(1,4))
               ,0
)
colvec <- c(c(brewer.pal(4,"Dark2"),"black")
            ,"black"
)
lwdvec <- c(rep(c(1,4),c(4,1))
            ,4
)
alphavec <- c(rep(c(1,0.2),c(4,1))
              ,1
)

shapevec <- c(rep(32,5),118)

(avg_plot <- ggplot(combdf,aes(x=time,y=steplen,colour=model,
                               lwd=model,
                               alpha=model,linetype=model, shape=model))
+ geom_point()
+ geom_line()
+ xlab("Time of Day") 
+ scale_shape_manual(values=shapevec, name="Model")
+ ylab(expression('Mean step length'~(log[10]*m)))
+ scale_colour_manual(values=colvec, name="Model")
+ scale_alpha_manual(values=alphavec, name="Model")
+ scale_size_manual(values=lwdvec, name="Model")
+ scale_linetype_manual(values=linetypev, name="Model")
+ ggtitle("Cat 14 Average step-length by time")
) 
###

cat14acfdf <- cat14simdf %>% dplyr:::select(-time)%>%ldply(.,acffun)

combacfdf <- (cat14acfdf
              %>% transmute(lag=lag,model=.id,ACF=ACF)
              %>% filter(model %in% mnames)
              %>% mutate(model=factor(model,levels=mnames,labels=mnames2))
)


(acf_plot <- ggplot(combacfdf,aes(x=lag,y=ACF,colour=model,
                                  lwd=model,alpha=model,linetype=model,shape=model))
+ geom_point()
+ geom_line()
+ xlab("Lag") 
+ ylab("Autocorrelation")
+ scale_shape_manual(values=shapevec, name="Model")
+ scale_colour_manual(values=colvec, name="Model")
+ scale_alpha_manual(values=alphavec, name="Model")
+ scale_size_manual(values=lwdvec, name="Model")
+ scale_linetype_manual(values=linetypev, name="Model")
+ ggtitle("Cat 14 ACF")
)


##################

allmodsumdf2 <- readRDS("./summary_stats/cat.15.df.RDS")$multimoddf
df2 <- transform(allmodsumdf2, #change allmodsumdf to allmodsumdf2
                 distrib=ifelse(grepl("W",model),"Weibull","log-Normal"),
                 turn_angle=ifelse(grepl("VM",as.character(model)),"yes","no"),
                 homog=ifelse(grepl("TH",type),"Time-heterogeneous","Homogeneous"))
slname <- "Step-length\ndistribution"
(ggplot(df2, aes(x=nstates,y=deltaBIC,size=distrib,
                 colour=distrib,lty=turn_angle))
+ geom_line()
+ facet_wrap(~ homog)
+ scale_colour_brewer(name=slname,palette="Set1")
+ scale_size_manual(name=slname,values=c(1.75,0.75))
## scale_size_continuous( name="# parameters") +
#    geom_point(aes(size=parameters))+
+ scale_linetype(name="von Mises\nturning angle?")
+ labs(y=expression(Delta*"BIC"),
       x="Number of States")
+ geom_hline(yintercept=0,colour="gray")
+ ggtitle("Cat 15 HMM comparisons")
+ zmargin)


linetypev <- c(rep(2,2),rep(1,5))
sumdat <- transform(readRDS(input_files[4])[[1]],
                    mod = ifelse(type=="HMM + TH","HMM + TH","HMM")
)
linetypev <- c(rep(2,2),rep(1,5))
(adj_BIC_plot <- ggplot(sumdat, aes(x=nstates
                                    , y=deltaBIC
                                    , colour=model
                                    , linetype=model
                                    , group=model))
+ facet_wrap(~mod,ncol = 2,scales = "free_x")
+ scale_size_continuous(name="# of states")
# + scale_x_continuous(breaks=seq(0,300,by=30))
+ geom_point() #aes(shape=factor(nstates),alpha=nstates))
+ scale_linetype_manual(values=linetypev, name="Model")
+ scale_colour_brewer(palette="Dark2", name="Model")
# + scale_shape_discrete(name="Number of states")
# + scale_alpha_continuous(name="Number of states")
+ labs(y=expression(paste(Delta, "BIC"))
       , x="Number of parameters") 
+ geom_line()
+ ggtitle("Cat 15 BIC comparisons")
+ zmargin)

cat15simdf <- readRDS(input_files[4])[[2]]
cat15avgdf <- avgdf(cat15simdf)

mnames <- c("hmm","hmmblock",
            "hmmsin","hmmquad","obs","vithmm")
mnames2 <- c("LNVMHMM-homogeneous"
             , "LNVMHMM-block"
             , "LNVMHMM-sin"
             , "LNVMHMM-quadratic"
             , "observed"
             , "viterbi homogenous (n=3)")

combdf <- (cat15avgdf
           %>% transmute(time=time,model=variable,steplen=value)
           %>% filter(model %in% mnames)
           %>% mutate(model=factor(model,levels=mnames,labels=mnames2))
)
linetypev <- c(rep(c(2,1),c(1,4))
               ,0
)
colvec <- c(c(brewer.pal(4,"Dark2"),"black")
            ,"black"
)
lwdvec <- c(rep(c(1,4),c(4,1))
            ,4
)
alphavec <- c(rep(c(1,0.2),c(4,1))
              ,1
)

shapevec <- c(rep(32,5),118)

(avg_plot <- ggplot(combdf,aes(x=time,y=steplen,colour=model,
                               lwd=model,
                               alpha=model,linetype=model, shape=model))
+ geom_point()
+ geom_line()
+ xlab("Time of Day") 
+ scale_shape_manual(values=shapevec, name="Model")
+ ylab(expression('Mean step length'~(log[10]*m)))
+ scale_colour_manual(values=colvec, name="Model")
+ scale_alpha_manual(values=alphavec, name="Model")
+ scale_size_manual(values=lwdvec, name="Model")
+ scale_linetype_manual(values=linetypev, name="Model")
+ ggtitle("Cat 15 Average step-length by time")
) 
###

cat15acfdf <- cat15simdf %>% dplyr:::select(-time)%>%ldply(.,acffun)

combacfdf <- (cat15acfdf
              %>% transmute(lag=lag,model=.id,ACF=ACF)
              %>% filter(model %in% mnames)
              %>% mutate(model=factor(model,levels=mnames,labels=mnames2))
)


(acf_plot <- ggplot(combacfdf,aes(x=lag,y=ACF,colour=model,
                                  lwd=model,alpha=model,linetype=model,shape=model))
+ geom_point()
+ geom_line()
+ xlab("Lag") 
+ ylab("Autocorrelation")
+ scale_shape_manual(values=shapevec, name="Model")
+ scale_colour_manual(values=colvec, name="Model")
+ scale_alpha_manual(values=alphavec, name="Model")
+ scale_size_manual(values=lwdvec, name="Model")
+ scale_linetype_manual(values=linetypev, name="Model")
+ ggtitle("Cat 15 ACF")
)

