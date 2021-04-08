##******************************************************************************
##
## EXAMPLES: IVD DATA AND LYMNODE DATA
##
## ATTENTION: THIS MAY BE TIME CONSUMING
##
## SAVE DATA INTO RDATA/
##
##******************************************************************************

rm(list = ls())

library("mvmeta")
library("foreach")
library("parallel")
library("doSNOW")
library("doRNG")


source("simfun/llk.o.R")
source("simfun/data.pre.R")
source("simfun/dtametasa.fc.R")
source("simfun/dtametasa.rc.R")
source("simfun/sauc.ci.R")
source("simfun/sauc.R")


# 1. READ DATA ----


## IVD DATA OR, LYMNODE DATA

# dtname <- "data-IVD"

dtname <- "data-Lymph"

data<- read.csv(paste0(dtname,".csv"))


# 2. SET PARAMETERS IN THE MODEL ----

## B TIMES BOOTSTRAP

B <- 2000

## HIDE PROGRESS BAR IN THE FUNCITON

hide <- TRUE

## SET SEED

seed <- 2021


##********************************************************
##
## ESTIMATION OF THE PROPOSED MODELS ----
##
##********************************************************


## SET SELECTION PROBABILITY p = 1, 0.9, ..., 0.1

(p.seq <- seq(1, 0.1, -0.1))


## ESITMATION WHEN WE TREAT c1, c2 AS PARAMETERS

est2 <- sapply(p.seq, function(p) {
  
  ## ESTIMATES OF THE PARAMETERS
  opt2 <- dtametasa.rc(data, p)
  
  ## CONFIDENCE INTERVALS (CI) 
  sauc <- sAUC.ci(opt2, B=B, hide.progress = hide, set.seed = seed)
  
  c(opt2$par, sauc[[2]], sauc[[3]])
  
})



## ESITMATION WHEN WE SET c1 = c2

est11 <- sapply(p.seq, function(p) {
  
  opt1 <- dtametasa.fc(data, p)
  
  sauc <- sAUC.ci(opt1, B=B, hide.progress = hide, set.seed = seed)
  
  c(opt1$par, sauc[[2]], sauc[[3]])
  
  })


## ESITMATION WHEN WE SET c1 = 1, c2 = 0

est10 <- sapply(p.seq, function(p) {
  
  opt1 <- dtametasa.fc(data, p, c1.sq =1)
  
  sauc <- sAUC.ci(opt1, B=B, hide.progress = hide, set.seed = seed)
  
  c(opt1$par, sauc[[2]], sauc[[3]])
  
})


## ESITMATION WHEN WE SET c1 = 0, c2 = 1

est01 <- sapply(p.seq, function(p) {
  
  opt1 <- dtametasa.fc(data, p, c1.sq = 0)
  
  sauc.try <- sAUC.ci(opt1, B=B, hide.progress = hide, set.seed = seed)
  
  c(opt1$par, sauc.try[[2]], sauc.try[[3]])
  
})


## ALL RESULTS

colnames(est2)<- paste0("p = ", p.seq)
rownames(est2)[13:14]<- c("sauc.ci.lb", "sauc.ci.ub")
est2

colnames(est11)<- paste0("p = ", p.seq)
rownames(est11)[13:14]<- c("sauc.ci.lb", "sauc.ci.ub")
est11


colnames(est10)<- paste0("p = ", p.seq)
rownames(est10)[13:14]<- c("sauc.ci.lb", "sauc.ci.ub")
est10


colnames(est01)<- paste0("p = ", p.seq)
rownames(est01)[13:14]<- c("sauc.ci.lb", "sauc.ci.ub")
est01


save.image(paste0("RData/",dtname,".RData"))

