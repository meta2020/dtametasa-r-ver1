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

source("simfun/llk.o.R")
source("simfun/data.pre.R")
source("simfun/dtametasa.fc.R")
source("simfun/dtametasa.rc.R")
source("simfun/sauc.R")


# 1. READ DATA ----


## IVD DATA OR, LYMNODE DATA *******

dtname <- "data-IVD"

# dtname <- "data-Lymph"

data<- read.csv(paste0(dtname,".csv"))


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

  opt2$par
  
})



## ESITMATION WHEN WE SET c1 = c2

est11 <- sapply(p.seq, function(p) {
  
  opt1 <- dtametasa.fc(data, p)
  
  opt1$par
  
  })


## ESITMATION WHEN WE SET c1 = 1, c2 = 0

est10 <- sapply(p.seq, function(p) {
  
  opt1 <- dtametasa.fc(data, p, c1.sq =1)
  

  opt1$par
  
})


## ESITMATION WHEN WE SET c1 = 0, c2 = 1

est01 <- sapply(p.seq, function(p) {
  
  opt1 <- dtametasa.fc(data, p, c1.sq = 0)
  
  opt1$par
  
})


## ALL RESULTS

colnames(est2)<- paste0("p = ", p.seq)
est2

colnames(est11)<- paste0("p = ", p.seq)
est11


colnames(est10)<- paste0("p = ", p.seq)
est10


colnames(est01)<- paste0("p = ", p.seq)
est01


save.image(paste0("RData/", dtname,".RData"))

