##******************************************************************************
##
## EXAMPLES: IVD DATA AND LYMNODE DATA
##
##******************************************************************************

rm(list = ls())

# 1. READ DATA ----

## IVD

# data<- read.csv("data-IVD.csv")

## LYMNODE

# data<- read.csv("data-Lymph.csv")


# 2. SET PARAMETERS IN THE MODEL ----

## B TIMES BOOTSTRAP

B <- 2      

## HIDE PROGRESS BAR IN THE FUNCITON

hide <- TRUE

## SET SEED

seed <- 2021


##********************************************************
##
## ESTIMATION OF THE PROPOSED MODELS ----
##
##********************************************************

library(dtametasa)


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


## ESITMATION WHEN WE SET c1 = 1, c2 = 0

est01 <- sapply(p.seq, function(p) {
  
  opt1 <- dtametasa.fc(data, p, c1.sq =0)
  
  sauc <- sAUC.ci(opt1, B=B, hide.progress = hide, set.seed = seed)
  
  c(opt1$par, sauc[[2]], sauc[[3]])
  
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



## APPENDIX TABLE

cbind.data.frame(t(est2[c(10,13,14,8,9,1,2),]), 
                 t(est11[c(10,13,14,8,9),]),
                 t(est10[c(10,13,14,8,9),]),
                 t(est01[c(10,13,14,8,9),]))





##********************************************************
##
## PLOT: SAUC AND SROC FROM THE PROPOSED MODELS ----
##
##********************************************************


## Color: from dark (p = 1) to light (p = 0.1)

pt.id <- c(2,4,6,8)
  
par(mfrow = c(2,4))

sROC.matrix(est2[c(1,2,4,5), pt.id])
title("A")

sROC.matrix(est11[c(1,2,4,5), pt.id])
title("B")

sROC.matrix(est10[c(1,2,4,5), pt.id])
title("C")

sROC.matrix(est01[c(1,2,4,5), pt.id])
title("D")



## PLOT: SAUC WITH CI

sauc1 <- est1[c(11, 14, 15),]
sauc2 <- est2[c(11, 14, 15),]

par(mfrow = c(1,2))

matplot(t(sauc1), type = "o", lty = c(1,1,1), 
        pch = 19, col = c("black", "grey", "grey"),
        cex = 0.5,
        xlab = "p", ylab = "sAUC",
        ylim = c(0,1),
        xaxt = "n")
axis(1, at = 1:10, labels = p.seq)
title("Estimated sAUC given c1 = c2")

matplot(t(sauc2), type = "o", lty = c(1,1,1), 
        pch = 19, col = c("black", "grey", "grey"),
        cex = 0.5,
        xlab = "p", ylab = "sAUC",
        ylim = c(0,1),
        xaxt = "n")
axis(1, at = 1:10, labels = p.seq)
title("Estimated sAUC")

par(mfrow = c(1,1))



# save.image("RData/IVDest.RData")


