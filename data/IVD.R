##******************************************************************************
##
## Example 1: diagnosis of intra vascular device related bloodstream infection 
##
##******************************************************************************

rm(list = ls())

## Prepare data ----
data<- read.csv("data-IVD.csv")

## Set parameters in the model ----

## 1000 times bootstrap

B <- 1000      

## Permit positive value in rho

pos.r <- TRUE

## Hide progress bar in the bootstrap

hide <- TRUE

## Set seed

seed <- 2021


##*********************************************************
##
## Standard model without considering publication bias ----
##
## No continuity correction in the points plot
##
##*********************************************************

library(mada)

fit <- reitsma(data, method = "ml")
plot(sroc(fit, type = "naive"), type = "l",
     xlim = c(0,1), ylim = c(0,1))
points(1-with(data, TN/(TN+FP)), with(data, TP/(TP+FN)), pch = 4)
title("sROC from Reistma model")

##********************************************************
##
## The proposed models that consider publication bias ----
##
##********************************************************

library(DTAsens)

## When p0 = 0.7

p0 <- 0.7

## Estimations from two proposed models

(opt1 <- dtasens1(data, p = p0, pos.r = pos.r))
(opt2 <- dtasens2(data, p = p0, pos.r = pos.r))

## Plot: sROC with confidence intervals (CIs) from two proposed models

par(mfrow = c(1,2))

sauc.ci1 <- sAUC.ci(opt1, B = B, plot.ROC.ci = TRUE, hide.progress = TRUE, set.seed = seed)
title("Estimated sROC given c1 = c2")

sauc.ci2 <- sAUC.ci(opt2, B = B, plot.ROC.ci = TRUE, hide.progress = TRUE, set.seed = seed)
title("Estimated sROC")

par(mfrow = c(1,1))


## When p = 1, 0.9, ..., 0.1

(p.seq <- seq(1, 0.1, -0.1))

## Estimations from two proposed models

## Estimated with c1 and c2

est2 <- sapply(p.seq, function(p) {
  opt2 <- dtasens2(data, p, pos.r = pos.r)
  sauc <- sAUC.ci(opt2, B=B, hide.progress = hide, set.seed = seed)
  c(opt2$par, sauc[[2]], sauc[[3]])
})


## Estimated with given c1 = c2

est1 <- sapply(p.seq, function(p) {
  opt1 <- dtasens1(data, p, pos.r = pos.r)
  sauc <- sAUC.ci(opt1, B=B, hide.progress = hide, set.seed = seed)
  c(opt1$par, sauc[[2]], sauc[[3]])
  })



## Print results

colnames(est1)<- paste0("p = ", p.seq)
rownames(est1)[14:15]<- c("sAUC.CI.L", "sAUC.CI.U")
est1

colnames(est2)<- paste0("p = ", p.seq)
rownames(est2)[14:15]<- c("sAUC.CI.L", "sAUC.CI.U")
est2


## Plot: sROC from two proposed models

## Color: from dark (p = 1) to light (p = 0.1)

par(mfrow = c(1,2))

msROC(est1)
title("sAUC estimated given c1 = c2")

msROC(est2)
title("sAUC estimated with free c1 c2")

par(mfrow = c(1,1))

## Plot: sAUC with CIs from two proposed models

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



save.image("RData/IVDest.RData")


