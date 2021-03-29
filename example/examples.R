##******************************************************************************
##
## EXAMPLES: IVD DATA AND LYMNODE DATA
## 
## REQUIRED PACKAGES: dtametasa, latex2exp
##
##******************************************************************************

rm(list = ls())

# 1. READ DATA ----

## IVD DATA

# data<- read.csv("data-IVD.csv")

## OR, LYMNODE DATA

# data<- read.csv("data-Lymph.csv")


# 2. SET PARAMETERS IN THE MODEL ----

## B TIMES BOOTSTRAP

B <- 1000

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

## SET COLORS
col <- gray.colors(10, gamma = 1, start = 0, end = 0.8)

## CHOOSE SOME POINTS TO PLOT

pts <- c(2,4,6,8)

## FIGURE 1 / 2

library(latex2exp)

## OUTPUT EPS PLOT INTO LOCAL DEVICE

# setEPS(width = 12, height = 6)
# postscript("plot.eps")

par(mfrow =c(2,4))

sROC.matrix(est2[c(1,2,4,5),  pts])
legend("bottomright", 
       bty='n',
       legend = c(TeX(paste0("$p = 0.9, \\, \\tilde{c}_1^2 =", round(est2[8,2],2), 
                             ",\\, \\tilde{c}_2^2 = ", round(est2[9,2],2), "$")), 
                  TeX(paste0("$p = 0.7, \\, \\tilde{c}_1^2 =", round(est2[8,4],2), 
                             ",\\, \\tilde{c}_2^2 = ", round(est2[9,4],2), "$")), 
                  TeX(paste0("$p = 0.5, \\, \\tilde{c}_1^2 =", round(est2[8,6],2), 
                             ",\\, \\tilde{c}_2^2 = ", round(est2[9,6],2), "$")), 
                  TeX(paste0("$p = 0.3, \\, \\tilde{c}_1^2 =", round(est2[8,8],2), 
                             ",\\, \\tilde{c}_2^2 = ", round(est2[9,8],2), "$"))
       ), 
       col = col[pts], pch = 18, pt.cex = 2,
       lty = rep(1,4))
title("A", adj = 0)


sROC.matrix(est11[c(1,2,4,5), pts])
legend("bottomright", 
       bty='n',
       legend = c(TeX("$p = 0.9, \\, c_1^2 = c_2^2$"), 
                  TeX("$p = 0.7, \\, c_1^2 = c_2^2$"), 
                  TeX("$p = 0.5, \\, c_1^2 = c_2^2$"), 
                  TeX("$p = 0.3, \\, c_1^2 = c_2^2$")), 
       col = col[pts], pch = 18, pt.cex = 2,
       lty = rep(1,4))
title("B", adj = 0)

sROC.matrix(est10[c(1,2,4,5), pts])
legend("bottomright", 
       bty='n',
       legend = c(TeX("$p = 0.9, \\, c_1^2 = 1, \\, c_2^2 = 0$"), 
                  TeX("$p = 0.7, \\, c_1^2 = 1, \\, c_2^2 = 0$"), 
                  TeX("$p = 0.5, \\, c_1^2 = 1, \\, c_2^2 = 0$"), 
                  TeX("$p = 0.3, \\, c_1^2 = 1, \\, c_2^2 = 0$")), 
       col = col[pts], pch = 18, pt.cex = 2,
       lty = rep(1,4))
title("C", adj = 0)


sROC.matrix(est01[c(1,2,4,5),pts])
legend("bottomright", 
       bty='n',
       legend = c(TeX("$p = 0.9, \\, c_1^2 = 0, \\, c_2^2 = 1$"), 
                  TeX("$p = 0.7, \\, c_1^2 = 0, \\, c_2^2 = 1$"), 
                  TeX("$p = 0.5, \\, c_1^2 = 0, \\, c_2^2 = 1$"), 
                  TeX("$p = 0.3, \\, c_1^2 = 0, \\, c_2^2 = 1$")), 
       col = col[pts], pch = 18,pt.cex = 2, 
       lty = rep(1,4))
title("D", adj = 0)

sauc2   <- est2[c(10, 13, 14),]
sauc11  <- est11[c(10, 13, 14),]
sauc10  <- est10[c(10, 13, 14),]
sauc01  <- est01[c(10, 13, 14),]


matplot(t(sauc2), type = "o", lty = c(1,1,1), 
        pch = 19, col = c("black", "grey", "grey"),
        cex = 0.5,
        xlab = "p", ylab = "sAUC",
        ylim = c(0,1),
        xaxt = "n")
axis(1, at = 1:10, labels = p.seq)
title("E", adj = 0)



matplot(t(sauc11), type = "o", lty = c(1,1,1), 
        pch = 19, col = c("black", "grey", "grey"),
        cex = 0.5,
        xlab = "p", ylab = "sAUC",
        ylim = c(0,1),
        xaxt = "n")
axis(1, at = 1:10, labels = p.seq)
title("F", adj = 0)

matplot(t(sauc10), type = "o", lty = c(1,1,1), 
        pch = 19, col = c("black", "grey", "grey"),
        cex = 0.5,
        xlab = "p", ylab = "sAUC",
        ylim = c(0,1),
        xaxt = "n")
axis(1, at = 1:10, labels = p.seq)
title("G", adj = 0)

matplot(t(sauc01), type = "o", lty = c(1,1,1), 
        pch = 19, col = c("black", "grey", "grey"),
        cex = 0.5,
        xlab = "p", ylab = "sAUC",
        ylim = c(0,1),
        xaxt = "n")
axis(1, at = 1:10, labels = p.seq)
title("H", adj = 0)


par(mfrow = c(1,1))

# sink()

# save.image("RData/IVD-est.RData")
# save.image("RData/Lym-est.RData")


