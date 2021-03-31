##******************************************************************************
##
## ADD CI OF SROC ONTO SROC PLOT
##
##******************************************************************************


plot.sROC.ci <- function(x,
                         sroc.ci.col = "grey",
                         sroc.ci.lty = 1,
                         sroc.ci.lwd = 1,
                         add.sROC.ci=TRUE,
                         ci.level = 0.95){
  
  if(!inherits(x, "sROC.ci")) stop("ONLY VALID FOR RESULTS OF FUNCTION sAUC.ci")
  
  PAR <- x$bootstrap.par
  B <- dim(PAR)[1]
  
  
  fpr.t <- seq(0,1,0.001)
  se.t  <- sapply(1:B, function(i){
    
    u1  <- PAR[i,1]
    u2  <- PAR[i,2]
    t22 <- PAR[i,3]
    t12 <- PAR[i,4]
    
    plogis(u1 - (t12/t22) * (qlogis(fpr.t) + u2))
    
  })
  
  ci <- cbind(
    
    apply(se.t, 1, function(x) quantile(x, (1-ci.level)/2, na.rm = TRUE)),
    apply(se.t, 1, function(x) quantile(x, probs = 1-(1-ci.level)/2, na.rm = TRUE))
    
  )
  
  matplot(x = fpr.t, y = ci, type = "l",
          col = sroc.ci.col, lty = sroc.ci.lty, lwd = sroc.ci.lty,
          add = TRUE)
  
  list <- c(x, sroc.ci <- list(x = fpr.t, y = ci))
  
  
}
