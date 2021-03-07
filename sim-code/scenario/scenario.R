##******************************************************************************
##
## TRUE PARAMETERS FOR DATA GENERATION
## 
## PRODUCE TRUE1.RDATA, TRUE2.RDATA, TRUE3.RDATA
##
##******************************************************************************

source("../simfun/sim.funs.R")

##******************************************************************************
##
## SCENARIO 1, 2, 3, 4----
##
##******************************************************************************

set <- lapply(c(25, 50, 200), function(S) {
  
  t <- 4
  S   <- rep(S, t)
  se  <- rep(0.5, t)
  sp  <- rep(0.85,t)
  u1  <- qlogis(se)
  u2  <- qlogis(sp)
  
  t1 <- rep(c(0.5, 1), each = 2) ## t1 = 0.5
  t2 <- rep(c(0.5, 2), each = 2) ## t2 = 0.5
  r   <- rep(c(-0.6, -0.3), t/2)
  #t12 <- t1*t2*r
  
  b   <- rep(0.5, t)
  a1  <- c(-0.5, -0.5, -0.2, -0.2)
  a2  <- c(0.20, 0.2, 0.5, 0.7)
  c11 <- rep(0.5, t)
  #c22 <- rep(0.5, t)
  sauc <- mAUC(rbind(u1, u2, t1, t2, r))
  
  
  set <- cbind(S, se, sp, u1, u2, t1, t2, r, b, a1, a2, c11, sauc)
  
  return(set)
  
})
save(set, file = "true1.RData")

##******************************************************************************
##
## SCENARIO 5, 6, 7, 8 ----
##
##******************************************************************************

set <- lapply(c(25, 50, 200), function(S) {
  
  t <- 4
  S   <- rep(S, t)
  se  <- rep(0.8, t)
  sp  <- rep(0.8,t)
  u1  <- qlogis(se)
  u2  <- qlogis(sp)
  
  t1 <- rep(c(0.5, 1), each = 2) ## t1 = 0.5
  t2 <- rep(c(0.5, 2), each = 2) ## t2 = 0.5
  r   <- rep(c(-0.6, -0.3), t/2)
  #t12 <- t1*t2*r
  
  
  b   <- rep(0.5, t)
  a1  <- c(-1, -1.05, -0.85, -0.8)
  a2  <- c(-0.35, -0.35, -0.1, 0.1)
  c11 <- rep(0.5, t)
  #c22 <- rep(0.5, t)
  sauc <- mAUC(rbind(u1, u2, t1, t2, r))
  
  set <- cbind(S, se, sp, u1, u2, t1, t2, r, b, a1, a2, c11, sauc)
  
  return(set)
  
})

save(set, file = "true2.RData")

##******************************************************************************
##
## SCENARIO 9, 10, 11, 12 ----
##
##******************************************************************************

set <- lapply(c(25, 50, 200), function(S) {
  
  t <- 4
  S   <- rep(S, t)
  se  <- rep(0.9, t)
  sp  <- rep(0.4,t)
  u1  <- qlogis(se)
  u2  <- qlogis(sp)
  
  t1 <- rep(c(0.5, 1), each = 2) ## t1 = 0.5
  t2 <- rep(c(0.5, 2), each = 2) ## t2 = 0.5
  r   <- rep(c(-0.6, -0.3), t/2)
  #t12 <- t1*t2*r
  
  
  b   <- rep(0.5, t)
  a1  <- c(-0.5, -0.5, -0.3, -0.2)
  a2  <- c(0.20, 0.20, 0.5, 0.65)
  c11 <- rep(0.5, t)
  #c22 <- rep(0.5, t)
  
  sauc <- mAUC(rbind(u1, u2, t1, t2, r))
  
  set <- cbind(S, se, sp, u1, u2, t1, t2, r, b, a1, a2, c11, sauc)
  
  return(set)
  
})

save(set, file = "true3.RData")

