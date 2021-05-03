##******************************************************************************
##
## TRUE PARAMETERS IN THE SIMULATION DATA
##
##******************************************************************************

source("../../simfun/sauc.R")

##******************************************************************************
##
## A TOTAL NUMBER OF 6 SCENARIOS, IN 3 SAMPLE SIZES
##
## P=0.7, b=0.5, t1=t2=0.5 ----
##
## "set-t0.5-c11.RData": c1=c2 
## "set-t0.5-c10.RData": c1=1, c2=0
## "set-t0.5-c01.RData": c1=0, c2=1
##
##******************************************************************************


set <- lapply(c(25, 50, 200), function(S) {
  
  S   <- rep(S, 6)
  se  <- c(rep(0.5, 2), rep(0.8, 2), rep(0.9, 2))
  sp  <- c(rep(0.85,2), rep(0.8, 2), rep(0.4, 2))
  u1  <- qlogis(se)
  u2  <- qlogis(sp)
  
  t11 <- rep(0.5^2, 6) 
  t22 <- rep(0.5^2, 6) 
  r   <- rep(c(-0.3, -0.6), 3)
  t12 <- sqrt(t11*t22)*r
  
  b   <- rep(0.5, 6)
  a   <- c(-0.4642380, -0.4850436, -1.0351978, -1.0495008, -0.4980358, -0.5156251)
  
  c11 <- rep(0.5, 6)
  c22 <- 1-c11
  
  sauc<- SAUC(rbind(u1, u2, t22, t12))
  
  cbind(S, u1, u2, t11, t22, t12, c11, c22, b, a, sauc, r, se, sp)
  
})

save(set, file = "set-t0.5-c11.RData")


set <- lapply(c(25, 50, 200), function(S) {
  
  S   <- rep(S, 6)
  se  <- c(rep(0.5, 2), rep(0.8, 2), rep(0.9, 2))
  sp  <- c(rep(0.85,2), rep(0.8, 2), rep(0.4, 2))
  u1  <- qlogis(se)
  u2  <- qlogis(sp)
  
  t11 <- rep(0.5^2, 6) 
  t22 <- rep(0.5^2, 6) 
  r   <- rep(c(-0.3, -0.6), 3)
  t12 <- sqrt(t11*t22)*r
  
  b   <- rep(0.5, 6)
  a   <- c( 0.7281141, 0.7282932, -0.7715298, -0.7736863, -1.4076683, -1.4066477)
  
  c11 <- rep(1, 6)
  c22 <- 1-c11
  
  sauc<- SAUC(rbind(u1, u2, t22, t12))
  
  cbind(S, u1, u2, t11, t22, t12, c11, c22, b, a, sauc, r, se, sp)
  
})

save(set, file = "set-t0.5-c10.RData")


set <- lapply(c(25, 50, 200), function(S) {
  
  S   <- rep(S, 6)
  se  <- c(rep(0.5, 2), rep(0.8, 2), rep(0.9, 2))
  sp  <- c(rep(0.85,2), rep(0.8, 2), rep(0.4, 2))
  u1  <- qlogis(se)
  u2  <- qlogis(sp)
  
  t11 <- rep(0.5^2, 6) 
  t22 <- rep(0.5^2, 6) 
  r   <- rep(c(-0.3, -0.6), 3)
  t12 <- sqrt(t11*t22)*r
  
  b   <- rep(0.5, 6)
  a   <- c(-1.0519481, -1.0550662, -0.7693638, -0.7712513,  1.3170592,  1.3096783)
  
  c11 <- rep(0, 6)
  c22 <- 1-c11
  
  sauc<- SAUC(rbind(u1, u2, t22, t12))
  
  cbind(S, u1, u2, t11, t22, t12, c11, c22, b, a, sauc, r, se, sp)
  
})

save(set, file = "set-t0.5-c01.RData")

