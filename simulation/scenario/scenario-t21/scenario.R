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
## P=0.7, b=0.5, t1=2, t2=1 ----
##
## "set-t21-c11.RData": c1=c2 
## "set-t21-c10.RData": c1=1, c2=0
## "set-t21-c01.RData": c1=0, c2=1
##
##******************************************************************************


set <- lapply(c(25, 50, 200), function(S) {
  
  S   <- rep(S, 6)
  se  <- c(rep(0.5, 2), rep(0.8, 2), rep(0.9, 2))
  sp  <- c(rep(0.85,2), rep(0.8, 2), rep(0.4, 2))
  u1  <- qlogis(se)
  u2  <- qlogis(sp)
  
  t11 <- rep(4, 6) 
  t22 <- rep(1, 6) 
  r   <- rep(c(-0.3, -0.6), 3)
  t12 <- sqrt(t11*t22)*r
  
  b   <- rep(0.5, 6)
  a   <- c(-0.1676463, -0.2535515, -0.7662682, -0.8515198, -0.2004216, -0.2862107)
  
  c11 <- rep(0.5, 6)
  c22 <- 1-c11
  
  sauc<- SAUC(rbind(u1, u2, t22, t12))
  
  cbind(S, u1, u2, t11, t22, t12, c11, c22, b, a, sauc, r, se, sp)
  
})

save(set, file = "set-t21-c11.RData")


set <- lapply(c(25, 50, 200), function(S) {
  
  S   <- rep(S, 6)
  se  <- c(rep(0.5, 2), rep(0.8, 2), rep(0.9, 2))
  sp  <- c(rep(0.85,2), rep(0.8, 2), rep(0.4, 2))
  u1  <- qlogis(se)
  u2  <- qlogis(sp)
  
  t11 <- rep(4, 6) 
  t22 <- rep(1, 6) 
  r   <- rep(c(-0.3, -0.6), 3)
  t12 <- sqrt(t11*t22)*r
  
  b   <- rep(0.5, 6)
  a   <- c( 1.2660037,  1.2713692, -0.1178447, -0.1227172, -0.8451219, -0.8502649)
  
  c11 <- rep(1, 6)
  c22 <- 1-c11
  
  sauc<- SAUC(rbind(u1, u2, t22, t12))
  
  cbind(S, u1, u2, t11, t22, t12, c11, c22, b, a, sauc, r, se, sp)
  
})

save(set, file = "set-t21-c10.RData")


set <- lapply(c(25, 50, 200), function(S) {
  
  S   <- rep(S, 6)
  se  <- c(rep(0.5, 2), rep(0.8, 2), rep(0.9, 2))
  sp  <- c(rep(0.85,2), rep(0.8, 2), rep(0.4, 2))
  u1  <- qlogis(se)
  u2  <- qlogis(sp)
  
  t11 <- rep(4, 6) 
  t22 <- rep(1, 6) 
  r   <- rep(c(-0.3, -0.6), 3)
  t12 <- sqrt(t11*t22)*r
  
  b   <- rep(0.5, 6)
  a   <- c(-0.8791967, -0.8825028, -0.5686037, -0.5682687,  1.4071332,  1.4001445)
  
  c11 <- rep(0, 6)
  c22 <- 1-c11
  
  sauc<- SAUC(rbind(u1, u2, t22, t12))
  
  cbind(S, u1, u2, t11, t22, t12, c11, c22, b, a, sauc, r, se, sp)
  
})

save(set, file = "set-t21-c01.RData")

