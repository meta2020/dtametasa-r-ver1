##
## SIMULATION EXPERIMENT FUNCTION (1 & 2)
##
## COMBINE ml.p, ml.s, ml.a1, ml.a2
## 
##


##
## ONE-TIME ---------------------------------------------------------
##

simu <- function(list.n, 
                 row.n, 
                 b0, 
                 c1, 
                 b.interval, 
                 a.interval,
                 r.up,
                 p0){  
  
  
  conf <- set[[list.n]][row.n,]
  
  ## CONFIGURE
  
  S  <- conf[1]
  
  b  <- conf[9]
  
  if (p0==0.7)  a  <- conf[10]
  
  if (p0==0.85) a  <- conf[11]
   
  

  ## DERIVE DATA (FULL SET: BN)
  
  ##
  ## PDATA -----------------------------------------------------------
  ##
  
  pdata <- bn.data(conf[c(1:3, 6:8)])
  
  
  ml.p <- ml.s <- ml.a1 <- ml.a2<- rep(NA, 8)
  
    
  ##
  ## E.P --------------------------------------------------------
  ##

  t <- pdata$ldor.t
  p.a <- pnorm(a + b*t)
  p.e <- mean(p.a, na.rm = TRUE)

  ##
  ## SDATA ------------------------------------------------------------
  ##
  
  s.id <- sapply(1:S, function(i) rbinom(1, 1, p.a[i])) %in% c(1)
  
  ##  1. ml.P FOR PDATA ------------------------------------------------------------
  
  fit.p <- try(mvmeta(cbind(y1,y2),S=cbind(v1, rep(0, S), v2), data=pdata, method = "ml"), silent = TRUE)
  
  if((!inherits(fit.p, "try-error")) ) {
    
    if(fit.p$converged){
      
      v <-  fit.p$Psi
      ml.p0 <- c(fit.p$coefficients, sqrt(v[c(1, 4)]), v[2]/prod(sqrt(v[c(1, 4)])))  ## u1 u2 t1 t2 r t12
      ml.p[c(1:5, 8)]<- c(ml.p0, fit.p$converged-1)
      
    } 
    
  } 

  ##  2. ml.S FOR SDATA (PB EXISTS) -------------------------------------------------

  if(TRUE %in% s.id) {
    
    sdata<- pdata[s.id,] 
    
    fit.s <- try(mvmeta(cbind(y1,y2),S=cbind(v1, rep(0, nrow(sdata)), v2), data=sdata, method = "ml"),silent=TRUE)
    
    if(!inherits(fit.s, "try-error")) {
      
      if(fit.s$converged){
        
        v <-  fit.s$Psi
        ml.s0 <- c(fit.s$coefficients, sqrt(v[c(1,4)]), v[2]/prod(sqrt(v[c(1,4)]))) ## u1 u2 t1 t2 t12
        ml.s[c(1:5, 8)]  <- c(ml.s0, fit.s$converged-1)
        
      } 
      
    } 
    
  }
  
  if (!(NA %in% ml.s[1:5])) start0 <- c(ml.s[c(1:5)]) else start0 <- c(0, 0, 0.1, 0.1, -0.1)   ##
  
  #start0 <- conf[4:8]
  #start0 <- c(0, 0, 0.1, 0.1, -0.1)
  
  ## 3. ml.A1 FOR SDATA (ADJUST PB) -------------------------------------------

  opt1 <- try(optim1(data=sdata, 
                     p = p.e,   
                     start5 = start0,
                     b0 = b0, 
                     c1 = c1, 
                     b.interval = b.interval,
                     a.interval = a.interval,
                     r.up = r.up), 
              silent = TRUE)
  
  if(!inherits(opt1, "try-error")) {
    

    if(opt1$convergence == 0) ml.a1[-7] <- c(opt1$par, opt1$convergence) 
    
  } 

  
  ## 4. ml.A2 FOR SDATA (ADJUST PB) ------------------------------------------------
  
  ## p.e.
  
  opt2 <- try(optim2(data=sdata, 
                     p = p.e,   
                     start5 = start0,
                     b0 = b0,
                     c10 = c1,
                     b.interval = b.interval,
                     a.interval = a.interval,
                     r.up = r.up), 
              silent = TRUE)
  
  if(!inherits(opt2, "try-error")) {
    
    if(opt2$convergence == 0) ml.a2 <- c(opt2$par[1:6], (opt2$par[7])^2,  opt2$convergence) 
  
    } 

  ## COMBINE RES ------------------------------------------------
  

  x   <- cbind(ml.p, ml.s, ml.a1, ml.a2)
  auc <- mAUC(x[c(1:5),])
  p.e <- c(p.e, rep(NA, 3))
  
  res <- rbind(x, auc, p.e)
  rownames(res) <- c("u1", "u2", "t1", "t2", "r", "b", "c11", "conv", "sAUC", "p.e")
  colnames(res) <- c("ml.p", "ml.s", "ml.a1","ml.a2")

  return(res)
  
}  
