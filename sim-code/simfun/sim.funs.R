##
## Functions needed in the simulation
## POPULATION DATA ----

bn.data <- function(par){

  ##par: S, se, sp, t1, t2, r
  
  S  <- par[1]
  se <- par[2]
  sp <- par[3]
  t1 <- par[4]
  t2 <- par[5]
  r  <- par[6]
  
  v1 <- rnorm(S, 0.5, 0.5)^2
  v2 <- rnorm(S, 0.5, 0.5)^2

  #if (se<0 || se>1) stop("CHECK SENS IN [0,1]")
  #if (sp<0 || sp>1) stop("CHECK SPEC IN [0,1]")

  u1 <- qlogis(se)
  u2 <- qlogis(sp)

  u   <- c(u1, u2)
  t12 <- r*t1*t2
  t11 <- t1^2
  t22 <- t2^2

  ## Sigma+Omega

  SO <- lapply(1:S, function(s){

    matrix(c(v1[s]+t11, t12, t12, v2[s]+t22),2,2)

  })

  ## CHECK PD

  check.PD <- sapply(1:S, function(s){

    eigen(SO[[s]])$values

  })

  if (any(as.vector(check.PD)<= 0)) stop("VAR-COV MATRIX (S+O) is NOT PD")

  ## y FROM N(u, SO)

  Y <- t(sapply(1:S, function(s) mvtnorm::rmvnorm(1, u, SO[[s]])))

  ## SENS AND SPEC

  X <- plogis(Y)

  ## FINAL DATAFRMAE WITH NAME (y1, y2, v1, v2)

  ldor.t <- rowSums(Y)/sqrt(v1+v2)

  DT <- cbind.data.frame(X, Y, v1, v2, ldor.t)

  colnames(DT) <- c("se", "sp", "y1", "y2", "v1", "v2", "ldor.t")

  return(DT)
}

## AUC ----

sAUC <- function(par.vec){
  
  u1  <- par.vec[1]
  u2  <- par.vec[2]
  t1  <- par.vec[3]
  t2  <- par.vec[4]
  r   <- par.vec[5]
  
  integrate(function(x) { plogis(u1 - (r*t1/t2) * (qlogis(x) + u2)) }, 0, 1)
  
}

mAUC <- function(par.matrix){
  
  if(nrow(par.matrix) < 5) stop("PLEASE CHECK THE INPUT MATRIX")
  
  sapply(1:ncol(par.matrix), function(i) {
    
    u1 = par.matrix[1,i]
    u2 = par.matrix[2,i]
    t1 = par.matrix[3,i]
    t2 = par.matrix[4,i]
    r  = par.matrix[5,i]
    
    if (NA %in% par.matrix[,i]) {auc <- NA} else {
      
      auc.try <- try(
        integrate(function(x) { plogis(u1 - (r*t1/t2) * (qlogis(x) + u2)) }, 0, 1), 
        silent = TRUE)
      
      if(!class(auc.try)=="try-error") auc.try$value else NA
      
    }
    
  })
  
}




##
## LIKELIHOOD FUNCTION (OBSERVED) ----------------------------------------------
##

llk.o <- function(par, data, p, a.interval) {
  
  n <- nrow(data)
  
  y1 <- data$y1
  y2 <- data$y2
  v1 <- data$v1
  v2 <- data$v2
  
  u1 <- par[1]
  u2 <- par[2]
  
  t1  <- par[3]
  t11 <- t1^2
  t2  <- par[4]
  t22 <- t2^2
  r   <- par[5]
  
  t12 <- t1*t2*r
  
  b  <- par[6]
  
  c1  <- par[7]
  c11 <- c1^2
  c22 <- 1-c11
  c2  <- sqrt(c22)
  
  ldor     <- c1*y1 + c2*y2
  
  se.ldor2 <- c11*v1+c22*v2
  se.ldor  <- sqrt(se.ldor2)
  
  u.ldor   <- c1*u1 + c2*u2
  t.ldor   <- c11*t11 + c22*t22 + 2*c1*c2*t12
  
  t        <- ldor/se.ldor
  
  ##
  ## FUNCTOIN b(Sigma) -------------------------------------------------------
  ##
  
  f.b <- function(a){
    
    sq <- suppressWarnings(sqrt(1 + b^2 * (1 + t.ldor/se.ldor2))) 
    
    pnorm( (a + b * u.ldor/se.ldor) / sq )
    
  }
  
  
  ##
  ## FIND THE ROOT OF a = a.opt ----------------------------------------------
  ##
  
  a.p <- function(a) {sum(1/f.b(a), na.rm = TRUE) - n/p}
  
  a.opt.try <- suppressWarnings(try(uniroot(a.p, a.interval, extendInt="downX"), silent = TRUE)) 
  
  a.opt <- a.opt.try$root
  
  
  ##
  ##  LOGLIKELIHOOD-1 OF y|Sigma ---------------------------------------------
  ##
  
  det.vec <- (v1+t11)*(v2+t22)-t12^2
  
  log.det.vec <- suppressWarnings(log(det.vec))
  
  f.l1  <- ((y1-u1)^2*(v2+t22) - 2*(y2-u2)*(y1-u1)*t12 + (y2-u2)^2*(v1+t11)) / det.vec + log.det.vec
  
  s.l1  <- -0.5*sum(f.l1, na.rm = TRUE)
  
  
  ##
  ##  LOGLIKELIHOOD-2 OF a(a.opt) --------------------------------------------
  ##
  
  
  f.l2 <- pnorm(a.opt + b * t)
  
  s.l2 <- sum( log(f.l2), na.rm = TRUE )
  
  
  ##
  ##  LOGLIKELIHOOD-3 OF b(a.opt) --------------------------------------------
  ##
  
  f.l3 <- f.b(a.opt)
  
  s.l3 <- sum( log(f.l3), na.rm = TRUE )
  
  
  ##
  ##  FINAL LOGLIKELIHOOD ----------------------------------------------------
  ##
  
  return(-(s.l1 + s.l2 - s.l3)) ## NEGATIVE
  
}


##
## LIKELIHOOD FUNCTION (POPULATION)---------------------------------------------
##

llk.p <- function(par, data) {
  
  n <- nrow(data)
  
  y1 <- data$y1
  y2 <- data$y2
  v1 <- data$v1
  v2 <- data$v2
  
  u1 <- par[1]
  u2 <- par[2]
  
  t1  <- par[3]
  t11 <- t1^2
  t2  <- par[4]
  t22 <- t2^2
  r   <- par[5]
  
  t12 <- t1*t2*r
  
  ##
  ##  LOGLIKELIHOOD-1 OF y|Sigma ---------------------------------------------
  ##
  
  det.vec <- (v1+t11)*(v2+t22)-t12^2
  
  log.det.vec <- suppressWarnings(log(det.vec))
  
  f.l1  <- ((y1-u1)^2*(v2+t22) - 2*(y2-u2)*(y1-u1)*t12 + (y2-u2)^2*(v1+t11)) / det.vec + log.det.vec
  
  s.l1  <- -0.5*sum(f.l1, na.rm = TRUE)
  
  
  
  ##
  ##  FINAL LOGLIKELIHOOD ----------------------------------------------------
  ##
  
  return(-(s.l1)) ## NEGATIVE
  
}
