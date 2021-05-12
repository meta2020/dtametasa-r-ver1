##******************************************************************************
##
## PROPOSED MODEL, SPECIFY C VECTOR, SA1
## 
##
##******************************************************************************


dtametasa.fc <- function(data,   
                     p,
                     c1.sq = 0.5, 
                     correct.value = 0.5,
                     correct.type = "all",
                     brem.init = NULL,  
                     b.init = 1,
                     b.interval = c(0, 2),
                     a.interval = c(-3, 3),
                     positive.r = TRUE,
                     ci.level = 0.95,
                     show.warn.message = FALSE,
                     plot.sroc = TRUE,
                     a.root.extendInt = "downX",
                     num.var = FALSE
){

  ##
  ## INPUT: DATA PREPROCESS  ----------------------------------------------------------
  ##

  if (p <=0 || p>1) stop("PLEASE SET SELECTION PROB: P in (0, 1]",  call. = FALSE)

  if (!any(c("y1","y1", "v1", "v2", "TP", "FN", "TN", "FP") %in% names(data))) stop("DATA' COLNAMES MUST BE 'TP/FN/TN/FP' OR 'y1/y2/v1/v2'", call. = FALSE)

  n <- nrow(data)

  if ("TP" %in% names(data)){

    data <- correction(data, value = correct.value, type= correct.type)

    data <- logit.data(data)

  }

  y1 <- data$y1
  y2 <- data$y2
  v1 <- data$v1
  v2 <- data$v2


  c11 <- c1.sq
  c22 <- 1-c11
  c1  <- sqrt(c11)
  c2  <- sqrt(c22)

  ##
  ##  INPUT: OPTIMIZATION LOGLIKELIHOOD FUNCTION --------------------------------------
  ##

  ## AUTO-SET START POINTS

  #start6 <- c(0, 0, 0.1, 0.1, -0.1, b.init)

  if(is.null(brem.init)) {

    fit.m <- mvmeta::mvmeta(cbind(y1,y2),S=cbind(v1, rep(0, n), v2), method="ml")

    if(!inherits(fit.m, "try-error")) {

      if(fit.m$converged){

        p1 <- sqrt(fit.m$Psi[1])
        p2 <- sqrt(fit.m$Psi[4])
        p.r<- fit.m$Psi[3]/(p1*p2)

        start6 <- c(fit.m$coefficients, p1, p2, p.r, b.init)

      } else start6 <- c(0, 0, 0.1, 0.1, -0.1, b.init)

    } else start6 <- c(0, 0, 0.1, 0.1, -0.1, b.init)

  } else start6 <- c(brem.init, b.init)


  eps <- sqrt(.Machine$double.eps)

  if(positive.r) r.up <- 1 else  r.up <- eps

  fn <- function(par) llk.o(par = c(par[1:6], c1),
                            y1, y2, v1, v2, n, p,
                            a.interval,
                            a.root.extendInt, 
                            show.warn.message)

  opt <- try(nlminb(start6,
                   fn,
                   lower = c(-5, -5, eps, eps, -1, b.interval[1]),
                   upper = c( 5,  5, 3, 3, r.up , b.interval[2])
  ), silent = TRUE)


  if(!inherits(opt,"try-error")) {
  
    ##
    ##  OUTPUT: ALL PARAMETERS -------------------------------------------------
    ##

    u1  <- opt$par[1]
    se  <- plogis(u1)
    u2  <- opt$par[2]
    sp  <- plogis(u2)
    t1  <- opt$par[3]
    t11 <- t1^2
    t2  <- opt$par[4]
    t22 <- t2^2
    r   <- opt$par[5]
    t12 <- t1*t2*r

    b  <- opt$par[6]

    t.ldor <- c11*t11 + c22*t22 + 2*c1*c2*t12
    u.ldor <- c1*u1 + c2*u2

    se.ldor2 <- c11*v1+c22*v2
    se.ldor  <- sqrt(se.ldor2)

    sq     <- sqrt(1 + b^2 * (1 + t.ldor/se.ldor2))

    ##
    ## ALPHA CALC --------------------------------------------------------
    ##

   a.p <- function(a) { sum(1/ pnorm( (a + b * u.ldor/se.ldor) / sq ), na.rm = TRUE) - n/p }

    if (!show.warn.message) a.opt.try <- suppressWarnings(try(
      uniroot(a.p, interval = a.interval, extendInt = a.root.extendInt),
      silent = TRUE)) else a.opt.try <- try(
        uniroot(a.p, interval = a.interval, extendInt=a.root.extendInt), silent = TRUE)

    a.opt <- a.opt.try$root
    
    ##
    ##  HESSIANS -------------------------------------------------
    ##
    
    drv.fun <- llk.ind(u1, u2, t1, t2, r, b, a.opt, c1, c2,
                       y1, y2, v1, v2)
    
    hes.data <- attr(drv.fun, "hessian")
    opt$hessian <- sapply(1:7, function(i) colSums(hes.data[,,i], na.rm = TRUE))[1:6,1:6]
    
    rownames(opt$hessian) <- c("u1", "u2", "t1", "t2", "r", "b")
    colnames(opt$hessian) <- c("u1", "u2", "t1", "t2", "r", "b")
    
    opt$num.hessian <- numDeriv::hessian(fn, opt$par)
    rownames(opt$num.hessian) <- c("u1", "u2", "t1", "t2", "r", "b")
    colnames(opt$num.hessian) <- c("u1", "u2", "t1", "t2", "r", "b")

    ##
    ## SAUC CI -------------------------------------------------
    ##
    if (num.var) hes <- opt$num.hessian else hes <- opt$hessian
    if(p==1) inv.I.fun.m <- solve(hes[1:5,1:5]) else inv.I.fun.m <- solve(hes[1:6, 1:6])
    
    opt$var.ml <- inv.I.fun.m
    
    f <- function(x) plogis(u1 - (t1*t2*r/(t2^2)) * (qlogis(x) + u2))
    
    f.lb <- function(x) plogis( u1 - (t1*t2*r/t2^2) * (qlogis(x) + u2) + 
                                   qnorm((1-ci.level)/2)* 
                                  suppressWarnings(
                                    sqrt(QIQ(x, u1, u2, t1, t2, r, inv.I.fun.m[1:5,1:5])))) 
    
    f.ub <- function(x) plogis( u1 - (t1*t2*r/t2^2) * (qlogis(x) + u2) + 
                                   qnorm(1-(1-ci.level)/2)* 
                                  suppressWarnings(
                                     sqrt(QIQ(x, u1, u2, t1, t2, r, inv.I.fun.m[1:5,1:5])))) 
    
    sauc.try <- try(integrate(f, 0, 1))
    if(!inherits(sauc.try, "try-error")) sauc <- sauc.try$value else sauc <- NA
    
    # sauc.lb.try <- try(integrate(f.lb, 0, 1))
    # if(!inherits(sauc.lb.try, "try-error"))  sauc.lb <- sauc.lb.try$value else sauc.lb <- NA
    # 
    # sauc.ub.try <- try(integrate(f.ub, 0, 1))
    # if(!inherits(sauc.ub.try, "try-error"))  sauc.ub <- sauc.ub.try$value else sauc.ub <- NA
    # 
    # opt$sauc.ci1 <- c(sauc, sauc.lb, sauc.ub)
    # names(opt$sauc.ci1) <- c("sauc", "sroc.lb", "sroc.ub")
    
    
    sauc.lb2 <-  plogis(qlogis(sauc) + qnorm((1-ci.level)/2) *
                          suppressWarnings(
                            sqrt(QIQ1(x, u1, u2, t1, t2, r, inv.I.fun.m[1:5,1:5]))/(sauc*(1-sauc))) )
    
    sauc.ub2 <-  plogis(qlogis(sauc) + qnorm(1-(1-ci.level)/2)* 
                          suppressWarnings(
                            sqrt(QIQ1(x, u1, u2, t1, t2, r, inv.I.fun.m[1:5,1:5]))/(sauc*(1-sauc))) )
    
    opt$sauc.ci <- c(sauc, sauc.lb2, sauc.ub2)
    names(opt$sauc.ci) <- c("sauc", "sauc.lb", "sauc.ub")
    
    ##
    ## b CI -------------------------------------------------
    ##
    # if(p==1) opt$b.ci <- c(b, NA, NA) else {
    #   
    #   b.se <- suppressWarnings(sqrt(inv.I.fun.m[6,6]))
    #   b.lb <- b + qnorm((1-ci.level)/2)*b.se
    #   b.ub <- b + qnorm(1-(1-ci.level)/2)*b.se
    #   
    #   opt$b.ci <- c(b, b.lb, b.ub)
    #   
    # }
    # 
    # names(opt$b.ci) <- c("b", "b.lb", "b.ub")
    
    ##
    ## ALL PAR ----------------------------------------
    ##
    if(p==1) opt$par.all <- c(u1, u2, t11, t22, t12, NA, NA, NA, NA, sauc, se, sp)  else opt$par.all <- c(u1, u2, t11, t22, t12, c11, c22, b, a.opt, sauc, se, sp)
      
    names(opt$par.all) <- c("u1", "u2", "t11", "t22", "t12", "c11", "c22", "b", "a", "sauc", "se", "sp")
    
    if(p==1) opt$par <- c(u1, u2, t1, t2, r, NA, NA, NA, NA, sauc, se, sp)  else opt$par <- c(u1, u2, t1, t2, r, c1, c2, b, a.opt, sauc, se, sp)
    
    names(opt$par) <- c("u1", "u2", "t1", "t2", "r", "c1", "c2", "b", "a", "sauc", "se", "sp")

    ##
    ##  P.HAT CALC, FROM b FUNCTION ----------------------------------------
    ##

    bp <- pnorm( (a.opt + b * u.ldor/se.ldor) / sq )

    opt$p.hat <- n/sum(1/bp)

    opt$data <- data
    
   
    ##
    ## END ----------------------------------------
    ##
    
    class(opt) <- "dtametasa"
    
    ##
    ## PLOT SROC ----------------------------------------
    ##
    
    if(plot.sroc){
      
      curve(f, xlim = c(0,1), ylim = c(0,1), xlab = "1 - specificity", ylab = "sensitivity")
      curve(f.ub, add = TRUE, lty=2)
      curve(f.lb, add = TRUE, lty=2)
      
    }
}

  opt

}




