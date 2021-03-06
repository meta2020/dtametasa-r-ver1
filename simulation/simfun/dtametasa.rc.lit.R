##******************************************************************************
##
## PROPOSED MODEL, NOT SPECIFY C VECTOR -- RELEASE C
## 
## REDUCED FUNCTION FOR SIMULATION, OUTPUT PAR (NO SAUC, SE, SP)
##
##******************************************************************************


dtametasa.rc <- function(data,
                  p,
                  brem.init, 
                  c1.sq.init,
                  b.init,
                  b.interval, 
                  a.interval,
                  show.warn.message = FALSE
                  ){

  ##
  ## INPUT: DATA PREPROCESS ----------------------------------------------------------
  ##
  
    n <- nrow(data)
  
    y1 <- data$y1
    y2 <- data$y2
    v1 <- data$v1
    v2 <- data$v2


  ##
  ##  INPUT: OPTIMIZATION LOGLIKELIHOOD FUNCTION --------------------------------------
  ##


    ## AUTO-SET START POINTS

    c1 <- sqrt(c1.sq.init)

    # if(is.null(brem.init)) {
    # 
    #   fit.m <- mvmeta::mvmeta(cbind(y1,y2),S=cbind(v1, rep(0, n), v2), method="ml")
    # 
    #   if(!inherits(fit.m, "try-error")) {
    # 
    #     if(fit.m$converged){
    # 
    #       p1 <- sqrt(fit.m$Psi[1])
    #       p2 <- sqrt(fit.m$Psi[4])
    #       p.r<- fit.m$Psi[3]/(p1*p2)
    # 
    #       start7 <- c(fit.m$coefficients, p1, p2, p.r, b.init, c1)
    # 
    #     } else start7 <- c(0, 0, 0.1, 0.1, -0.1, b.init, c1)
    # 
    #   } else start7 <- c(0, 0, 0.1, 0.1, -0.1, b.init, c1)
    # 
    # } else 
      
    start7 <- c(brem.init, b.init, c1)


    eps <- sqrt(.Machine$double.eps)

    fn <- function(par) llk.o(par,
                              y1, y2, v1, v2,
                              n, p,
                              a.interval,
                              show.warn.message)

    opt <- try(nlminb(start7,
                     fn,
                     lower = c(-5, -5, eps, eps,-1, b.interval[1], 0),
                     upper = c( 5,  5, 3, 3,  1, b.interval[2], 1)
    ),silent = TRUE)

  if(!inherits(opt,"try-error")) {

 
    ##
    ##  OUTPUT: ALL PARAMETERS -------------------------------------------------
    ##

    u1  <- opt$par[1]
    u2  <- opt$par[2]

    t1  <- opt$par[3]
    t11 <- t1^2
    t2  <- opt$par[4]
    t22 <- t2^2
    r   <- opt$par[5]
    t12 <- t1*t2*r

    b   <- opt$par[6]

    c1  <- opt$par[7]
    c11 <- c1^2
    c22 <- 1-c11
    c2  <- sqrt(c22)


    u.ldor   <- c1*u1 + c2*u2
    t.ldor   <- c11*t11 + c22*t22 + 2*c1*c2*t12

    se.ldor2 <- c11*v1+c22*v2
    se.ldor  <- sqrt(se.ldor2)

    sq     <- sqrt(1 + b^2 * (1 + t.ldor/se.ldor2))

    ##
    ## ALPHA CALC --------------------------------------------------------
    ##

    a.p <- function(a){ sum(1/ pnorm( (a + b * u.ldor/se.ldor) / sq ), na.rm = TRUE) - n/p }

    if (!show.warn.message) a.opt.try <- suppressWarnings(try(uniroot(a.p, a.interval, extendInt="downX"), silent = TRUE)) else a.opt.try <- try(uniroot(a.p, a.interval, extendInt="downX"), silent = TRUE)

    a.opt <- a.opt.try$root


    ##
    ## PAR --------------------------------------------------------
    ##

    opt$par2 <- c(u1, u2, t11, t22, t12,  b, a.opt, c11)
    
    names(opt$par2) <- c("u1", "u2", "t11", "t22", "t12", "b", "a", "c11")
    
    opt$par <- c(u1, u2, t1, t2, r, b, a.opt, c1)
    
    names(opt$par) <- c("u1", "u2", "t1", "t2", "r", "b", "a", "c1")


  }

  opt

}

