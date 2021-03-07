##
## Model 1
## No estimation on c1 c2
##

optim1 <- function(data,   ## 2 FORMAT: N OR Y, make data name as format
                   p,
                   c1,
                   start5,  
                   b0 ,
                   b.interval,
                   a.interval,
                   r.up
){


  ##
  ##  INPUT: OPTIMIZATION LOGLIKELIHOOD FUNCTION --------------------------------------
  ##

  fn <- function(par) llk.o(c(par[1:6], c1),
                             data = data, 
                             p = p,
                             a.interval = a.interval)

  start6 <- c(start5, b0)
  eps <- sqrt(.Machine$double.eps)

  
  opt <- try(nlminb(start6,
                   fn,
                   #method="L-BFGS-B",
                   lower = c(-5, -5, eps, eps, -1, b.interval[1]),
                   upper = c( 5,  5, 3, 3, r.up , b.interval[2])
  ), silent = TRUE)
  
  
  opt

}




