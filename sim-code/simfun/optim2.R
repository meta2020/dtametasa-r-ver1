##
## Model 2
## Estimation on c1 c2
##

optim2 <- function(data,
                  p,
                  start5,
                  b0 ,
                  c10,
                  b.interval, ## SET A VALUE b.interval in [-5, 5]
                  a.interval,
                  r.up
                  ){


  ##
  ##  INPUT: OPTIMIZATION LOGLIKELIHOOD FUNCTION --------------------------------------
  ##


  fn <- function(par) llk.o(par,
                             data = data, p = p,
                             a.interval = a.interval)

  start7 <- c(start5, b0, c10)
  
  eps <- sqrt(.Machine$double.eps)
  
  opt <- try(nlminb(start7,
                   fn,
                   #method="L-BFGS-B",
                   lower = c(-5, -5, eps, eps,-1, b.interval[1], 0),
                   upper = c( 5,  5, 3, 3, r.up, b.interval[2], 1)
  ),silent = TRUE)

  opt

}

