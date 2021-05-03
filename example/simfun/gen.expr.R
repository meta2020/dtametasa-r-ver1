
library(rlang)
gen.expr <- function()
  
{  
  t11 <- expr(t1^2)
  t22 <- expr(t2^2)
  t12 <- expr(t1*t2*r)
  c11<- expr(c1^2)
  #c1 <- expr(sqrt(c11))
  c22<- expr(c2^2)
  #c2 <- expr(sqrt(c22))
  
  ldor     <- expr(c1*y1 + c2*y2)
  se.ldor2 <- expr((!!c11)*v1+(!!c22)*v2)
  se.ldor  <- expr(sqrt(!!se.ldor2))
  
  u.ldor   <- expr(c1*u1 + c2*u2)
  t.ldor   <- expr(!!c11*!!t11 + !!c22*!!t22 + 2*c1*c2*!!t12)
  
  t        <- expr(!!ldor/!!se.ldor)


  
  det.vec <- expr((v1+!!t11)*(v2+!!t22)-(!!t12)^2)
  
  log.det.vec <- expr(log(!!det.vec))
  
  f.l1  <- expr(((y1-u1)^2*(v2+!!t22) - 2*(y2-u2)*(y1-u1)*!!t12 + (y2-u2)^2*(v1+!!t11)) / !!det.vec + !!log.det.vec)
  
  s.l1  <- expr(-0.5*!!f.l1)
  
  
  f.l2 <- expr(pnorm(a + b * !!t))
  
  s.l2 <- expr((log(!!f.l2)))
  
  
  sq <- expr(sqrt(1 + b^2 * (1 + !!t.ldor/!!se.ldor2)))
  
  f.l3<- expr(pnorm( (a + b * !!u.ldor/!!se.ldor) / !!sq ))
  
  s.l3 <- expr(log(!!f.l3))
  
  ## negative loglik
  llk.f <- expr(-(!!s.l1+!!s.l2-!!s.l3))

  
deriv3(llk.f, 
       namevec=c("u1", "u2", "t1", "t2", "r", "b","c1"),
       function.arg=c("u1", "u2", "t1", "t2", "r", "b", "a","c1", "c2", 
                      "y1", "y2", "v1", "v2"), 
       hessian = TRUE)


}

llk.ind <- gen.expr()
