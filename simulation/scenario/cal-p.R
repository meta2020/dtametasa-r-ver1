##******************************************************************************
## SIMULATION EXPERIMENT BASED ON POPULATION DATA ONLY
##
## AIM: TO CALCULATE ALPHA AND P
##  
##******************************************************************************


library("foreach")
library("parallel")
library("doSNOW")
library("doRNG")
source("../simfun/sim.pdata.R")

# sETTINGS ----

re <- 1000


# CALCULATE ALPHAS ----

## LOAD SCENARIOS,

# load("18rows/set-0.5b-all.RData")
# load("18rows/set-0.5b-all-c10.RData")
# load("18rows/set-0.5b-all-c01.RData")

##******************************************************************************
##
##  FUNCTION TO CALCULATE ALPHAS, GIVEN P ----
##
##******************************************************************************

b  <- set[[1]][1,9]


fa <- function(a) mean(pnorm(a + b*pdata$t.clnDOR)) - 0.7

a <- NULL


ncores <- detectCores()
cl <- makeCluster(ncores, "SOCK")
doSNOW::registerDoSNOW(cl)

set.seed(2021)

for(i in 1:18){
  
	a1 <- foreach(r=1:re, .combine = "c")  %dorng%  {
		
		pdata <- dtametasa::sim.pdata(set[[3]][i,1:8])
		
		uniroot(fa, c(-3,3), extendInt="yes")$root
	}

	a <- c(a, mean(a1))	
	
}


parallel::stopCluster(cl)

round(a,6)


##******************************************************************************
##
## FUNCTION TO CHECK P, GIVEN ALPHA ----
##
##******************************************************************************


ncores <- detectCores()
cl <- makeCluster(ncores, "SOCK")
doSNOW::registerDoSNOW(cl)

set.seed(2021)

p <- NULL

for(i in 1:18){
  
  p1 <- foreach(r=1:re, .combine = "c")  %dorng%  {
    
    pdata <- dtametasa::sim.pdata(set[[3]][i,1:8])
    
    mean(pnorm(set[[3]][i,10] + set[[3]][i,9]*pdata$t.clnDOR))
    
  }
  
  p <- c(p, mean(p1))	
  
}


parallel::stopCluster(cl)

p
