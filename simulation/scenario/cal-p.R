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


## pdata FUNCTION ----

#source("../simfun/fun-sim.pdata.R")


# sETTINGS ----

re <- 1000


# CALCULATE ALPHAS ----

#load("scenarioset-0.5b-c.RData")

##******************************************************************************
##
##  FUNCTION TO CALCULATE ALPHAS, GIVEN P ----
##
##******************************************************************************

b  <- set[[1]][1,9]
#b  <- 1.5
#b  <- 100


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

for(i in 1:8){
  
  p1 <- foreach(r=1:re, .combine = "c")  %dorng%  {
    
    pdata <- dtametasa::sim.pdata(set[[1]][i,1:8])
    
    mean(pnorm(set[[1]][i,10] + set[[1]][i,9]*pdata$t.clnDOR))
    
  }
  
  p <- c(p, mean(p1))	
  
}


parallel::stopCluster(cl)


