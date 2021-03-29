##******************************************************************************
##
## 1000 TIMES SIMULATION EXPERIMENT
##
## SAVE DATA INTO DIFFERENT FOLDERS IN RES/
##  
##******************************************************************************

library("mvmeta")
library("dtametasa")
library("foreach")
library("parallel")
library("doSNOW")
library("doRNG")

## 1. LOAD ONR-TIME SIMULATION FUNCTION 

# source("simfun/simu-sa-4models.R")
# source("simfun/simu-sa-5models.R")


## 2. PREPARATION

re <- 1000

ncores <- detectCores()
cl <- makeCluster(ncores, "SOCK")
doSNOW::registerDoSNOW(cl)

set.seed(2021)

## 3. LOAD SCENARIOS

# load("scenario/18rows/set-0.5b-all.RData")
# load("scenario/18rows/set-0.5b-all-c10.RData")
# load("scenario/18rows/set-0.5b-all-c01.RData")

  
  for(list.n in c(1:3)){  ## i list  # Sample Size
    
    for(row.n in c(1:8)){ ## j row  # SCENARIO 
      
      DATA <- foreach(r=1:re, .combine = "cbind", .packages = c("mvmeta", "dtametasa"))  %dorng%  {
        
        simu(list.n, 
             row.n,
             b0 = 1,
             csq = 0.5,
             b.interval = c(0,2),
             a.interval = c(-3,3))
      }
      save(DATA, file = paste0("res/DT-pkg-0.5b-all-c01/sim_l", list.n, "_r", row.n, ".RData"))
    }
  }


parallel::stopCluster(cl)


