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



## 1. LOAD ONR-TIME SIMULATION FUNCTION AND SCENARIOS


# source("simfun/simu-sa-4models.R")

# load("scenario/18rows/set-0.5b-all.RData")
# folder1 <- "res/DT-pkg-0.5b-all"


# source("simfun/simu-sa-5models.R")

# load("scenario/18rows/set-0.5b-all-c10.RData")
# folder2 <- "res/DT-pkg-0.5b-all-c10"

# load("scenario/18rows/set-0.5b-all-c01.RData")
# folder3 <- "res/DT-pkg-0.5b-all-c01"


## 2. 1000 TIMES REPEAT

re <- 1000

ncores <- detectCores()
cl <- makeCluster(ncores, "SOCK")
doSNOW::registerDoSNOW(cl)

set.seed(2021)


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
      save(DATA, file = paste0(folder1, "/sim_l", list.n, "_r", row.n, ".RData"))  
    }
  }


parallel::stopCluster(cl)


