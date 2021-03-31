##******************************************************************************
##
## 1000 TIMES SIMULATION EXPERIMENT
##
## SAVE DATA INTO DIFFERENT FOLDERS IN RES/
##  
##******************************************************************************

## 1. LOAD FUNCTIONS

library("mvmeta")
library("foreach")
library("parallel")
library("doSNOW")
library("doRNG")

source("simfun/data.pre.R")
source("simfun/sim.pdata.R")
source("simfun/llk.o.R")
source("simfun/sauc.R")
source("simfun/dtametasa.fc.R")
source("simfun/dtametasa.rc.R")



## 2. LOAD ONR-TIME SIMULATION FUNCTION AND SCENARIOS

##******************************************************************************
##
## 4 MODELS COMPARISON: PROPOSED.c, PROPOSED (c1=c2), Reitsma.O, Reistma.P
## TRUE c is c11=c22
##
## RESULTS DATA SAVE INTO 
## FOLDER 1 ----
##
##******************************************************************************

# source("simfun/simu-sa-4models.R")
# load("scenario/18rows/set-0.5b-all.RData")
# folder1 <- "res/DT-pkg-0.5b-all"


## 3. 1000 TIMES REPEAT

re <- 1000

ncores <- detectCores()
cl <- makeCluster(ncores, "SOCK")
doSNOW::registerDoSNOW(cl)

set.seed(2021)

for(list.n in c(1:3)){  ## i list  # Sample Size
  
  for(row.n in c(1:8)){ ## j row  # SCENARIO 
    
    DATA <- foreach(r=1:re, .combine = "cbind", .packages = c("mvmeta"))  %dorng%  {
      
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





##******************************************************************************
##
## 5 MODELS COMPARISON: PROPOSED.c, PROPOSED (c1=1), PROPOSED (c1=c2), Reitsma.O, Reistma.P
## TRUE c is c11=1, c22=0
##
## RESULTS DATA SAVE INTO 
## FOLDER 2 ----
##
##******************************************************************************

# source("simfun/simu-sa-5models.R")
# load("scenario/18rows/set-0.5b-all-c10.RData")
# folder2 <- "res/DT-pkg-0.5b-all-c10"

## 3. 1000 TIMES REPEAT

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
    save(DATA, file = paste0(folder2, "/sim_l", list.n, "_r", row.n, ".RData"))  
  }
}


parallel::stopCluster(cl)



##******************************************************************************
##
## 5 MODELS COMPARISON: PROPOSED.c, PROPOSED (c1=0), PROPOSED (c1=c2), Reitsma.O, Reistma.P
## TRUE c is c11=0, c22=1
##
## RESULTS DATA SAVE INTO 
## FOLDER 3 ----
##
##******************************************************************************

# source("simfun/simu-sa-5models.R")
# load("scenario/18rows/set-0.5b-all-c01.RData")
# folder3 <- "res/DT-pkg-0.5b-all-c01"

## 3. 1000 TIMES REPEAT

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
    save(DATA, file = paste0(folder3, "/sim_l", list.n, "_r", row.n, ".RData"))  
  }
}


parallel::stopCluster(cl)




