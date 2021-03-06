##******************************************************************************
##
## 1000 TIMES SIMULATION EXPERIMENT
##
## SAVE DATA INTO DIFFERENT FOLDERS IN res-par/t12 or res-par/t0.7
##
##  
##******************************************************************************
rm(list = ls())

## 1. LOAD FUNCTIONS

library("mvmeta")
library("foreach")
library("parallel")
library("doSNOW")
library("doRNG")

files.sources <- list.files("simfun/")
sapply(paste0("simfun/", files.sources), source)





## 2. LOAD ONR-TIME SIMULATION FUNCTION AND SCENARIOS

source("simfun/simu-sa-5models-par.R")

## CHOOSE ONE OF OF THE SCENARIOS

# tset <- "t12"
# tset <- "t0.7"

## 3. 1000 TIMES REPEAT

re <- 1000


##******************************************************************************
##
## 5 MODELS COMPARISON: PROPOSED.c, PROPOSED (c1=c2), PROPOSED (c1=1), Reitsma.O, Reistma.P
## TRUE c: c11=c22
##
## RESULTS DATA SAVE INTO /c11/
## FOLDER 1 ----
##
##******************************************************************************

ncores <- detectCores()
cl <- makeCluster(ncores, "SOCK")
doSNOW::registerDoSNOW(cl)



t <- proc.time()


load(paste0("scenario/scenario-",tset,"/set-", tset, "-c11.RData"))

folder1 <- paste0("res-par/",tset,"/c11/")


set.seed(2021)

for(list.n in c(1:3)){  ## i list  # Sample Size
  
  for(row.n in c(1:6)){ ## j row  # SCENARIO 
    
    DATA <- foreach(r=1:re, .combine = "cbind", .packages = c("mvmeta"))  %dorng%  {

      simu(list.n, 
           row.n,
           b0 = 1,
           csq0 = 0.5,   ## SA2: INITIAL VALUE OF c11
           csq.w = 1,    ## SA1: MIS-SPECIFIED VALUE OF c11
           b.interval = c(0,2),
           a.interval = c(-3,3)
           )
    }
    save(DATA, file = paste0(folder1, "/sim_l", list.n, "_r", row.n, ".RData"))  
  }
}


# parallel::stopCluster(cl)



##******************************************************************************
##
## 5 MODELS COMPARISON: PROPOSED.c, PROPOSED (c1=1), PROPOSED (c1=c2), Reitsma.O, Reistma.P
## TRUE c: c11=1, c22=0
##
## RESULTS DATA SAVE INTO /c10/
## FOLDER 2 ----
##
##******************************************************************************

load(paste0("scenario/scenario-",tset,"/set-", tset, "-c10.RData"))

folder2 <- paste0("res-par/",tset,"/c10/")


# ncores <- detectCores()
# cl <- makeCluster(ncores, "SOCK")
# doSNOW::registerDoSNOW(cl)
# 
set.seed(2021)

for(list.n in c(1:3)){  ## i list  # Sample Size
  
  for(row.n in c(1:6)){ ## j row  # SCENARIO 
    
    DATA <- foreach(r=1:re, .combine = "cbind", .packages = c("mvmeta"))  %dorng%  {
      
      simu(list.n, 
           row.n,
           b0 = 1,
           csq0 = 0.5,   ## SA2: INITIAL VALUE OF c11
           csq.w = 0.5,  ## SA1: MIS-SPECIFIED VALUE OF c11
           b.interval = c(0,2),
           a.interval = c(-3,3))
    }
    save(DATA, file = paste0(folder2, "/sim_l", list.n, "_r", row.n, ".RData"))  
  }
}


# parallel::stopCluster(cl)



##******************************************************************************
##
## 5 MODELS COMPARISON: PROPOSED.c, PROPOSED (c1=0), PROPOSED (c1=c2), Reitsma.O, Reistma.P
## TRUE c: c11=0, c22=1
##
## RESULTS DATA SAVE INTO /c01/
## FOLDER 3 ----
##
##******************************************************************************

load(paste0("scenario/scenario-",tset,"/set-", tset, "-c01.RData"))

folder3 <- paste0("res-par/",tset,"/c01/")


# ncores <- detectCores()
# cl <- makeCluster(ncores, "SOCK")
# doSNOW::registerDoSNOW(cl)
# 
set.seed(2021)

for(list.n in c(1:3)){  ## i list  # Sample Size
  
  for(row.n in c(1:6)){ ## j row  # SCENARIO 
    
    DATA <- foreach(r=1:re, .combine = "cbind", .packages = c("mvmeta"))  %dorng%  {
      
      simu(list.n, 
           row.n,
           b0 = 1,
           csq0 = 0.5,   ## SA2: INITIAL VALUE OF c11
           csq.w = 0.5,  ## SA1: MIS-SPECIFIED VALUE OF c11
           b.interval = c(0,2),
           a.interval = c(-3,3))
    }
    save(DATA, file = paste0(folder3, "/sim_l", list.n, "_r", row.n, ".RData"))  
  }
}


parallel::stopCluster(cl)



proc.time()-t
