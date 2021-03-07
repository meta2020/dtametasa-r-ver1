##******************************************************************************
## REPEAT SIMU.FUN 1000 TIMES
## 
## NEED TO CREATE 2 FOLDERS DATA1 AND DATA2 TO SAVE RDATA 
##******************************************************************************

## LOAD LIBRARY

library(mvmeta)
library("foreach")
library("parallel")
library("doSNOW")
library("doRNG")

## LOAD FUNCTIONS

source("simfun/sim.funs.R")
source("simfun/simu.R")
source("simfun/optim1.R")
source("simfun/optim2.R")

## 1000 TIMES SIMULATION 
re <- 1000


## SIMLULATION STARTS, P = 0.7 ----

ncores <- detectCores()
cl <- makeCluster(ncores, "SOCK")
doSNOW::registerDoSNOW(cl)

set.seed(2021)

for(true.n in c(1:3)){  
  
  load(paste0("scenario/true", true.n, ".RData"))
  
  for(list.n in c(1:3)){  ## i list  #Sample Size
    
    for(row.n in c(1:4)){ ## j row   # senarios 
      
      DATA <- foreach(r=1:re, .combine = "cbind", .packages="mvmeta")  %dorng%  {
        simu(list.n, 
            row.n,
            b0 = 0.5,
            c1 = sqrt(0.5),
            b.interval = c(0,2),
            a.interval = c(-3,3),
            r.up = 1,
            p0 = 0.7)
      }
      ## SAVE DATA
      
      save(DATA, file = paste0("DATA1/sim_s",true.n, "_l", list.n, "_r", row.n, ".RData"))
    }
  }

}

parallel::stopCluster(cl)


## SIMLULATION STARTS, P = 0.85 ----

ncores <- detectCores()
cl <- makeCluster(ncores, "SOCK")
doSNOW::registerDoSNOW(cl)


set.seed(2021)

for(true.n in c(1:3)){  ## big b
  
  load(paste0("scenario/true", true.n, ".RData"))
  
  for(list.n in c(1:3)){  ## i list  #Sample Size
    
    for(row.n in c(1:4)){ ## j row   
      
      DATA <- foreach(r=1:re, .combine = "cbind", .packages="mvmeta")  %dorng%  {
        simu(list.n, 
             row.n,
             b0 = 0.5,
             c1 = sqrt(0.5),
             b.interval = c(0,2),
             a.interval = c(-3,3),
             r.up = 1,
             p0 = 0.85)
      }
      ## SAVE DATA
      
      save(DATA, file = paste0("DATA2/sim_s",true.n, "_l", list.n, "_r", row.n, ".RData"))
    }
  }
  
}
parallel::stopCluster(cl)

