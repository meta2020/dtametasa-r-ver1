##******************************************************************************
##
## TRUE PARAMETERS IN THE SIMULATION DATA
##
##******************************************************************************

setwd("~/Documents/meeting/20210323/sim/scenario")

##
## P=0.7, b=0.5 ----
##

load("18rows/set-0.5b-all.RData")

set <- lapply(1:3, function(i) set[[i]][c(1,2,4,5,9,11,17,18),])

save(set, file = "set-0.5b.RData")



##
## P=0.7, b=1.5 ----
##

load("18rows/set-1.5b-all.RData")

set <- lapply(1:3, function(i) set[[i]][c(1,2,4,5,9,11,17,18),])

save(set, file = "set-1.5b.RData")


##
## P=0.7, b=0.5 c11 = 1 ----
##


load("18rows/set-0.5b-all-c10.RData")

set <- lapply(1:3, function(i) set[[i]][c(1,4,11,17),])

save(set, file = "set-0.5b-c10.RData")

##
## P=0.7, b=0.5 c11 = 0.1 ----
##

load("18rows/set-0.5b-all-c19.RData")

set <- lapply(1:3, function(i) set[[i]][c(1,4,11,17),])

save(set, file = "set-0.5b-c19.RData")


