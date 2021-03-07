##******************************************************************************
##
## PRINT SIM TABLE OF THE PARAMETERS
##
##******************************************************************************


library(knitr)
options(knitr.kable.NA = " ")

## CREATE SMALL TABLE ----

make.table <- function(true.n, list.n, row.n){
	
	true <- c(set[[list.n]][row.n, ][c(4:9, 12)])
	#S <- set[[list.n]][row.n, ][c(1)]
	
	load(paste0(dt, "/sim_s",true.n, "_l", list.n, "_r",row.n, ".RData"))
	dim(DATA) <- c(10,4,1000)
	
	B.mean<- 100*(apply(DATA, 1:2, function(x) mean(x, na.rm = TRUE))[1:7,] - true)
	B.mean[is.nan(B.mean)] <- NA
	
	SD <- 100*(apply(DATA, 1:2, function(x) sd(x, na.rm = TRUE))[1:7,])
	
	X <- cbind.data.frame(B.mean, SD)
	colnames(X) <- c(rep(c("BIAS", "SD"), each =4))
	
	X1 <- X[, c(2,6)]
	X2 <- X[, c(3,7)]
	X3 <- X[, c(4,8)]
	
	n <- nrow(X1)

	
	X4 <- cbind(X2, X3, X1)
	
	
	X4[1:5, ]

}


### PRINT TABLE ----

dt <- "mvmeta/DATA1"  #p = 0.7
#dt <- "mvmeta/DATA2"  #p = 0.85

{
	true.n <- 3

load(paste0("scenario/true", true.n, ".RData"))

table1 <- rbind(
	make.table(true.n, 1,1),
	make.table(true.n, 2,1),
	make.table(true.n, 3,1)
)

table2 <- rbind(
	make.table(true.n, 1,2),
	make.table(true.n, 2,2),
	make.table(true.n, 3,2)
)

table3 <- rbind(
	make.table(true.n, 1,3),
	make.table(true.n, 2,3),
	make.table(true.n, 3,3)
)

table4 <- rbind(
	make.table(true.n, 1,4),
	make.table(true.n, 2,4),
	make.table(true.n, 3,4)
)

n <- nrow(table1)/3
S <- c(c(25, rep(NA, n-1), 50, rep(NA, n-1), 200, rep(NA, n-1)),
							c(25, rep(NA, n-1), 50, rep(NA, n-1), 200, rep(NA, n-1)))


TB1 <- cbind(table1, NA, table2) 
TB2 <- cbind(table3, NA, table4)
TB  <- rbind(TB1, TB2)
PAR <- c(rep(c("mu1", "mu2", "tau1", "tau2", "rhoh"), 3), 
									rep(c("mu1", "mu2", "tau1", "tau2", "rhoh"), 3))

TB <- data.frame(S = S, PAR = PAR, TB)

colnames(TB) <- c("N", "PAR", rep(c("BIAS", "SD"), 3), "", rep(c("BIAS", "SD"), 3))

}

TB

kable(TB, format = "latex", row.names = FALSE, booktab = TRUE, digits = 2)



