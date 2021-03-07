##******************************************************************************
##
## TABLE OF sAUC MEDIAN, MEAN, SD and CONVERGENCE RATE ----
##
##******************************************************************************



## CREATE SMALL TABLE FUNCTION ----

make.table.auc <- function(true.n, list.n){
	
	m.auc <- e.auc<- sd.auc<- conv<-  NULL
	
	for(row.n in 1:nrow(set[[1]])){
		
		load(paste0(dt, "/sim_s",true.n, "_l", list.n, "_r",row.n, ".RData"))
		dim(DATA) <- c(10,4,1000)
		
		m.auc  <- rbind(m.auc, apply(DATA[9,2:4,], 1, function(x) median(x, na.rm = TRUE)))
		e.auc  <- rbind(e.auc, apply(DATA[9, 2:4,], 1, function(x) mean(x, na.rm = TRUE)))
		sd.auc <- rbind(sd.auc, apply(DATA[9,2:4,], 1, function(x) sd(x, na.rm = TRUE)))
		conv  <- rbind(conv, apply(DATA[8,2:4,], 1, function(x) sum(is.na(x))))
		
	}
	
	
	load(paste0("scenario/true", true.n, ".RData"))
	
	true <- set[[list.n]][,13]
	sauc <- cbind(m.auc, e.auc-true, sd.auc)
	
	conv2 <- 1-conv[, c(2,3,1)]/1000

	sauc3 <- cbind(sauc[, c(1, 4, 7)], conv2[,1])
	sauc1 <- cbind(sauc[, c(2, 5, 8)], conv2[,2])
	sauc2 <- cbind(sauc[, c(3, 6, 9)], conv2[,3])
	
	

	X <- cbind(sauc1, NA, sauc2, NA, sauc3)
	
		
	dimnames(X) <- list(
		rep(c("sAUC"), 4),
		c("MED.BIAS", "BIAS", "SD", "CR", "",
				"MED.BIAS", "BIAS", "SD", "CR", "", 
				"MED.BIAS", "BIAS", "SD", "CR")
	)

	X*100

}


### PRINT TABLE ----

library(knitr)
options(knitr.kable.NA = " ")


s <- c(25, rep(NA, 3), 50, rep(NA, 3), 200, rep(NA, 3))

true.n <- 1 

dt <- "DATA1"  #p = 0.7

table1 <- 	rbind(
	make.table.auc(true.n, 1),
	make.table.auc(true.n, 2),
	make.table.auc(true.n, 3)
	)

tb1 <- cbind.data.frame(S = s, table1)

dt <- "DATA2"  #p = 0.7

table2 <-	rbind(
	make.table.auc(true.n, 1),
	make.table.auc(true.n, 2),
	make.table.auc(true.n, 3)
)

tb2 <- cbind.data.frame(S = s, table2)
(tb <- rbind(tb1, tb2))


kable(tb, "latex", row.names = FALSE, booktab = TRUE, digits = 2)
