##******************************************************************************
##
## MEDIAN PLOT OF THE SAUC BASED NO 1000 SIMULATION ----
##
##******************************************************************************


library(lattice)
library(reshape2)

### PLOT FUNCTION ----

latt.sauc <- function(dt){
auc <- NULL

for (true.n in 1:3){
	
for (list.n in 1:3){
	
for(row.n in 1:4){
	
	load(paste0(dt, "/sim_s",true.n, "_l", list.n, "_r",row.n, ".RData")) ## MAKE SURE THE SUCCESS LOAD OF DATA
	dim(DATA) <- c(10,4,1000)
	auc.med <- apply(DATA[9,,], 1, function(x) median(x, na.rm = TRUE))
	auc  <- rbind(auc, auc.med)
	
}
}
}

## LOAD TRUE SAUC

load("scenario/true1.RData")
sauc1 <- set[[1]][,13]
load("scenario/true2.RData")
sauc2 <- set[[1]][,13]
load("scenario/true3.RData")
sauc3 <- set[[1]][,13]

auc[,1] <- c(rep(sauc1, 3), rep(sauc2, 3), rep(sauc3, 3))  #replace X1 with true sauc


true.n  <- rep(factor(c("s1", "s2", "s3"),
																					levels = c("s3", "s2", "s1"),
																					labels = c("Scenario 9-12", 
																																"Scenario 5-8", 
																																"Scenario 1-4")), 
														each = 12)
list.n <- rep(rep(factor(c("S = 25", "S = 50", "S = 200"), 
																					levels = c("S = 25", "S = 50", "S = 200")), 
														each = 4), 3)
scen   <- rep(rep(factor(c(1:4), levels = c(1:4)), 
														3),3)



X <- data.frame(auc, scen, list.n, true.n) #X1 X2 X3 X4, where X1 is est from pop data


X$id <- factor(1:nrow(X))
X.long <- melt(X, id.vars=c("id", "scen", "list.n", "true.n"),
																value.name="sAUC")
X.long$grp <- factor(X.long$variable, labels = c("True", "Bivariate RE", "SA 1", "SA 2"))


xyplot(sAUC ~ scen | list.n + true.n, 
							type="p",
							data=X.long,  
							groups=grp,
							ylab = "Median sAUC",
							xlab = "",
							tick.number = 4,
							#layout = c(3,3),
							pch = c(20,1,0,5),
							col = 1,
							cex = 1.1,
							strip=strip.custom(bg=gray(.8)),
							scales = list(alternating=1, 
																					relation="free",
																					y=list(tick.number=4),
																					x=list(at=NULL)),
							auto.key=TRUE,
							key = list(space="bottom",
																		col="black", 
																		columns = 4, 
																		between = 0.5, 
																		text=list(levels(X.long$grp)), 
																		points=list(pch=c(20,1,0,5), type="p")))

}


### SAVE PLOT ----


dt <- "DATA1"

setEPS(width = 10, height = 8)
postscript("sauc1.eps")

latt.sauc(dt)

dev.off()


dt <- "DATA2"

setEPS(width = 10, height = 8)
postscript("sauc2.eps")

latt.sauc(dt)

dev.off()
