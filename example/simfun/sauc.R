##******************************************************************************
##
## SAUC
##
##******************************************************************************


sAUC <- function(par){

  sapply(1:ncol(par), function(i) {

    u1  <- par[1,i]
    u2  <- par[2,i]
    t22 <- par[3,i]
    t12 <- par[4,i]

    if (NA %in% par[,i]) {auc <- NA} else {

      auc.try <- try(integrate(function(x) { plogis(u1 - (t12/t22) * (qlogis(x) + u2)) }, 0, 1))

      if(!inherits(auc.try,"try-error")) auc.try$value else NA

    }

  })

}

