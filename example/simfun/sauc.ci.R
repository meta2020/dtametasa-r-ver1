##******************************************************************************
##
## PARAMETRIC BOOTSTRAP SAUC CI
##
##******************************************************************************


sAUC.ci <- function(object,
                    B = 1000,
                    ncores = 0, 
                    type = "SOCK",
                    ci.level = 0.95,
                    set.seed = NULL,
                    hide.progress = FALSE)
{
  if(!requireNamespace("foreach"))  install.packages("foreach")   else library("foreach")
  if(!requireNamespace("parallel")) install.packages("parallel")  else library("parallel")
  if(!requireNamespace("doSNOW"))   install.packages("doSNOW")    else library("doSNOW")
  if(!requireNamespace("doRNG"))    install.packages("doRNG")     else library("doRNG")

  if(!inherits(object, "dtametasa")) stop("ONLY VALID FOR RESULTS OF dtametasa.fc OR dtametasa.rc")

  data <- object$data

  S  <- nrow(data)
  y1 <- data$y1
  y2 <- data$y2
  v1 <- data$v1
  v2 <- data$v2
  
  u <- object$par[c(1,2)]
  
  SO <- lapply(1:S, function(s){
    
    matrix(c(v1[s], 0, 0, v2[s]),2,2) + matrix(object$par[c(3,5,5,4)], 2,2)
    
  })

  if(ncores == 0) ncores <- parallel::detectCores() else ncores <- ceiling(ncores)

  cl <- makeCluster(ncores, type = type)
  registerDoSNOW(cl)

  opts <- NULL

  if (!hide.progress){

    pb <- txtProgressBar(max = B, style = 3, width = 40)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
  }


  if(object$func.name == "dtametasa.fc"){
    
    set.seed(set.seed)
    par <- foreach(r=1:B, .combine=rbind, .packages = "dtametasa", .options.snow = opts)  %dorng%  {

      # y1.t <- sapply(1:S, function(i) rnorm(1,y1[i],sqrt(v1[i])))
      # y2.t <- sapply(1:S, function(i) rnorm(1,y2[i],sqrt(v2[i])))

      Y <- t(sapply(1:S, function(i) mvtnorm::rmvnorm(1, u, SO[[i]])))
      
      data.t <- data.frame(y1 = Y[,1], y2 = Y[,2], v1 = v1, v2 = v2)

      args <- c(list(data = data.t), object$pars.info)
      sa1 <- try(do.call("dtametasa.fc", args), silent = TRUE)
      if (!inherits(sa1,"try-error")) sa1$par[c(1,2,4,5, 10)] else rep(NA, 5)
      

    }
  }

  if(object$func.name == "dtametasa.rc"){

    SO <- lapply(1:S, function(s){
      
      matrix(c(v1[s], 0, 0, v2[s]),2,2) + matrix(object$par[c(3,5,5,4)], 2,2)
      
    })
    
    set.seed(set.seed)

    par <- foreach(r=1:B, .combine=rbind, .packages = "dtametasa", .options.snow = opts)  %dorng%  {

      # y1.t <- sapply(1:S, function(i) rnorm(1,y1[i], sqrt(v1[i])))
      # y2.t <- sapply(1:S, function(i) rnorm(1,y2[i], sqrt(v2[i])))

      Y <- t(sapply(1:S, function(i) mvtnorm::rmvnorm(1, u, SO[[i]])))
      
      data.t <- data.frame(y1 = Y[,1], y2 = Y[,2], v1 = v1, v2 = v2)
      
      args <- c(list(data = data.t), object$pars.info)
      sa2 <- try(do.call("dtametasa.fc", args), silent = TRUE)
      if (!inherits(sa2,"try-error")) sa1$par[c(1,2,4,5, 10)] else rep(NA, 5)

  }
  }

  if (!hide.progress) close(pb)
  stopCluster(cl)

  PAR <- as.matrix(par)
  sauc.t   <- PAR[, 5]

  n <- length(sauc.t)
  
  var1 <- sqrt((n-1)/n * var(sauc.t, na.rm = TRUE))
  
  se <- sqrt(var1)
  
  sl.sauc.t <- (sauc.t - mean(sauc.t, na.rm = TRUE))/se

  q1 <- quantile(sl.sauc.t, probs = (1-ci.level)/2, na.rm = TRUE)
  q2 <- quantile(sl.sauc.t, probs = 1-(1-ci.level)/2, na.rm = TRUE)

  sauc <- object$par[10]

  list <- list(sauc = sauc,
               #sl.sauc.t = sl.sauc.t,
               ci.l = max(sauc+q1*se, 0),
               ci.u = min(sauc+q2*se, 1))



  class(list) <- "sROC.ci"

  list

}

