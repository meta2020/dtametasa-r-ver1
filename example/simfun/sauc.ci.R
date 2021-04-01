##******************************************************************************
##
## PARAMETRIC BOOTSTRAP SAUC CI
##
##******************************************************************************


sAUC.ci <- function(object,
                    B = 1000,
                    ncores = 0, type = "SOCK",
                    ci.level = 0.95,
                    set.seed = NULL,
                    hide.progress = FALSE)
{
  if(!requireNamespace("foreach"))  install.packages("foreach")   else requireNamespace("foreach")
  if(!requireNamespace("parallel")) install.packages("parallel")  else requireNamespace("parallel")
  if(!requireNamespace("doSNOW"))   install.packages("doSNOW")    else requireNamespace("doSNOW")
  if(!requireNamespace("doRNG"))    install.packages("doRNG")     else requireNamespace("doRNG")

  if(!inherits(object, "dtametasa")) stop("ONLY VALID FOR RESULTS OF dtametasa.fc OR dtametasa.rc")

  data <- object$data

  S  <- nrow(data)
  y1 <- data$y1
  y2 <- data$y2
  v1 <- data$v1
  v2 <- data$v2

  if(ncores == 0) ncores <- detectCores() else ncores <- ceiling(ncores)

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

      y1.t <- sapply(1:S, function(i) rnorm(1,y1[i],sqrt(v1[i])))
      y2.t <- sapply(1:S, function(i) rnorm(1,y2[i],sqrt(v2[i])))

      data.t <- data.frame(y1 = y1.t, y2 = y2.t, v1 = v1, v2 = v2)

      args <- c(list(data = data.t), object$pars.info)
      sa1 <- do.call("dtametasa.fc", args)
      sa1$par[c(1,2,4,5, 10)]

    }
  }

  if(object$func.name == "dtametasa.rc"){

    set.seed(set.seed)

    par <- foreach(r=1:B, .combine=rbind, .packages = "dtametasa", .options.snow = opts)  %dorng%  {

      y1.t <- sapply(1:S, function(i) rnorm(1,y1[i], sqrt(v1[i])))
      y2.t <- sapply(1:S, function(i) rnorm(1,y2[i], sqrt(v2[i])))

      data.t <- data.frame(y1 = y1.t, y2 = y2.t, v1 = v1, v2 = v2)
      args <- c(list(data = data.t), object$pars.info)
      opt2.t <- do.call("dtametasa.rc", args)
      opt2.t$par[c(1,2,4,5, 10)]

  }
  }

  if (!hide.progress) close(pb)
  stopCluster(cl)

  PAR <- as.matrix(par)
  sauc.t   <- PAR[, 5]

  n <- length(sauc.t)
  se <- (n-1)/n * sd(sauc.t, na.rm = TRUE)
  sl.sauc.t <- (sauc.t - mean(sauc.t, na.rm = TRUE))/se

  s.sauc.r <- order(sl.sauc.t)
  #PAR.r <- PAR[, s.sauc.r]

  s.sauc.t <- sort(sl.sauc.t)

  q1 <- quantile(s.sauc.t, probs = (1-ci.level)/2, na.rm = TRUE)
  q2 <- quantile(s.sauc.t, probs = 1-(1-ci.level)/2, na.rm = TRUE)

  sauc <- object$par[10]

  list <- list(sauc = sauc,
       ci.l = max(sauc+q1*se, 0),
       ci.u = min(sauc+q2*se, 1),
       bootstrap.par  = PAR,
       cluster = cl)


  class(list) <- "sROC.ci"

  list

}

