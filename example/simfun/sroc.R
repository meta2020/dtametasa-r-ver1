##******************************************************************************
##
## PLOT SROC MATRIX
##
##******************************************************************************



SROC.matrix <- function(par,  ## u1 u2 t12 t22
                 add = FALSE,
                 ncols = NULL,
                 sroc.lty = 1,
                 sroc.lwd = 1,
                 add.spoint=TRUE,
                 p.vec,
                 legend.text = paste0("p = ",p.vec),
                 legend.cex = 1,
                 spoint.pch = 18,
                 spoint.cex = 2,
                 xlab = "1 - specificity",
                 ylab = "Sensitivity",
                 ...) {

  if(length(par) < 4) stop("PLEASE CHECK THE INPUT VECTOR")

  if(length(par) == 4)  par <- as.matrix(par)

  if(nrow(par) < 4) stop("PLEASE CHECK THE INPUT MATRIX")

  if (!add) plot(NULL, xlim=c(0,1), ylim=c(0,1), xlab = xlab, ylab = ylab)

  if (is.null(ncols)) ncols <- gray.colors(ncol(par), gamma = 1, start = 0, end = 0.8)

  for (i in 1:ncol(par)) {

    u1  <- par[1,i]
    u2  <- par[2,i]
    t1  <- par[3,i]
    t2  <- par[4,i]
    r   <- par[5,i]

    roc <- function(x) plogis(u1 - (t1*r/t2) * (qlogis(x) + u2))
    curve(roc, 0, 1, col = ncols[i], add = TRUE,
          lty = sroc.lty[i], lwd = sroc.lwd[i], ...)
  }


  if (add.spoint) {
    sens <- plogis(par[1,])
    spec <- plogis(par[2,])
    points(1-spec, sens, col=ncols, pch = spoint.pch, cex = spoint.cex, ...)
  }

}

