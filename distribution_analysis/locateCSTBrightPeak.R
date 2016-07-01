
# locateBrightPeak
#  This function locates the brightest peak in a CST file for each of the 7 parameters
#  We assume the data have been biexp transformed.
#	return: a vector of 7 peak locations.
locateBrightPeak <- function (ff, param=colnames(ff)[7:13], xlim=NULL, ylim=NULL, ret.kde=FALSE, show=TRUE) {
  
  # Bright peak is always located above x=3 and next peak is always below
  cut = 3.0
  
  kde <- list()
  mid <- vector('numeric')
  k = 1
  for (p in param) {
    
    h <- bkde (exprs(ff)[,p], grid=2001, bandwidth=.05)
    
    # figure out an adaptive threshold value
    mx <- max (h$y[which(h$x > cut)])
    thresh <- 0.5 * mx
    
    # searching from above, find the midpoint between threshold crossings
    for (i in length(h$y):1) {
      if (h$y[i] < thresh & h$y[i-1] > thresh) {
        hi <- h$x[i]
        break
      }
    }
    for (j in i:1) {
      if (h$y[j] > thresh & h$y[j-1] < thresh) {
        lo <- h$x[j]
        break
      }
    }
    mid[k] <- (hi + lo) / 2
    
    # save results for plotting
    kde[[k]] <- h
    k = k + 1
  }
  
  if (show) {
    
    nparam <- length(param)
    # find a square big enough to hold all of the plots
    for (nacross in 1:10) {
      if (nacross^2 > nparam) {break}
    }
    opar <- par (mfrow=c(nacross,nacross))
    
    for (k in 1:length(kde)) {
      plot (kde[[k]], type='l', main=param[k], xlim=xlim, ylim=ylim)
      xline (mid[k], lty='dotdash', col='red')
    }
    par(opar)
  }
  if (ret.kde) {
    ret = list(mid=mid, kde=kde)
  }
  else {
    mid
  }
  
}
