
#
# This function finds the local maxima on a kernel density estimate
#
find.local.maxima <- function (kde, thresh=.05, show=FALSE, span = 11) {
  require ("wmtsa")    # for peaks
  
  ind <- msExtrema(kde$y, span=span)$index.max
  
  max_y <- max (kde$y)
  sel <- kde$y / max_y > thresh
  
  ind <- which (ind & sel)
  
  if (show) {
    plot (kde, type='l', xlab="", ylab="", xaxt='n')
    points (kde$x[ind], kde$y[ind], pch=20, cex=.5, col='red')
  }
  
  return (list(x=kde$x[ind], y=kde$y[ind], pick=ind))
}

#
# This function finds the local minima on a kernel density estimate
#
find.local.minima <- function (kde, thresh=.05, offset=5000, show=FALSE, ...) {
  require ("wmtsa")    # for peaks
  
  ind <- msExtrema(kde$y, span=11)$index.min
  
  max_y <- max (kde$y)
  sel <- kde$y / max_y > thresh
  
  pick <- which (ind & sel)
  
  # apply offset
  #tmp <- max (which(kde$x < (kde$x[pick] - offset)))
  #pick <- which (kde$x == kde$x[tmp])
  
  if (show) {
    par(...)
    plot (kde, type='l', xlab="", ylab="", xaxt='n')
    points (kde$x[pick], kde$y[pick], pch=20, cex=.5, col='red')
  }
  
  return (list(x=kde$x[pick], y=kde$y[pick], pick=pick))
}

# helper function, not to be exposed
msExtrema <- function(x, span=3)
{
  # find local maxima
  index1 <- peaks(x, span=span, strict=FALSE)
  
  # find local minima
  index2 <- peaks(-x, span=span, strict=FALSE)
  
  # remove the interior of plateaus
  index.max <- index1 & !index2
  index.min <- index2 & !index1
  
  # construct output
  list(index.max=index.max, index.min=index.min)
}


# find the full width half-maximum (or other width at percent maximum)
find.width = function (kde, frac=0.5) {
  x = kde$x
  y = kde$y
  mx = max(kde$y)
  
  for (i in 1:length(y)) {
    if (y[i]/mx > frac) {break}
  }
  left = x[i]
  for (i in length(y):1) {
    if (y[i]/mx > frac) {break}
  }
  right = x[i]
  center = left + (right - left)/2
  
  return (list(center=center, left=left, right=right, frac=frac))
}

cumm.kde = function (kde) {
  n = length(kde$y)
  cval = vector('numeric', length=n)
  cval[1] = kde$y[1]
  for (i in 2:n) {
    cval[i] = cval[i-1] + kde$y[i]
  }
  cval = cval / cval[n]
  
  return(list(x=kde$x, y=cval))
}

# estimate the first derivative using the symmetric difference quotient 
deriv.kde = function (kde, normalize=TRUE) {
  x = kde$x
  y = kde$y
  npts = length(y)
  yp = vector('numeric')
  for (i in 2:(npts-1)) {
    yp[i] = (y[i+1] - y[i-1]) / (x[i+1] - x[i-1])
  }
  yp[1] = yp[2]
  yp[npts] = yp[npts-1]
  
  if (normalize) {
    yp = yp / max (abs(yp))
  }
  res = list(x=x, y=yp)
  res
}