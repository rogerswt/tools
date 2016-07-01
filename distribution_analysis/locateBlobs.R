#
#  locate.blobs
#
locate.blobs = function (ff, param=c("FSC-A", "SSC-A"), eps=.01, max_peaks=10, min_level=5*eps, nbin=501, bandwidth=0.02, log.transform=FALSE, show=FALSE) {
  require ("KernSmooth")
  require ("flowCore")
  if (!(is(ff)[[1]]) == "flowFrame") {
    stop ("first argument must be a flowFrame\n")
  }
  kde = ff2kde (ff, param, nbin=nbin, bandwidth=bandwidth, log.transform=log.transform)
  
  # search from the top down
  heights = seq(1-eps, min_level, by=-eps)
  centers = matrix (nrow=0, ncol=2)
  colnames(centers) = param
  contours = list()
  levels = vector('numeric')
  nfound = 0
  for (height in heights) {
    
    cont <- contourLines (kde$x1, kde$x2, kde$fhat, levels=height)
    cont = close.contour (cont)
    
    # loop over the contour lines to detect new centers
    for (c in 1:length(cont)) {
      found = FALSE
      cmat =  cont2mat(cont[[c]], param)
      if (nrow(centers) > 0) {
        for (p in 1:nrow(centers)) {
          if (inside (centers[p,], cmat)) {
            found = TRUE
            break
          }
        }
      }
      if (!found) {
        centers = rbind (centers, centroid (cont2mat(cont[[c]], param)))
        nfound = nfound + 1
        contours[[nfound]] = cont[[c]]
      }
    }
    
    # loop over the contour lines to update the blob contours
    for (c in 1:length(cont)) {
      enclosed = 0
      cmat = cont2mat(cont[[c]], param)
      for (p in 1:nrow(centers)) {
        if (inside (centers[p,], cmat)) {
          enclosed = enclosed + 1
          which = p
        }
      }
      # update the contour if it encloses exactly one center
      if (enclosed == 1) {
        contours[[which]] = cont2mat (cont[[c]], param)
        levels[which] = cont[[c]]$level
      }
    }
  }
  
  if (show) {
    pplot (ff, param, instrument='diva', tx='biexp', ty='biexp')
    for (i in 1:length(contours)) {
      text (centers[i,1], centers[i,2], label=paste(i))
      lines (contours[[i]])
    }
  }
  
  return (list (centers=centers, contours=contours, levels=levels))
}

ff2kde = function (ff, param=c("FSC-A", "SSC-A"), nbin=501, bandwidth=0.02, log.transform=FALSE) {
  
  # extract a matrix of values
  mat <- exprs(ff)[,param]
  
  # compute a reasonable bandwith for the kde
  bw1 <- bandwidth * max (mat[,1])
  bw2 <- bandwidth * max (mat[,2])
  # do the kernel density estimate
  kde <- bkde2D (mat, bandwidth=c(bw1, bw2), gridsize=c(nbin,nbin))
  
  if (log.transform) {
    epsilon = 1e-4
    kde$fhat = log10(epsilon + kde$fhat)
    # renormalize to [0,1]
    mx = max(kde$fhat)
    mn = min(kde$fhat)
    kde$fhat = (kde$fhat - mn) / (mx - mn)
  }
  # normalize the density estimate for sanity
  kde$fhat <- kde$fhat / max(kde$fhat)
  
  kde
}

# if contour isn't closed, close it
#  ASSUMPTION: there is only one contour on the list resulting from contourLines()
close.contour = function (cont) {
  x = cont[[1]]$x
  y = cont[[1]]$y
  npts = length(x)
  if ((x[1] != x[npts]) | (y[1] != y[npts]) ) {
    # close it!
    x[npts+1] = x[1]
    y[npts+1] = y[1]
  }
  newcont = cont
  newcont[[1]]$x = x
  newcont[[1]]$y = y
  
  newcont
}




