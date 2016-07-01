
#
# Use ideas from image analysis to find blobs in bivariates
#
watershed_boundary <- function (ff, parameters=c("FSC-A", "SSC-A"), 
                                region = list(x=c(60000,262142), y=c(-Inf,262142)),
                                location=list(c(100000, 40000), c(165000, 90000)), bandwidth=c(.01, .01), gridsize=c(501L, 501L),
                                tolerance = .01, smooth=FALSE, debug=FALSE) {
  require ("KernSmooth")
  require ("flowCore")
  require ("EBImage")
  
  if (!is(ff) == "flowFrame") {
    cat ("first argument must be a flowFrame\n")
    return
  }
  
  # extract a loose subregion fully containing the blob(s) of interest
  if (debug) {cat ("\nselecting region...")}
  rect_mat <- matrix(unlist(region), ncol=2, dimnames=list(c("min", "max"), parameters))
  fg <- Subset (ff, rectangleGate(filterId="region", .gate=rect_mat))
  
  # extract a matrix of values
  mat <- exprs(fg)[,parameters]
  
  # compute a reasonable bandwith for the kde
  bw1 <- bandwidth[1] * max (mat[,1])
  bw2 <- bandwidth[2] * max (mat[,2])
  # do the kernel density estimate
  if (debug) {cat ("\ncomputing the KDE...")}
  kde <- bkde2D (mat, bandwidth=c(bw1, bw2), gridsize=gridsize)
  
  img <- kde[[3]] / max(kde[[3]])
  
  if (smooth) {
    smooth_kernel <- makeBrush (31, shape='disc')
    img <- filter2 (img, filter=smooth_kernel)
    img <- img / max(img)
  }
  
  eimg <- equalize(img)
  
  # threshold at the median value of the equalized image
  # threshold <- median(eimg)
  threshold = .7
  timg <- eimg > threshold
  
  # dilate to expand the region a bit
  dilate_kernel <- makeBrush(31, shape='disc')
  timg <- dilate (timg, kern=dilate_kernel)
  
  # mask the image with the thresholded image
  mimg <- img * timg
  
  # do the watershed segmentation
  if (debug) {cat ("\ndoing the watershed segmentation...")}
  ws <- watershed (mimg, tolerance=tolerance)
  
  # extract contours of blobs
  nblob <- max(ws)
  cont <- ocontour(ws)
  
  # convert pixels to object coordinates
  poly <- list()
  centroid <- list()
  for (i in 1:nblob) {
    poly[[i]] <- cbind(kde[[1]][cont[[i]][,1]+1], kde[[2]][cont[[i]][,2]+1])
    colnames(poly[[i]]) <- parameters
    xcen <- mean(poly[[i]][,1])
    ycen <- mean(poly[[i]][,2])
    centroid[[i]] <- c(xcen, ycen)
  }
  
  # extract the boundary of the blob closest to specified location(s)
  if (debug) {cat ("\npicking target blob(s)...")}
  sel <- list()
  for (j in 1:length(location)) {
    min_dist <- Inf
    for (i in 1:nblob) {
      dx <- location[[j]][1] - centroid[[i]][1]
      dy <- location[[j]][2] - centroid[[i]][2]
      dist <- dx*dx + dy*dy
      if (dist < min_dist) {
        min_dist <- dist
        sel[[j]] <- i
      }
    }
  }
  
  if (debug) {
    plot (ff, parameters, nbin=201, colramp=blob_color())
    hicol <- c('red', 'green', 'magenta', 'white')
    j=1
    for (i in 1:nblob) {
      lines (poly[[i]])
    }
    for (i in 1:length(sel)) {
      lines(poly[[sel[[i]]]], lwd=2, col=hicol[i])
    }
    npts <- 0
    if (nrow(ff) > npts) {
      pts <- sample(1:nrow(ff), size=npts)
    } else {
      pts <- 1:nrow(ff)
    }
    points (exprs(ff)[pts,parameters], pch='.')
  }
  if (debug) {cat ("\ndone\n")}
  
  # construct the return val
  if (length(sel) > 1) {
    retlist <- list()
    for (i in 1:length(sel)) {
      retlist[[i]] <- poly[[sel[[i]]]]
    }
    return (retlist)
  } else {
    return (poly[[sel[[1]]]])
  }
}
