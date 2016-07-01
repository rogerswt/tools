#
#	Find blobs in 2D projection.
#
#  Defaults are reasonable for finding lymphocytes in data from BD Diva instruments.
#
#	The idea is to compute the kernel density estimate of a 2D set (often FSC/SSC), compute
#	a single contour at a specified height, and then select that contour which is closest to a
#	target centroid location.
#
# If log.transform is TRUE, transform the kde before contouring.
#

blob.boundary <- function (ff, parameters=c("FSC-A", "SSC-A"), location=c(75000, 25000), bandwidth=c(.02, .02), gridsize=c(201L, 201L), height=.1, log.transform=FALSE) {
	require ("KernSmooth")
	require ("flowCore")
  
  if (!(is(ff)[[1]]) == "flowFrame") {
		stop ("first argument must be a flowFrame\n")
	}
	
	# extract a matrix of values
	mat <- exprs(ff)[,parameters]
	
	# compute a reasonable bandwith for the kde
	bw1 <- bandwidth[1] * max (mat[,1])
	bw2 <- bandwidth[2] * max (mat[,2])
	# do the kernel density estimate
	kde <- bkde2D (mat, bandwidth=c(bw1, bw2), gridsize=gridsize)
	
	# normalize the density estimate for sanity
	kde$fhat <- kde$fhat / max(kde$fhat)
  
  if (log.transform) {
    epsilon = 1e-4
    kde$fhat = log10(epsilon + kde$fhat)
    # renormalize to [0,1]
    mx = max(kde$fhat)
    mn = min(kde$fhat)
    kde$fhat = (kde$fhat - mn) / (mx - mn)
  }
	
	# compute contour lines at the given height
	cont <- contourLines (kde$x1, kde$x2, kde$fhat, levels=height)
	if (length(cont) == 0) {
		cat ("No contours were found!\n")
		return
	}
	
	# pick the  contour closest to the target location
	dist <- vector("numeric")
	for (i in 1:length(cont)) {
		xcen <- mean(cont[[i]]$x)
		ycen <- mean(cont[[i]]$y)
		dist[i] <- (xcen - location[1])^2 + (ycen - location[2])^2
	}
	nearest <- sort(dist, index.return=T)$ix[1]
	nrows <- length(cont[[nearest]]$x)
	out <- matrix(NA, nrow=nrows, ncol=2)
	out[,1] <- cont[[nearest]]$x
	out[,2] <- cont[[nearest]]$y
	colnames(out) <- parameters
	out
}
