#
# Compute spill matrix from Comp tubes
#

# the assumption is that the comp tubes are in the current working directory
# NOTE: ignoring autofluorescence from the unstained tube, for now...

compute.spill <- function (ff, fsc.thresh=30000, path=NULL, show=FALSE) {
  # temporarily change to where the comp tubes are
  if (!is.null(path)) {
    orig.dir <- getwd()
    setwd (path)
  }
  
  # get the fluorescence parameters
  p <- find.fl.parameters(ff)
  # if the parameter name has a "/", change it to ",2f,"
  p2 = sub("/", ",2f,", p, fixed=T)
  # decorate with Diva names
  fn <- paste ("Compensation Controls_", sub("-A", "", p2), sep="")
  fn = match.fl.files (fn)
  n_fl <- length (p)
  spill <- matrix (nrow=n_fl, ncol=n_fl)
  colnames(spill) <- p

  # get rid of "debris", if required
  if (!is.null(fsc.thresh)) {
    dgate = rectangleGate ("FSC-A"=c(fsc.thresh, Inf))
  } else {
    dgate = rectangleGate ("FSC-A"=c(-Inf, Inf))
  }
  
  # set up plot
  if (show) {
    nrow <- n_fl
    ncol <- nrow
    mr = 0.1
    par (mfrow=c(nrow+1, ncol), mar=rep(mr, 4))  
  }
  
  # gate  the unstained tube
#   fn.unst = grep("Compensation Controls_Unstained Control", dir(), value=TRUE, fixed=TRUE)
#   unst <- suppressWarnings (read.FCS (fn.unst))
#   unst = derail (unst, parameters=c("FSC-A", "SSC-A"))
#   unst = Subset (unst, dgate)
#   res = locate.blobs(fc[[1]], param=c("FSC-A", "SSC-A"), log.transform=TRUE, show=TRUE, bandwidth=c(.05, .05))
#   bb = res$contours[[1]]
#   gate <- polygonGate (bb)
  
  # read and gate each tube and compute a column of spill coefficients
  fc = list()
  for (i in 1:n_fl) {
    fc[[i]] = suppressWarnings (read.FCS (fn[i], trans=F))
    fc[[i]] = derail (fc[[i]], parameters=c("FSC-A", "SSC-A"))
    fc[[i]] = Subset (fc[[i]], dgate)
    res = locate.blobs(fc[[i]], param=c("FSC-A", "SSC-A"), log.transform=TRUE, bandwidth=c(.05, .05))
    bb = res$contours[[1]]
    if (show) {
      pplot (fc[[i]], c("FSC-A", "SSC-A"), plotaxt=FALSE)
      lines(bb)
    }
    gate <- polygonGate (bb) 
    fc[[i]] <- Subset (fc[[i]], gate)
  }
  
  # compute coefficients
  for (i in 1:n_fl) {
    p1 <- p[i]
    rows <- 1:n_fl
    spill[i,i] <- 1.0
    for (j in rows) {
      if (j == i) {
        if (show) {
          plot.empty.diag ()
        }
        next
      }
      p2 <- p[j]
      res <- spill.slope (fc[[i]], p1, p2, show=show)
      if (res < 0.0) {res = 0.0}
      spill[i,j] <- res
    }
  }
    
  # change dir back to orig.dir if necessary
  if (!is.null (path)) {
    setwd (orig.dir)
  }
    
  # restore par
  par (mfrow=c(1,1), mar=c(5,4,4,1))
  
  spill
}

match.fl.files <- function (f.pre) {
  fvec = dir()
  f.out = vector('character')
  for (i in 1:length(f.pre)) {
    f.out[i] = grep(f.pre[i], fvec, value=TRUE, fixed=TRUE)
  }
  f.out
}

find.fl.parameters <- function (ff) {
  # eliminate scattering and Time
  p <- colnames(ff)
  fp <- vector('character')
  k = 1
  tst <- c("FSC", "SSC", "Time")
  for (i in 1:length(p)) {
    found = FALSE
    for (j in 1:length(tst)) {
      if (length(grep(tst[j], p[i])) != 0) {
        found = TRUE
        break
      }
    }
      if (!found) {
        fp[k] = p[i]
        k = k + 1
        next
      }
  } 
  fp
}

spill.slope <- function (ff, p1, p2, rail.val=262143, dim.thresh=1000, show=FALSE) {
  x <- exprs(ff)[,p1]
  y <- exprs(ff)[,p2]
  
  # eliminate rail events
  ind <- which (x < rail.val)
  x <- x[ind]
  y <- y[ind]
  
  all.x = x
  all.y = y
  
  # eliminate events that are very dim on the primary channel
  ind <- which (x > dim.thresh)
  x <- x[ind]
  y <- y[ind]
  
  all.x = x
  all.y = y
  
  mod <- lm (y ~ x)
  
  # eliminate points with residuals more than 3 sd from fit, then re-compute the fit
  dist = 3*sd(mod$residuals)
  ind = which(abs(mod$residuals) > dist)
  
  x = x[-ind]
  y = y[-ind]
  

  fail = FALSE  
  if (length(x) != 0) {
    mod <- lm (y ~ x)
    val = mod$coefficients[2]
    # if the spread of y values exceeds that of x values, throw up our hands and give up
    if (sd(y) > 2*sd(x)) {
      fail=TRUE
      val = 0
    }
  } else {
    val = 0
  }
  
  if (show) {
    plot (all.x, all.y, pch=20, cex=.3, col='blue', xlim=c(0,max(all.x)), ylim=c(0,max(all.x)), xaxt='n', yaxt='n')
    if (!fail) {
      if (length(x) != 0) {
        points (x, y, pch=20, cex=.3, col='black')
        i1 <- which (x == min(x))
        i2 <- which (x == max(x))
        
        lines (x[c(i1,i2)], predict(mod)[c(i1,i2)], col='red')
      }
      
      loc <- .95 * max(all.x)
      text (0,loc, labels=paste(p1, "to"), pos=4)
      text (0, .75*loc, labels=p2, pos=4)
      text (0, .5*loc, sprintf("Slope = %.2e", val), pos=4)
    }
  }
  # return the slope of the fit
  val
}

plot.empty.diag = function () {
  plot (0, 0, pch='', xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n')
}
