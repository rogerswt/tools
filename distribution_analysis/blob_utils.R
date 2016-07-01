
# draw a circle around the center of a blob
circle <- function (blob, radius, length=100) {
  cen1 <- mean(blob[,1])
  cen2 <- mean(blob[,2])
  
  out <- matrix(nrow=(length+1), ncol=2)
  colnames(out) <- colnames(blob)
  
  for (i in 1:length) {
    theta <- 2*pi*i/length
    out[i,1] <- cen1 + radius * cos(theta)
    out[i,2] <- cen2 + radius * sin(theta)
  }
  out[length+1,] <- out[1,]	# join the beginning and end (aesthetics)
  out
}

close.contour = function (blob) {
  x = blob[,1]
  y = blob[,2]
  npts = nrow(blob)
  if ((x[1] != x[npts]) | (y[1] != y[npts])) {
    blob = rbind(blob, c(x[1], y[1]))
  }
  blob
}

get.hull <- function (blob) {
  x <- blob[,1]
  y <- blob[,2]
  hull <- chull(x, y)
  poly <- matrix(c(x[hull], y[hull]), nrow=length(hull), ncol=2)
  # get rid of dupes
  poly <- unique(poly)
  # close the contour
  poly <- rbind(poly, poly[1,])
  colnames(poly) <- colnames(blob)
  return(poly)	
}

inflate.contour <- function (blob, dist) {
  require("splancs")
  x <- blob[,1]
  y <- blob[,2]
  len <- length(x)
  x_infl <- mat.or.vec (len, 1)
  y_infl <- mat.or.vec (len, 1)
  
  for (i in 1:len - 1) {
    dx <- x[i+1] - x[i]
    dy <- y[i+1] - y[i]
    ux <- -dy / sqrt(dx*dx + dy*dy)
    uy <-  dx / sqrt(dx*dx + dy*dy)
    x_infl[i] <- x[i] + ux*dist
    y_infl[i] <- y[i] + uy*dist
  }
  # do the closure point
  x_infl[len] <- x_infl[1]
  y_infl[len] <- y_infl[1]
  
  new_poly <- as.points (x_infl, y_infl)
  colnames(new_poly) <- colnames(blob)
  return (new_poly)
  
}

smooth.contour <- function (blob, npts=5) {
  require("splancs")
  x <- blob[,1]
  y <- blob[,2]
  len <- length(x)
  x_smo <- mat.or.vec (len, 1)	
  y_smo <- mat.or.vec (len, 1)
  
  # prepare for circular wrapping	
  xe <- mat.or.vec (len + npts - 1, 1)
  ye <- mat.or.vec (len + npts - 1, 1)
  b <- floor(npts/2)
  
  xe[(b+1):(len+b)] <- x
  ye[(b+1):(len+b)] <- y
  for (i in 1:(b)) {
    xe[i] <- x[len - i]
    ye[i] <- y[len - i]
    xe[len+b+i] <- x[i]
    ye[len+b+i] <- y[i]
  }
  
  # now do the smoothing
  for (i in 1:len) {
    j <- i+b
    x_smo[i] <- sum(xe[(j-b):(j+b)]) / npts
    y_smo[i] <- sum(ye[(j-b):(j+b)]) / npts
  }
  
  new_poly <- as.points (x_smo, y_smo)
  colnames(new_poly) <- colnames(blob)
  return (new_poly)
  
}

# Worker function - what is the centroid of a contour? Assumes cont is a matrix
centroid = function (cont) {
  nr = nrow(cont)
  if ((cont[1,1] == cont[nr,1]) && (cont[1,2] == cont[nr,2])) {
    cont = cont[1:(nr-1),]
  }
  
  vals = c(mean(cont[,1], na.rm=T), mean(cont[,2], na.rm=T))
  names(vals) = colnames(cont)
  return (vals)
}

# Worker function - is a point inside a contour?
inside = function (p, contour) {
  sum_theta = 0
  for (i in 1:(nrow(contour)-1)) {
    x1 = contour[i,1] - p[1]
    y1 = contour[i,2] - p[2]
    x2 = contour[i+1,1] - p[1]
    y2 = contour[i+1,2] - p[2]
    sum_theta = sum_theta + angle2D ( x1, y1, x2, y2)
  }
  
  sum_theta = abs(sum_theta)
  crit = .1
  if (abs(sum_theta - 2*pi) < crit) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}

# worker function - what is the angle between two vectors?
angle2D = function (x1, y1, x2, y2) {
  theta1 = atan2 (y1, x1)
  theta2 = atan2 (y2, x2)
  dtheta = theta2 - theta1
  if (dtheta > pi) {dtheta = dtheta - 2*pi}
  if (dtheta < -pi) {dtheta = dtheta + 2*pi}
  
  dtheta
}

cont2mat = function (cont, param) {
  nrows <- length(cont$x)
  mat <- matrix(NA, nrow=nrows, ncol=2)
  mat[,1] <- cont$x
  mat[,2] <- cont$y
  if ((mat[1,1] != mat[nrows,1]) && (mat[1,2] != mat[nrows,2])) {
    mat = rbind (mat, c(cont$x[1], cont$y[1]))  # close the contour
  }
  colnames(mat) <- param
  mat
}

# area of a contour.  Assumes cont is closed.
#  NOTE:  not handling possible problems due to concavity...
#  area formula from http://www.mathopenref.com/coordtrianglearea.html
contour.area = function (cont) {
  if (!is.matrix(cont)) {cont = cont2mat(cont, param=c("x", "y"))}
  cen = centroid (cont)
  
  # add up triangle areas
  a.tot = 0
  for (i in 1:(nrow(cont)-1)) {
    A = cen
    B = cont[i,]
    C = cont[i+1,]
    area = 0.5 * abs(A[1] * (B[2] - C[2]) + B[1] * (C[2] - A[2]) + C[1] * (A[2] - B[2]))
    a.tot = a.tot + area
    # cat (area, a.tot, "\n")
  }
  
  names(a.tot) = "area"
  a.tot
}


