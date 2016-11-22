#
#  new blob location algorithm
#
#  2016-09-02  WTR
#
#
################################################################################
################################################################################
#                     Copyright CytoVas LLC 2015.                             ##
#        All Rights Reserved. No part of this source code may be reproduced   ##
#            without CytoVas' express written consent.                        ##
################################################################################
################################################################################
#

locate.blobs.2 = function(ff, param, bandwidth = .02, nbin = 501, min_area = .01, show = FALSE) {
  kde = get_kde(ff = ff, params = param, bandwidth = bandwidth, nbin = nbin)
  cnt = get_contours(kde = kde)
  cnt = eliminate_open_contours(cnt)
  lev = get_levels(cnt)
  cpl = contours_per_level(cnt, lev)
  ut = find_upward_transitions(cpl)
  cnt = filter_contours(cnt, ut)
  cnt = sort_contours(cnt, decreasing = FALSE)
  cmat = contour_mat_list(cnt)
  for (i in 1:length(cmat)) {
    colnames(cmat[[i]]) = param
  }
  cmat = eliminate_small_contours(cmat, min_area = min_area)
  res = select_contours(cmat, progress = FALSE)
  
  # added 2016-10-24 WTR
  res = eliminate_duplicate_contours(res)
  
  # find centers (for compatibility with locate.blobs)
  centers = matrix(NA, nrow = length(res), ncol = 2)
  for (i in 1:length(res)) {
    centers[i, 1] = mean(res[[i]][,1])
    centers[i, 2] = mean(res[[i]][,2])
  }
  if (show) {
    pplot(ff, param)
    for (i in 1:length(res)) {
      lines(res[[i]], lwd = 1)
      points(centers[i, 1], centers[i, 2], pch = 'x')
    }
  }
  
  return(list(centers = centers, contours = res))
}

eliminate_duplicate_contours = function(clist) {
  out = list()
  idx = 1:length(clist)
  k = 1
  out[[k]] = clist[[1]]
  clist = clist[idx[-1]]
  while (TRUE) {
    idx = 1:length(clist)
    dupe_idx = vector('numeric')
    for (i in idx) {
      if (identical(out[[k]], clist[[i]])) {
        dupe_idx = append(dupe_idx, i)
      }
    }
    if (length(dupe_idx) > 0) {
      clist = clist[-dupe_idx]
    }
    if (length(clist) > 0) {
      k = k + 1
      out[[k]] = clist[[1]]
      if (length(clist) == 1) {
        break
      } else {
        clist = clist[-1]
      }
    } else {
      break
    }
  }
  out
}

# do a 2D kde at given resolution
get_kde = function(ff, params, bandwidth, nbin) {
  x = exprs(ff)[, params]
  bw = bandwidth
  kde = bkde2D(x, bandwidth = c(bw, bw), gridsize = c(nbin, nbin))
  
  kde$fhat = kde$fhat / max(kde$fhat)
  
  kde
}

# get lots of contours
get_contours = function(kde, levels = seq(.001, .999, by = .001)) {
  res = contourLines(x = kde$x1, y = kde$x2, z = kde$fhat, levels = levels)
  
  res
}

eliminate_open_contours = function(cnt) {
  out = list()
  k = 1
  for (i in 1:length(cnt)) {
    x = cnt[[i]]$x
    y = cnt[[i]]$y
    npts = length(x)
    if (x[1] != x[npts] | y[1] != y[npts]) {next}
    out[[k]] = cnt[[i]]
    k = k + 1
  }
  
  out
}

get_levels = function(cnt) {
  levels = sapply(cnt, FUN = function(x){x$level})
  ulevels = unique(levels)
  
  ulevels
}

# count the number of contours at given levels
contours_per_level = function(cnt, levels = seq(.001, .999, by = .001)) {
  # tol = .1 * min(levels)
  tol = .0001
  
  nc = vector(mode = 'numeric')
  for (i in 1:length(levels)) {
    l = levels[i]
    nc[i] = length(which(sapply(cnt, FUN = function(x){abs(x$level - l) < tol})))
  }
  
  names(nc) = levels
  nc
}

# get the levels at which number of contours increases
find_upward_transitions = function(cpl) {
  ut = vector(mode = 'numeric')
  k = 1
  for (i in 2:length(cpl)) {
    if (cpl[i] > cpl[i-1]) {
      ut[k] = as.numeric(names(cpl[i]))
      k = k + 1
    }
  }
  
  ut
}

get_contours_at_level = function(cnt, level) {
  # tol = .1 * min(levels)
  tol = .0001
  
  res = list()
  idx = which(sapply(cnt, FUN = function(x){abs(x$level - level) < tol}))
  for (i in 1:length(idx)) {
    res[[i]] = cnt[[idx[i]]]
  }
  res
}

# get all contours at upward transitions
filter_contours = function(cnt, lev_ut) {
  res = list()
  k = 1
  for (lev in lev_ut) {
    tmp = get_contours_at_level(cnt = cnt, level = lev)
    for (i in 1:length(tmp)) {
      res[[k]] = tmp[[i]]
      k = k + 1
    }
  }
  res
}

# sort contours by area
sort_contours = function(cnt, decreasing = TRUE) {
  area = vector(mode = 'numeric')
  for (i in 1:length(cnt)) {
    area[i] = polygon.area(cnt[[i]])
  }
  idx = sort(area, decreasing = decreasing, index.return = TRUE)$ix
  tmp = list()
  for (i in 1:length(idx)) {
    tmp[[i]] = cnt[[idx[i]]]
  }
  tmp
}

# see http://stackoverflow.com/questions/16285134/calculating-polygon-area
polygon.area = function(cont) {
  if (!is.matrix(cont)) {cont = cont2mat(cont, param=c("x", "y"))}
  
  area = 0.0
  npts = nrow(cont)
  
  j = npts
  for (i in 1:npts) {
    area = area + (cont[j,1] + cont[i,1]) * (cont[j,2] - cont[i,2])
    j = i
  }
  area = area / 2.0
  
  area
}

contour_mat_list = function(cnt) {
  res = list()
  for (i in 1:length(cnt)) {
    mat = matrix(NA, nrow = length(cnt[[i]]$x), ncol = 2)
    mat[, 1] = cnt[[i]]$x
    mat[, 2] = cnt[[i]]$y
    res[[i]] = mat
  }
  res
}

eliminate_small_contours = function(cmat, min_area = 10) {
  res = list()
  
  k = 1
  for (i in 1:length(cmat)) {
    if(contour.area(cmat[[i]]) > min_area) {
      res[[k]] = cmat[[i]]
      k = k + 1
    }
  }
  
  res
}

# count the number of contours in cmat contained by the one at position idx.
# NOTE: it is required that cmat be ordered smallest to largest.
number_contained = function(cmat, idx) {
  if (idx == 1) {return(0)}
  
  n = 0
  for (i in 1:(idx - 1)) {
    cen = centroid(cmat[[i]])
    if (inside(p = cen, contour = cmat[[idx]])) {
      n = n + 1
    }
  }
  
  n
}

# contours have been sorted small to large
# start at the smallest contour.  Climb upwards, checking each larger
# contour, asking 2 questions:
#   1 - Does it contain me?  If not, ignore it
#   2 - If yes, does it contain any others?
#     - If yes, remove it from the list, store me, and start over (?)
#     - If no, remove me from the list and continue upwards
# When we reach the top, start over at the bottom
# Final stopping criterion: number of contours does not change in the pass
select_contours = function(cmat, progress = FALSE) {
  if (progress) {
    contour(x = kde$x1, y = kde$x2, z = kde$fhat, col = 'white')
    for (i in 1:length(cmat)) {
      lines(cmat[[i]])
    }
  }
  n_cont = length(cmat)
  out = list()
  k = 1       # output index
  i = 1       # me index
  j = 2       # next one above me index
  while (length(cmat) > 0) {
    if (j > length(cmat)) {
      # little is not contained.  Store it
      out[[k]] = cmat[[i]]  # store contour ...
      cmat = cmat[-i]       # ... and remove it ...
      if (progress) {lines(out[[k]], col = 'red', lwd = 2)}
      k = k + 1             #
      i = 1                 # ... and start over
      j = 2
      # readline(prompt = "little not contained")
      next
    }
    little = cmat[[i]]
    big = cmat[[j]]
    
    # Does big contain little?
    cen = centroid(little)
    if (!inside(p = cen, contour = big)) {
      # big doesn't contain little.  It's unrelated, move on
      j = j + 1
      next
    } else {
      # big contains little. Does big contain others?
      if (number_contained(cmat, j) == 1) {
        # big does not contain others
        if(progress) {lines(cmat[[i]], col = 'gray', lwd = 2)}
        cmat = cmat[-i]   # delete little ...
        j = i + 1
        next
      } else {
        # big DOES contain others
        if (progress) {lines(cmat[[j]], col = 'blue', lwd = 2)}
        cmat = cmat[-j]       # remove big ...
        out[[k]] = cmat[[i]]  # store contour ...
        if (progress) {lines(out[[k]], col = 'red', lwd = 2)}
        k = k + 1
        j = i + 1
        next
      }
    }
  }
  out
}





