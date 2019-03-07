#Cosine Similarity Metric
Cos.sim = function(a, b){
  if (length(a) != length(b)){
    cat ("Error: Vectors are of unequal length")
    return()
  }
  dot= vector("numeric")
  for(i in 1:length(a)){
    x = a[i]
    y = b[i]
    dot[i] =  x*y }
  
  for(i in 1:length(a)){
    x = a[i]
    y = b[i]
    norma = sqrt(sum(a^2))
    normb = sqrt(sum(b^2))
  }
  cossim = (sum(dot)/(norma*normb))
  
  return(cossim)
}

cos.metric = function(fp,row = 1){
  counts = counts(fp, trans = "norm")
  exp= rep(1, ncol(counts))
  dist= Cos.sim(counts[row,], exp)
  return(dist)
}

# sd.metric
# params = fp, row (default = 1)
# returns standard deviation metric distance for row of fp 
sd.metric = function(fp,row = 1){ 
  #   cat("\nsd metric")
  counts = counts(fp,trans="norm")
  distance = sd(counts[row,]-1)/mean(counts[row,]) 
  return (distance)
}

# euclidian.metric
# params = fp, row (default = 1)
# returns euclidean metric distance for row of fp 
euclidean.metric = function(fp,row = 1){ 
  #   cat("\neuclidian metric")
  counts = counts(fp,trans="norm")
  distance = 0 
  for(i in 1:ncol(counts)){
    distance = distance + ((counts[row,i]-1)^2)
  }
  return (distance)
}

# euclidian.metric.log
# params = fp, row (default = 1) with trans = log
# returns euclidean metric distance for row of fp 
euclidean.metric.log = function(fp,row = 1){ 
  #   cat("\neuclidian metric")
  counts = counts(fp)[row,]
  #add 1 to all that have a 0 count
  counts[which(counts==0)] = 1
  lcounts = log(counts)
  distance = 0
  for(i in 1:ncol(counts)){
    distance = distance + lcounts[i]^2
  }
  return (distance)
}

# euclidian.norm.metric
# params = fp, row (default = 1)
# returns euclidean metric distance for row of fp 
euclidean.norm.metric = function(fp,row = 1){
  #   cat("\nnormalized euclidian metric")
  counts = counts(fp,trans="norm")
  distance = 0
  sd = sd(counts[row,])
  for(i in 1:ncol(counts)){
    distance = distance + ((counts[row,i]-1)^2)
  }
  distance = distance/sd
  return (distance)
}



# average.distance
# params = 
# calculates the SD distance of   
# every 
average.distance = function(fp, i){
  ###################################
  # find the distances of each diagnosis from each model
  # then find the distance of the test instance to each model
  # add the distances from the test instance to each of the diagnosis
  # for each model and then use the min of that as the diagnosis
  ###################################
  distances = vector("numeric")
  sds = vector("numeric")
  diags = levels(sampleClasses(fp))
  testDist = sd.metric(fp,i)
  for(j in 1:length(diags)){
    metric = average.distance.helper(fp, diags[j], i)
    avg = metric$avg
    sds [j] = metric$sd
    distances[j] = abs(testDist - avg)
  }
  #   cat(distances)
  if(overlap(distances,sds)){
    return (-1)
  }
  return (distances)
}


# average.distance.helper
# params = fp containing counts, diag:diagnosis being analyzed, i:index of test sample
# calculates the SD distance of every row pertaining to a diag
# every 
average.distance.helper = function (fp, diag, i){
  #find out which ind are associated with the diagnosis and do not include the test instance
  ind = which(sampleClasses(fp) == diag)
  ind = ind [ind!=i]
  
  #get counts from fp
  counts = counts(fp,trans="norm")
  #    cat(counts[ind,])
  counts = counts[ind,]
  distances = vector("numeric")
  #calculate     
  for(j in 1:nrow(counts)){
    distances[j] = sd(counts[j,])/mean(counts[j,]) 
  }
  return (list(avg = mean(distances), sd = sd(distances)))
}

# overlap
# params = distances, standard deviations
# returns true if distances + sd of different diagnoses overlap
overlap = function (distances,sds){
  overlap = F
  for(i in 1:(length(distances)-1)){
    for(j in (i+1):length(distances)){
      #if distance 1 is less than distance 2
      #       cat("I,J",i,j,"\n")
      if (distances[i] < distances[j]){
        maxD1 = distances[i]+sds[i]
        minD2 = distances[j]-sds[j]
        if(maxD1>minD2){
          #these distances overlap and should be looked at by human operator
          overlap = T
          break
        }
        next
      }
      #if distance 2 is less than distance 1
      else{
        minD1 = distances[i]-sds[i]
        maxD2 = distances[j]+sds[j]
        if(maxD2>minD1){
          #these distances overlap and should be looked at by human operator
          overlap = T
          break
        }
      }
    }
    if(overlap){
      break
    }
  }
  return (overlap)
}

#Calculates earth mover's distance 
#exp=1 gives L1 earth mover's distance, exp=2 gives L2 earth mover's distance, etc.  

femd = function(fp, fs, n, exp=1){ 
  
  require(rpart) 
  require(lpSolve) 
  require(clue) 
  
  counts=counts(fp)[n,]
  
  
  r=1
  c=length(counts)
  y=vector ('numeric') 
  
  b <- rep(1/c, length=c) 
  
  #cat(i, "\n")
  binCenters=find_centers(fp, fs, n)
  cost=as.matrix(dist(binCenters,method="minkowski",diag=T,upper=T,p=exp))
  
  a=counts/sum(counts) 
  
  source=pmax(a-b,0) 
  destination=pmax(b-a,0) 
  
  y=lp.transport(cost,direction="min",row.signs=rep("=",c),row.rhs=source,col.sign=rep("=",c),col.rhs=destination,integers=NULL)$objval
  
  return(y) 
} 

#Function bin centers calculates the center of mass on each parameter for each bin
find_centers = function(fp, fs, idx){
  binCenters=vector("numeric")
  #do we want to do exprs on the flow set or the training set for the model?
  
  nbins=2^(nRecursions(fp))
  mat=matrix(nrow=nbins,ncol=length(parameters(fp)))
  nFlowFrame=length(fs)
  for (ind in 1:nbins){
    for (p in parameters(fp)){
      ev_idx <- which(tags(fp)[[idx]]==ind)
      if (length(ev_idx) != 0) {
        loc <- mean (exprs(fs[[idx]])[ev_idx,p])
      } else {
        loc <- (binBoundary(fp)[[ind]]@ll[p] + binBoundary(fp)[[ind]]@ur[p]) / 2
      }
      if(is.nan(loc)){
        loc= (binBoundary(fp)[[ind]]@ur[p]+binBoundary(fp)[[ind]]@ll[p])/2
      }
      mat[ind,which(parameters(fp)==p)]=loc
      #cat(paste("Index: ",ind," parameter: ",which(parameters(fp)==p), " result: ",mat[ind,which(parameters(fp)==p)], sep=""),"\n")
    }	
  }
  return (mat)
}
