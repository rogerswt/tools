# getFlowFrame
#  This function reads a flowFrame corresponding to a row in a query table
#  return value: a compensated flowFrame
#  the transform parameter is a named list.  Name is the method of transform, the value is the array of columns for that method
getFlowFrame <- function (tab=NULL, row, file = NULL, comp=T, transform=NULL, derail=F, swapNames = T, capNames = F) {
  if(is.null(tab) & is.null(file)){
    stop("Please either give a database table and a row or a filename")
  }
  if(!is.null(tab)){
    fn <- paste (tab$path[row], tab$folder[row], tab$filename[row], sep="/")
  }
  else{
    fn = file
  }
  
  ff <- suppressWarnings(read.FCS (fn, trans=F))
  if (comp){
    ff = autocomp(ff)
  }
  if(!is.null(transform)){
    for(i in 1:length(transform)){
      ff = doTransform(ff, transform[[i]], names(transform)[i])
    }
    if(derail){
      ff = Subset (ff, rectangleGate ("FSC-A"=c(1.0, 5.39), "SSC-A"=c(-Inf, 5.39))) # applying FSC-A 1.0 lower threshold, trying to reproduce results from 2011_11_28
    }
  }
  if (swapNames){
    ff = swapNames(ff,capNames)
  }
  return(ff)
}

# imports a db table and calls
# getFlowFrame on each instance in rows
# and returns resulting flowSet
getFS = function(tab, rows = c(1:nrow(tab)),transform = NULL, swapNames = T){
  fflist = list()
  for(i in rows){
    fflist[[i]] = getFlowFrame(tab, i, transform = transform, swapNames = swapNames)
  }
  return (as(fflist,"flowSet"))
}

# ffSample
#  Occasionally we only want a sampled set of events ...
#  return value: a randomly sampled flowFrame
ffSample <- function (f, nevents=10000) {
  if(is(f,"flowFrame")){
    Subset(ff, sampleFilter(nevents))
  }
  else if (is(f,"flowSet")){
    for(i in 1:length(f)){
      f[[i]] = Subset(f[[i]], sampleFilter(nevents))
    }
  }

}


# doTransform
#  This function performs biexponential transform on all parameters except SSC-W
# Jitter is set to false for biexponential to get reproducible results (otherwise would result in random #)
#  return value: a transformed flowFrame
doTransform <- function (f,cols=c(1:5,7:13),method=c("biexp","log","linear"), fac=5.4/262143) {
  if (is(f,"flowFrame")){
    method=match.arg(method)
    if(method=="biexp"){
      bx <- biexpTransform(jitter=F)
      bxlist <- transformList (colnames(f)[cols], bx)
      return(transform (f, bxlist))  
    }
    if(method=="log"){
      lx <- logTransform()
      lxlist <- transformList (colnames(f)[cols], lx)
      return(transform (f, lxlist))	
    }
    if(method=="linear"){
      lx <- linearTransform(a=fac)
      lxlist <- transformList (colnames(f)[cols], lx)
      return(transform (f, lxlist))	
    }
  }
  else if (is(f,"flowSet")){
    for(i in 1:length(f)){
      method=match.arg(method)
      if(method=="biexp"){
        bx <- biexpTransform(jitter=F)
        bxlist <- transformList (colnames(f[[i]])[cols], bx)
        f[[i]] = (transform (f[[i]], bxlist))  
      }
      if(method=="log"){
        lx <- logTransform()
        lxlist <- transformList (colnames(f[[i]])[cols], lx)
        f[[i]] = (transform (f[[i]], lxlist))	
      }
      if(method=="linear"){
        lx <- linearTransform(a=fac)
        lxlist <- transformList (colnames(f[[i]])[cols], lx)
        f[[i]] = (transform (f[[i]], lxlist))	
      }
    }
  }
  return (f)

}

swapNames = function(ff,capNames=F){
  ind = which(!is.na(parameters(ff)$desc))
  colnames(ff)[ind] = parameters(ff)$desc[ind]
  if(capNames){
    colnames(ff)[ind] = toupper(colnames(ff)[ind])
  }
  return(ff)
}
