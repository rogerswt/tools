#
#
#


# derail selected parameters
derail = function (ff, parameters=NULL, fac=0.99, remove.zeroes=FALSE) {
  if (is.null(parameters)) {
    stop ("You must supply at least one parameter to derail")
  }
  
  cnames = colnames(ff)
  
  # parameters must all be in ff
  tmp = parameters %in% cnames
  if (sum(tmp) != length (tmp)) {
    stop ("Parameters must all be in ff")
  }
  ind = which (cnames %in% parameters)
  if (is (ff, "flowSet")) {
    mx = parameters(ff[[1]])$maxRange
  } else if (is(ff, "flowFrame")) {
    mx = parameters(ff)$maxRange
  } else {
    stop ("First arg must be either a flowframe or a flowSet")
  }
  mx = fac * mx
  myexpr = vector('character')
  k = 1
  for (i in ind) {
    myexpr[k] = paste ("'", cnames[i], "'=c(-Inf, ", mx[i], ")", sep="")
    k = k + 1
  }
  tmp = paste (myexpr, collapse=", ")
  gexpr = paste ("list (", tmp, ")", sep="")
  
  out = Subset (ff, rectangleGate (filterId="derail", .gate=eval(parse(text=gexpr))))
  
  out
}

# deneg (get rid of negative values and possibly zeroes) for selected parameters
deneg = function (ff, parameters=NULL, min.value=0) {
  if (is.null(parameters)) {
    stop ("You must supply at least one parameter to derail")
  }
  
  cnames = colnames(ff)
  
  # parameters must all be in ff
  tmp = parameters %in% cnames
  if (sum(tmp) != length (tmp)) {
    stop ("Parameters must all be in ff")
  }
  ind = which (cnames %in% parameters)
  
  myexpr = vector('character')
  
  k = 1
  for (i in ind) {
    myexpr[k] = paste ("'", cnames[i], "'=c(", min.value, ", Inf)", sep="")
    k = k + 1
  }
  tmp = paste (myexpr, collapse=", ")
  gexpr = paste ("list (", tmp, ")", sep="")
  
  out = Subset (ff, rectangleGate (filterId="deneg", .gate=eval(parse(text=gexpr))))
  
  out
}

