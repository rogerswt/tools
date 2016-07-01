#
#  Dummy function.  For coverage testing
#
# 2014-12-10  WTR
#

myfunc.mat = function (x) {
  if (is(x, "matrix")) {
    n.elements = nrow(x) * ncol(x)
  } else {
    stop ("not a matrix")
  }
  
  n.elements
}

myfunc.ff = function (x) {
  if (is(x, "flowFrame")) {
  n.elements = nrow(x) * ncol(x)
  } else {
    stop ("not a flowFrame")
  }
  
  n.elements
}

myfunc.fp = function (x) {
  require ("flowFP")
  cmat = counts(fp)
  cmat
}
