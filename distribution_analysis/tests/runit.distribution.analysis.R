#
#  testing distribution_analysis
#
#  2014-12-05  WTR
#

library (flowCore)
ff = suppressWarnings (read.FCS ("~/devel/R3.1.x/tools/distribution_analysis/tests/test.fcs"))

test.blob.boundary = function () {
  checkException (blob.boundary(ff=1))                            # not a flowFrame
  bb = blob.boundary (ff)
  checkTrue (is(bb, "matrix"))
  checkEquals (nrow(bb), 139)
  checkEquals (ncol(bb), 2)
  checkEquals (colnames(bb), c("FSC-A","SSC-A"))
}

test.circle = function () {
  blob = matrix(c(10, 10), nrow=1, ncol=2)
  checkException (circle(blob=1))
  checkException (circle(blob=blob))                     # missing arg
  circ = circle(blob=blob, radius=5)
  checkEquals (nrow(circ), 101)
  checkEquals (ncol(circ), 2)
}

test.get.hull = function () {
  
}

test.inflate.contour = function () {
  
}

test.smooth.contour = function () {
  
}

test.cont2mat = function () {
  
}





