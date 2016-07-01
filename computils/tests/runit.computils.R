#
#  testing computils
#
#  2014-12-05  WTR
#

library (flowCore)
ff = suppressWarnings (read.FCS ("~/devel/R3.1.x/tools/computils/tests/test.fcs"))
fs = flowSet (ff, ff)

test.autocomp = function () {
  fc = autocomp (ff)
  checkTrue (is (autocomp (ff), "flowFrame"))       # does it return a flowFrame?
  checkTrue (is (autocomp (fs), "flowSet"))         # does it return a flowSet?
  val = as.numeric (exprs(autocomp(ff))[1,8])
  checkEquals (val, 85.90628)                       # spot check numeric result
  checkEquals (autocomp(1), NULL)                   # does it properly exit?
}

test.compute.spill = function () {
  
}

test.find.fl.parameters = function () {
  curr = c("FITC-A", "PE-A", "APC-A", "PerCP-A")
  checkEquals (find.fl.parameters(ff), curr)    
}

test.spill.slope = function () {
  val = as.numeric (spill.slope (ff, "FITC-A", "PE-A"))
  checkEquals (val, 0.4048673, tolerance=.00001)
}

test.plot.empty.diag = function () {
  
}


