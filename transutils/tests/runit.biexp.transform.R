#
#  testing functions in transutils
#
#  2014-12-04  WTR
#

test.biexp.transform = function () {
  checkEquals (biexp.transform(10, jitter=F), 0.0155802520)
  checkEquals (biexp.transform(1000, jitter=F), 1.1246851929)
  checkEquals (biexp.transform(-1000, jitter=F), -1.1246851929)
  checkException (biexp.transform("xx"))
}

test.inv.biexp.transform = function () {
  checkEquals (inv.biexp.transform(5.4), 255978.7842053979)
  checkEquals (inv.biexp.transform(0.0), 0.0)
  checkException (inv.biexp.transform("xx"))
}

test.biexp = function () {
  # get a flowFrame to work with
  load(paste(system.file("data", package="flowFP"), "/fs1.rda", sep=""))
  checkException (biexp(f=1, params=3:7))            # f is not a flowFrame
  checkException (biexp(f=fs1[[1]]))                 # omitted params
  checkTrue (is (biexp(f=fs1[[1]], params=3:7), "flowFrame"))
}

test.biexpTransform = function () {
  checkTrue (is (biexpTransform(), "transform"))     # is it a flowCore compatible transform object?
}

test.boxcox.transform = function () {
  checkEquals (boxcox.transform (10000, lambda=0.5, jitter=F), 1.046186, tolerance=1e-5)
  checkEquals (boxcox.transform (10000, lambda=0), NaN)
  checkException (boxcox.transform("xx"))
}

test.boxcoxTransform = function () {
  checkTrue (is (boxcoxTransform(), "transform"))     # is it a flowCore compatible transform object?
}