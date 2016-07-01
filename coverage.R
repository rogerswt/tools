#
#  Analyze test coverage
#
#  2014-12-08  WTR
#

tools.path = "~/git/R/tools/"

source (paste(tools.path, "sourceTools.R", sep=""))

library("testthat")
library("testCoverage")
library("flowCore")
library("flowViz")

# distribution_analysis
dist_anal.path = paste (tools.path, "distribution_analysis/", sep="")
dist_anal.files = paste (dist_anal.path, c("blob_boundary.R", "blob_utils.R", "deGate.R", "histogram_analysis.R", "locateBlobs.R", "watershed_boundary.R"), sep="")
dist_anal.test = paste (dist_anal.path, "tests/testthat/testthat.distribution.analysis.R", sep="")

# transutils
trans.path = paste (tools.path, "transutils/", sep="")
trans.files = paste (trans.path, c("biexp.R", "boxcox.R"), sep="")
trans.test = paste (trans.path, "tests/testthat/testthat.transutils.R", sep="")

source.files = c(dist_anal.files, trans.files)
test.files = c(dist_anal.test, trans.test)

testCoverage (source.files, test.files)
