#
#  Run a bunch of testthat tests
#
#  2014-12-09  WTR
#

# preload libraries so that test output is clean
library("testthat")
library("flowCore")
library("flowViz")
library("splancs")
library("sp")
library("fields")
library("spam")
library("grid")
library("wmtsa")
library("splus2R")
library("ifultools")
library("MASS")

tools.path = "~/git/R/tools/"
source (paste(tools.path, "sourceTools.R", sep=""))

# distribution_analysis
dist_anal.path = paste (tools.path, "distribution_analysis/", sep="")
dist_anal.test = paste (dist_anal.path, "tests/testthat/testthat.distribution.analysis.R", sep="")

# transutils
trans.path = paste (tools.path, "transutils/", sep="")
trans.test = paste (trans.path, "tests/testthat/testthat.transutils.R", sep="")

test.files = c(dist_anal.test, trans.test)

for (f in test.files) {
  test_file (f)
}