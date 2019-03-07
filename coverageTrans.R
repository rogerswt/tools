#
#  Analyze test coverage
#
#  2014-12-08  WTR
#

tools.path = FLOW_TOOLS_BASE

source (paste(tools.path, "sourceTools.R", sep=""))

library("testthat")
library("testCoverage")
library("flowCore")
library("flowViz")
library("flowFP")

# transutils
trans.path = paste (tools.path, "transutils/", sep="")
trans.files = paste (trans.path, c("biexp.R", "boxcox.R"), sep="")
trans.test = paste (trans.path, "tests/testthat/testthat.transutils.R", sep="")

reportCoverage(sourcefiles=trans.files, executionfiles=trans.test, writereport=F)

