#
# Set up and run a RUnit test suite
#
#  2014-12-04  WTR
#

library (RUnit)

source ("~/git/R/tools/sourceTools.R")

basepath = "~/git/R/"
dirs = c(
  "tools/transutils/tests",
  "tools/computils/tests",
  "tools/logutils/tests",
  "tools/classutils/tests",
  "tools/distribution_analysis/tests"  
  )
dirs = paste (basepath, dirs, sep="")

toolsTestSuite = defineTestSuite("tools", dirs=dirs, 
                                 testFileRegexp="^runit.+\\.[rR]$", 
                                 testFuncRegexp="^test.+")

# check that the test suite is valid
cat ("Test suite valid:", isValidTestSuite(toolsTestSuite))

# run the test suite
testResult = runTestSuite(toolsTestSuite)

# print the results (html and text)
printHTMLProtocol(testResult, file=paste (basepath, "testReport.html", sep=""))
printTextProtocol(testResult, file=paste (basepath, "testReport.txt", sep=""))


