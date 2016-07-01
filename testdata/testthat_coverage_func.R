#
#  testthat toy test script.
#   Looking for an issue with flowCore::flowFrame
#
# 2014-12-10  WTR
#

library("flowCore")
library("testthat")

context ("TestCoverage")
load ("./testmat.rda")    # instantiates a matrix called 'mat'
load ("./fp.rda")         # instantiates a flowFP object called 'fp'

test_that ("test.myfunc.mat", {
  expect_equal (nrow(mat), 4)
  expect_equal (ncol(mat), 2)
  expect_equal (colnames(mat), c("FSC-A", "SSC-A"))
  expect_equal (myfunc.mat(mat), 8)

})

# test_that ("test.myfunc.ff", {
#     ff = new("flowFrame", exprs=mat)
#     expect_equal (nrow(ff), 4)
#     expect_equal (colnames(ff), c("FSC-A", "SSC-A"))
# })

test_that ("test.myfunc.fp", {
  expect_equal (ncol(myfunc.fp(fp)), 256)
})




