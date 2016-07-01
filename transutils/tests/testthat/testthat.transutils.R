#
# Converted from runit.biexp.transform.R
#
# 2014_12_09  WTR

# 

library("flowCore")
library("testthat")

library("flowFP")
data(fs1)
ff = fs1[[1]]

source ("~/git/R/tools/transutils/sourceDirectory.R")

context ("Transformation methods")

# test_that("test.biexp", {
#     expect_error(biexp(f = 1, params = 3:7))
#     expect_error(biexp(f = ff))
#     # expect_true(is(biexp(f = ff, params = 3:7), "flowFrame"))
# })
# 
# test_that("test.biexp.transform", {
#     expect_equal(0.015580252, biexp.transform(10, jitter = F))
#     expect_equal(1.1246851929, biexp.transform(1000, jitter = F))
#     expect_equal(-1.1246851929, biexp.transform(-1000, jitter = F))
#     expect_error(biexp.transform("xx"))
# })

test_that("test.biexpTransform", {
      load ("~/git/R/tools/testdata/ffmat.rda")
      expect_equal(nrow(mat), 100000)
      expect_equal(nrow(ff), 30000)
#       ff <- read.FCS ("~/git/R/tools/testdata/ff.fcs")
#       res = colnames(ff)[7:8]
#       expect_equal(nrow(ff), 100000)
#       expect_equal (c("530/30 Blue-A", "575/26 Blue-A"), res)
#     bx = biexpTransform()
#     expect_true(is(bx, "transform"))
#     res = flowCore::transform (ff, transformList (res, bx))
#     val = parameters(res)$maxRange[7]
#     expect_equal(val, 5.418539, tolerance=1e-5)
})

# test_that("test.boxcox.transform", {
#     expect_equal(1.046186, boxcox.transform(10000, lambda = 0.5, 
#         jitter = F), tolerance=1e-5)
#     expect_equal(NaN, boxcox.transform(10000, lambda = 0))
#     expect_error(boxcox.transform("xx"))
# })
# 
# test_that("test.boxcoxTransform", {
#     expect_true(is(boxcoxTransform(), "transform"))
#     bxcx = boxcoxTransform()
#     res = flowCore::transform (ff, transformList (colnames(ff)[7:8], bxcx))
#     val = parameters(res)$maxRange[7]
#     expect_equal(val, 5.4, tolerance=1e-5)
# })
# 
# test_that("test.inv.biexp.transform", {
#     expect_equal(255978.784205398, inv.biexp.transform(5.4))
#     expect_equal(0, inv.biexp.transform(0))
#     expect_error(inv.biexp.transform("xx"))
# })
# 
