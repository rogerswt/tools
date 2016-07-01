#
# testthat tests
#
# 2014-12-05  WTR
#

context ("2D blob analysis")

library ("flowCore")
load ("~/git/R/tools/testdata/ff.rda")

library(KernSmooth)
kde = bkde(exprs(ff)[,"FSC-A"])

test_that("test.blob.boundary", {
  expect_error(blob.boundary(ff = 1))
  bb = blob.boundary(ff)
  expect_true(is(bb, "matrix"))
  expect_equal(145, nrow(bb))
  expect_equal(2, ncol(bb))
  expect_equal(c("FSC-A", "SSC-A"), colnames(bb))
  bb = blob.boundary(ff, log.transform=T)
  expect_equal (nrow(bb), 557)
})

test_that("test.circle", {
  blob = matrix(c(10, 10), nrow = 1, ncol = 2)
  expect_error(circle(blob = 1))
  expect_error(circle(blob = blob))
  circ = circle(blob = blob, radius = 5)
  expect_equal(101, nrow(circ))
  expect_equal(2, ncol(circ))
})

test_that("test.get.hull", {
  bb = blob.boundary(ff)
  hull = get.hull(bb)
  expect_equal (109, nrow(hull))
  expect_equal (2, ncol(hull))
  expect_equal (c("FSC-A", "SSC-A"), colnames(hull))
})

test_that("test.inflate.contour", {
  bb = blob.boundary(ff)
  expect_error(inflate.contour (blob=1))     # not a contour
  expect_error(inflate.contour (blob=bb))    # missing arg dist
  infl = inflate.contour(bb, dist=10)
  expect_equal (145, nrow(infl))
  expect_equal (2, ncol(infl))
  expect_equal (c("FSC-A", "SSC-A"), colnames(infl))
})

test_that("test.smooth.contour", {
  bb = blob.boundary(ff)
  expect_error(smooth.contour (blob=1))     # not a contour
  infl = smooth.contour(bb)
  expect_equal (145, nrow(infl))
  expect_equal (2, ncol(infl))
  expect_equal (c("FSC-A", "SSC-A"), colnames(infl))
})

test_that("test.contour.area", {
  bb = blob.boundary(ff)
  infl = inflate.contour (bb, dist=1000)
  expect_equal (c("area"=1240180671), contour.area(bb))
  expect_equal (c("area"=1374973025), contour.area(infl))
})

test_that("test.locate.blobs", {
  expect_error(locate.blobs(ff = 1))                             # first arg must be a flowFrame
  bl = locate.blobs(ff, param=c("FSC-A", "SSC-A"), eps=.05)    # this one takes awhile
  expect_equal (nrow(bl$centers), 2)
  expect_equal (c("FSC-A", "SSC-A"), colnames(bl$centers))
  expect_equal (nrow(bl$contours[[1]]), 275)
  expect_equal (ncol(bl$contours[[1]]), 2)
  expect_equal (nrow(bl$contours[[2]]), 127)
  bl = locate.blobs(ff, eps=.02, log.transform=T, show=T)             # test log transform
  expect_equal (nrow(bl$centers), 3)
  expect_equal (nrow(bl$contours[[1]]), 359)
  expect_equal (nrow(bl$contours[[2]]), 305)
  expect_equal (nrow(bl$contours[[3]]), 199)
})

test_that("find.local.maxima", {
  mx = find.local.maxima (kde, show=T)
  expect_equal(length(mx$x), 3)
  expect_equal(length(mx$y), 3)
  expect_equal(mx$pick, c(92, 168, 258))
})

test_that("find.local.minima", {
  mn = find.local.minima (kde, show=T)
  expect_equal(length(mn$x), 2)
  expect_equal(length(mn$y), 2)
  expect_equal(mn$pick, c(113, 214))
})

test_that("find.width", {
  wid = find.width (kde)
  expect_equal(wid$center, 133373.2, tolerance=.001)
  expect_equal(wid$left, 98009.03, tolerance=.001)
  expect_equal(wid$right, 168737.4, tolerance=.001)
  expect_equal(wid$frac, 0.5, tolerance=.001)
})
