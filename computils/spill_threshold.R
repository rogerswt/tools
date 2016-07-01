#
#  Function to "curve" a threshold due to spillover
#


# NOTE:  This function operates in linear coordinates.
#
# pname: the name of the parameter of interest
# pvalue: the value of the given parameter to be tested (is it above or below the curved threshold?)
# threshold.zero: the initial threshold value, to be adjusted by adding the spillover spread
# spill.matrix: the spillover matrix
# fac: a factor by which to scale the spillover spread
spill.threshold = function (pname, pvalue, threshold.zero, spill.matrix, fac=3) {
  
}