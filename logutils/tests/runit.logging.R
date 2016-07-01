#
#  testing logutils
#
# 2014-12-05  WTR
#
#  NOTE:  there seems to be parsing issues with RUnit.  Testing function.name isn't working correctly.

library(logging)

test.function.name = function () {
#   func = as.character (function.name())
#   cat (func, "\n")
#   checkEquals (func, "test.function.name")
}

test.log.msg = function () {
  
}

test.log.args = function () {
  
}

test.template.logging.function = function () {
  checkEquals (template.logging.function(), 1)
}