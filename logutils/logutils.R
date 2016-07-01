#
#  logutils.R
#
#  2014-12-04  WTR
#

# Retrieve the name of the containing function
function.name = function (which=-1) {
  return (sys.call(which)[[1]])
}

# log a message
log.msg = function (msg="", level='info') {
  logger = getLogger()
  if (!is.null(logger)) {
    if (level == 'info') {
      loginfo (msg)
    }
  }
}

# log the function name and the values of its arguments
log.args <- function () {
  func = function.name(which=-2)
  args = as.list((sys.frame(-1)))
  msg = sub ("list", "", paste ("ENTERING", func, list(args)))
  log.msg (msg)
}

# a template for a function that logs 
template.logging.function = function (x=1, y=2) {
  log.args()
  func = function.name()
  
  # set all adjustable parameters here
  var.list = list (
    "internal.var.1 = 1",
    "internal.var.2 = list(x=1, y=2)",
    "call.it.whatever.you.like = 3"
  )
  eval (parse (text=var.list))
  log.msg (paste ("INTERNAL PARAMETERS:", paste(var.list, collapse=", ")))
  
  #****************YOUR CODE GOES HERE ************************
  # requires, etc go here
  
  # function guts go here
  
  ret.val = 1   # an example of a return value
  
  #**************END YOUR CODE GOES HERE **********************

  log.msg ("EXITING", paste (func, "optional log info here"))
  return (ret.val)
}