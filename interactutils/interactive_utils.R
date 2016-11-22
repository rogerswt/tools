################################################################################
################################################################################
#                     Copyright CytoVas LLC 2015.                             ##
#        All Rights Reserved. No part of this source code may be reproduced   ##
#            without CytoVas' express written consent.                        ##
################################################################################
################################################################################
#
# interactive utils
#  2015-03-02  WTR


get.answer = function (pstring, default.value=NULL) {
  pstring = paste (pstring, ": [", default.value, "] - ", sep="")
  ret = readline (pstring)
  if (ret == "") {
    ret = default.value
  }
  ret
}

# paste together path elements.  Ignore empty ones
build.path = function (parray, trailing.slash = FALSE) {
  # start at root
  path = "/"
  for (p in parray) {
    path = paste (path, "/", p, sep="")
  }
  if (trailing.slash) {
    path = paste (path, "/", sep="")
  }
  # get rid of consecutive slashes
  path = gsub ("//", "/", path, fixed=TRUE)
  path = gsub ("//", "/", path, fixed=TRUE)
  
  path
}


