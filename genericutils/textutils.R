#
#  textutils.R
#
# Miscellaneous text processing utilities.
#
#
################################################################################
################################################################################
#                     Copyright CytoVas LLC 2015.                             ##
#        All Rights Reserved. No part of this source code may be reproduced   ##
#            without CytoVas' express written consent.                        ##
################################################################################
################################################################################
#


# paste tightly
tight = function(...) {
  res = paste(..., sep = "")
  
  res
}

# insert '/' between elements, and append a trailing / if needed
text_to_path = function(...) {
  tmp = c(...)
  x = paste(tmp, collapse = "/")
  
  # eliminate double // if they exist
  x = sub(pattern = "//", replacement = "/", x)
  if (grepl(pattern = "/$", x)) {
    return(x)
  } else {
    return(tight(x, "/"))
  }
}

