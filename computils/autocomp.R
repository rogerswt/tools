#
#	Apply the compensation matrix in the FCS header
#

autocomp <- function (f) {
  require (flowCore)
	if (is(f, "flowFrame")) {
		f <- compensate(f, keyword(f)$SPILL)
		return (f)
	}
	else if (is(f, "flowSet")) {
		len <- length(f)
		for (i in 1:len) {
			f[[i]] <- compensate(f[[i]], keyword(f[[i]])$SPILL)
		}
		return (f)
	}
	else {
		cat ("argument must be a flowFrame or a flowSet\n")
	}
}

