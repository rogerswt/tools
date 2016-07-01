#
#	This function makes a "FlowJo" style color ramp, but with saturation being variables
#

blob_color <- function(sat=.999, blueBackground=FALSE) {
	
	hue_list <- c(seq(.6667, .5, len=10),  seq(.5, 0.000, len=10))
#	sat_list <- c(seq (0, sat, len=3), rep(sat, len=17))
	sat_list <- rep(sat, len=20)
	clist <- hsv (h=hue_list, s=sat_list, v=1)
	if (blueBackground) {
		clist <- c("#0000FF", clist)
	}
	else {
		clist <- c("#FFFFFF", clist)
	}
	return (colorRampPalette(clist))
}


#THE METHOD BELOW HAS BEEN DEPRECATED BY TICK FUN PPLOT
#########################################################
#
#	This function is a wrapper function for flowCore:plot
#	It creates a plot that looks very much like FlowJo.
#
# pplot <- function (ff, plist, blueBackground=FALSE, showZero=TRUE, nbin=501, bandwidth=0.001, cr=blob_color(blueBackground=FALSE), nrpoints=0, ...) {
# 	require (fields)
# 	
# 	suppressWarnings (plot (ff, plist, colramp=cr, nbin=nbin, band=bandwidth, nrpoints=nrpoints, ...))
# 		
# 	if (showZero) {
# 		xline (0, lty='dotdash')
# 		yline (0, lty='dotdash')
# 	}
# }
#######################################################
