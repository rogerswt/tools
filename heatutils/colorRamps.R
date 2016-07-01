#
#    functions for heatmap (for package pheatmap)
#

heat.color.sat <- function(hue = 2/3, val=.5) {

  sat_list = seq (0, 1, len=20)
  val_list = seq (1, val, len=20)
  clist = hsv (h=hue, s=sat_list, v=val_list)
  
  return (colorRampPalette(clist))
}

# do 21 steps, one in the center and 10 on each side
heat.color.red.blue = function (val=0.5) {
  hue_list = c(rep(2/3, len=10), 0, rep(1, len=10))
  val_list = c(seq(val, 1, len=10), 1, seq(1, val, len=10))
  sat_list = c(seq(1, 0, len=10), 0, seq(0, 1, len=10))
  
  clist = hsv (h=hue_list, s=sat_list, v=val_list)
  
  return (colorRampPalette(clist))
}