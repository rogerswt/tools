#
# Uses in order tree traversal to color the nodes of the trees
# based on the associated color in the vector colors
# requires calling gen.colors beforehand
#
color.dendrogram = function (d, colors){
  if(!is.null(attributes(d[[1]])$height)){
    d[[1]] = color.dendrogram (d[[1]],colors)
  }
  if(is.leaf(d)){
    #todo add coloring mechanism based on label/index into colors
    color = colors[labels(d)]
    attr(d,"edgePar") = list(col = color, lwd = 2)
  }
  else{
    d[[2]] = color.dendrogram(d[[2]],colors)
  }
  return (d)
}

gen.colors = function(labels, colors){
  unique = unique(labels)
  colvec = vector("character")
  for(i in 1:length(unique)){
    ind = which(labels==unique[i])
    colvec[ind] = colors[i]
  }
  return (colvec)
}
