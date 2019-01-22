#' Plot confidence ellipses
#' 
#' Plot confidence ellipses.
#' 
#' 
#' @param mat Matrix with all the points
#' @param alpha the confidence level of the ellipses
#' @param coord a length 2 vector specifying the components to plot
#' @param eig a matrix with the component of the factor analysis (in row) and
#' the eigenvalues, the inertia and the cumulative inertia for each component.
#' Typically, the \code{eig} output of the \code{construct.axes} function
#' @param cex cf. function \code{\link{par}} in the \pkg{graphics} package
#' @param color a vector with the colors used; by default there are 35 colors
#' defined
#' @param title string corresponding to the title of the graph you draw (by
#' default NULL and a title is chosen)
#' @author Francois Husson
#' @keywords dplot internal
#' @examples
#' 
#' \dontrun{
#' data(chocolates)
#' donnee <- cbind.data.frame(sensochoc[,c(1,4,5:18)])
#' axe <- construct.axes(donnee, scale.unit = TRUE)
#' simul <- simulation(axe)
#' plotellipse (simul, alpha = 0.05, eig = signif(axe$eig,4))
#' #######################################
#' donnee <- cbind.data.frame(sensochoc[,c(1,4,5:18)])
#' axe <- construct.axes(donnee, group = c(6,8), 
#'     name.group = c("A-F","T-S"),scale.unit = TRUE)
#' simul <- simulation(axe, nbgroup = (ncol(axe$partiel)-2)/(ncol(axe$moyen)-2))
#' plotellipse (simul, alpha = 0.05, eig = signif(axe$eig,4))
#' }
#' @export plotellipse
"plotellipse" <- function(mat,alpha=0.05,coord=c(1,2),eig,cex=1,color=NULL,title=NULL){

if (length(color)==0) color = c("black","red","green3","blue",
  "cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey",
  "lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange",
  "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey",
  "darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon")
 plotellipseinter(mat,alpha=alpha,coord=coord,nbgroup=1,moy=TRUE,eig=eig,color=color,cex=cex,title=title)
if (length(mat$partiel)!=0) {
dev.new()
  nbgroup=length(levels(mat$partiel$simul[,ncol(mat$partiel$simul)])) / length(levels(mat$moy$simul[,ncol(mat$moy$simul)]))
  plotellipseinter(mat,alpha=alpha,coord=coord,nbgroup=nbgroup,moy=FALSE,eig=eig,color=color,cex=cex,title=title)
}
}
