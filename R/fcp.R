#' Free choice profiling
#' 
#' Free choice profiling with confidence ellipses
#' 
#' Perform MFA on the data frame and calculate confidence ellipses around the
#' products
#' 
#' @param X data.frame
#' @param group a list indicating the number of variables in each group; used
#' when method="freechoice" or method="hsort"
#' @param scale boolean, used when method="freechoice"; if TRUE, the variables
#' are scaled
#' @param ncp number of components used to procrustes the virtual subspaces on
#' the true subspace; NULL by default and the number of components is estimated
#' @param axes a length 2 vector specifying the components to plot
#' @param name.group the names of each group of variables
#' @param level.conf confidence level used to construct the ellipses. By
#' default, 0.95
#' @param nbsim the number of simulations (corresponding to the number of
#' virtual panels) used to compute the ellipses
#' @param nbchoix the number of panelists forming a virtual panel, by default
#' the number of panelists in the original panel
#' @param cex cf. function \code{\link{par}} in the \pkg{graphics} package
#' @param color a vector with the colors used; by default there are 35 colors
#' defined
#' @param title string corresponding to the title of the graph you draw (by
#' default NULL and a title is chosen)
#' @param new.plot boolean, if TRUE, a new graphical device is created
#' @param graph list with the graphs to draw; "ind", "var" and "ellipse" by
#' default
#' @return Returns a list with the result of the MFA and the bootstraped
#' results that can be used with the plotellipse function.
#' @author Francois Husson
#' @keywords multivariate dplot
#' @examples
#' 
#' \dontrun{
#' data(perfume_fcp)
#' res <- fcp(perfume_fcp, group = c(12,7,7,7,6,8))
#' }
#' 
#' @export fcp
fcp <- function(X,group,scale=TRUE, ncp = NULL, axes=c(1,2), name.group = NULL, level.conf = 0.95, nbsim=500, nbchoix=NULL, cex=1, color=NULL, title=NULL, new.plot=TRUE, graph=c("ind","var","ellipse")){

if (scale) type = rep("s",length(group))
if (!scale) type = rep("c",length(group))
res <- MFA(X,group=group,type=type,ncp=Inf,name.group=name.group,graph=FALSE)
if (new.plot) dev.new()
if ("ind"%in%graph) plot(res,cex=1,title=title,color=color,new.plot=FALSE)
if ("var"%in%graph){
  if ("ind"%in%graph) dev.new()
  plot(res,choix="var",cex=1,title=title,color=color,new.plot=FALSE,habillage="group")
}
result <- list()
result$mfa <- res
if ("ellipse"%in%graph){
  if (("ind"%in%graph)|("var"%in%graph)) dev.new()
  result$ellipse <- boot(X, method = "freechoice", axes = axes, scale = scale, ncp = ncp, group = group, nbsim = nbsim, level.conf = level.conf,
    nbchoix = nbchoix,color = color,cex = cex, title = title, new.plot = FALSE)
}
return(result) 
}
