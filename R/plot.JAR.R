#' Make penalty graph
#' 
#' Plot the graphs for the penalty analysis.
#' 
#' 
#' @param x an object of class fast
#' @param name.prod a string corresonding to the name of the product for which
#' penalty is drawn
#' @param model an integer: 1 means that the one-dimensional penalties are
#' drawn, 2 means that the multidimensional penalties are drawn
#' @param confidence A boolean: if true, the confidence interval is drawn for
#' each penalty
#' @param level threshold for the selection of the categories that are plotted
#' @param \dots further arguments passed to or from other methods, such as cex,
#' cex.main, ...
#' @return Draw a plot of the penalty against the ferquency of the categories.
#' @author Francois Husson
#' @seealso \code{\link{JAR}}
#' @keywords dplot
#' @examples
#' 
#' \dontrun{
#' data(JAR)
#' res.jar <- JAR(JAR,col.p=13,col.j=1,col.pref=2)
#' plot.JAR(res.jar,name.prod="284", model=1)
#'  }
#' 
#' @export plot.JAR
plot.JAR <- function(x, name.prod, model=1, confidence=TRUE, level=0.05,...){
  if (!inherits(x, "JAR"))  stop("x must be an object of class JAR")
  if (model==1 || model=="one") penal <- x$penalty1
  else penal <- x$penalty2
  nbmodtot<-nrow(penal)
  coord=matrix(NA,nrow=nbmodtot,ncol=6)
  nomoo=rep("a",6)
  nomoo=c(paste("Frequency for product",name.prod),colnames(penal),"Yinf","Ysup")
  colnames(coord)=nomoo
  rownames(coord)= rownames(penal)
  coord[1:nbmodtot,1:4]=cbind(x$Frequency[,name.prod],penal)
  coord[,5]=coord[,2]-qnorm(1-0.05/2)*coord[,3]
  coord[,6]=coord[,2]+qnorm(1-0.05/2)*coord[,3]
#
  plot(coord[coord[,4]<level,1:2],main=paste("Penalty for product" ,name.prod) ,xlim=c(0,100), ylim=c(min(0,coord[,5]), max(coord[,6])))
  text(coord[coord[,4]<level,1:2], rownames(coord)[coord[,4]<level],pos=3,offset=.3,cex=.8)
  if (confidence){
   for(lig in 1:nbmodtot){
    if(coord[lig,4]<level){
     lines(c(coord[lig,1],coord[lig,1]),c(coord[lig,5],coord[lig,6]))
     tiret=1
     ainf=coord[lig,1]-tiret
     asup=coord[lig,1]+tiret
     lines(c(ainf,asup),c(coord[lig,5],coord[lig,5]))
     lines(c(ainf,asup),c(coord[lig,6],coord[lig,6]))
   }
  }
 }
}
