#' Estimation of interaction coefficients
#' 
#' Computes automatically the interaction coefficients between two quantitative
#' variables \code{col.p} and \code{col.j} for the following model:
#' \code{"~col.p+col.j+col.p:col.j"}.
#' 
#' In most cases \code{col.p} represents the \emph{product} effect,
#' \code{col.j} represents the \emph{panelist} effect, and the variables of
#' interest are the sensory descriptors. The model considered is the following
#' one: \code{"~Product+Panelist+Product:Panelist"}.\cr Data must be complete
#' (but not necessarily balanced).
#' 
#' @param donnee a data frame made up of at least two qualitative variables
#' (\emph{product}, \emph{panelist}) and a set of quantitative variables
#' (sensory descriptors)
#' @param col.p the position of the \emph{product} effect for instance
#' @param col.j the position of the \emph{panelist} effect for instance
#' @param firstvar the position of the first endogenous variable
#' @param lastvar the position of the last endogenous variable (by default the
#' last column of \code{donnee})
#' @return Returns an array of dimension (\emph{p,j,k}), where \emph{p} is the
#' number of products, \emph{j} the number of panelists and \emph{k} the number
#' of sensory descriptors. The entries of this array are the interaction
#' coefficients between a panelist and a product for a given descriptor. \cr
#' For each sensory descriptor, returns a graph where each (panelist,product)
#' interaction coefficient is displayed, a graph where the contribution to the
#' (panelist,product) interaction coefficient by product is displayed, a graph
#' where the contribution to the (panelist,product) interaction coefficient by
#' panelist is displayed.
#' @author Francois Husson
#' @seealso \code{\link{aov}}
#' @keywords models
#' @examples
#' 
#' \dontrun{
#' data(chocolates)
#' resinteract=interact(sensochoc, col.p = 4, col.j = 1, firstvar = 5)
#' }
#' 
#' @export interact
"interact" <- function(donnee,col.p=1,col.j,firstvar,lastvar=ncol(donnee)){


############################################################################
plotinteract<-function(tab,cex=1.1,xlegend=ncol(tab)-5,ylegend=max(tab),xlab=NULL,ylab=NULL,main=NULL){
  x <- as.factor(1:ncol(tab))
  miny <- min(tab,na.rm=TRUE)
  maxy <- max(tab,na.rm=TRUE)
  plot(as.integer(x),apply(tab,2,mean),type="n",main=main,xlab=xlab,ylab=ylab,ylim=c(miny,maxy),xlim=c(1,ncol(tab)),cex=0.8)
  abline(v = x, lty = "dotted")
  abline(h = 0)
  for (i in 1:nrow(tab)) points(x,tab[i,],col=i,cex=cex,pch=20)
  legend("topright",legend=rownames(tab),col=1:nrow(tab),pch=rep(20,nrow(tab)),cex=0.8,bg="white")
}
############################################################################
old.contr = options()$contrasts
options(contrasts=c("contr.sum", "contr.sum"))
for (j in 1 :(firstvar-1))  donnee[,j] <- as.factor(donnee[,j])
nbprod <- length(levels(donnee[,col.p]))
nbjuge <- length(levels(donnee[,col.j]))
tab<-array(0,dim=c(nbprod,nbjuge,lastvar-firstvar+1))

for (varendo in firstvar:lastvar) {
  aux <- summary.lm(aov( donnee[,varendo]~donnee[,col.p]+donnee[,col.j]+donnee[,col.p]:donnee[,col.j], data = donnee, na.action =na.exclude))$coef
  for (k in 1:(nbjuge-1)) tab[1:(nbprod-1),k,varendo-firstvar+1] <- aux[((nbprod+nbjuge-1)+(k-1)*(nbprod-1)+1):((nbprod+nbjuge-1)+k*(nbprod-1)),1]
  tab[,nbjuge,varendo-firstvar+1] <- - apply(tab[,,varendo-firstvar+1],1,sum)
  tab[nbprod,,varendo-firstvar+1] <- - apply(tab[,,varendo-firstvar+1],2,sum)
}

dimnames(tab) = list(levels(donnee[,col.p]),levels(donnee[,col.j]),labels(donnee)[[2]][firstvar:lastvar])

for (k in 1:dim(tab)[[3]]){
  plotinteract(tab[,,k],main=colnames(donnee)[firstvar+k-1],xlab=colnames(donnee)[col.j],ylab=paste(colnames(donnee)[col.p],"-",colnames(donnee)[col.j],"interaction coefficients"))
  if (k != dim(tab)[[3]]) dev.new()
}
barrow(t(apply(tab^2,c(1,3),sum)/matrix(rep(apply(tab^2,3,sum),nrow(tab)),byrow=TRUE,nrow=nrow(tab))),color="orange")

## Make a graph to visualize the panelist which contribute the product-panelist interaction for each descriptor
barrow(t(  apply(tab^2,c(2,3),sum) /matrix(rep(apply(tab^2,3,sum),ncol(tab)),byrow=TRUE,nrow=ncol(tab))))
return(tab)
options(contrasts=old.contr)
}
