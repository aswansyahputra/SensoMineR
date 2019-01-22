#' Graphical display of the interaction between two qualitative variables
#' 
#' This function is designed to display the interaction between two qualitative
#' variables, in most cases the \emph{product} and the \emph{session}
#' variables.
#' 
#' The data set must be balanced (or not unbalanced too much).
#' 
#' @param donnee a data frame made up of at least two qualitative variables
#' (\emph{product}, \emph{panelist}) and a set of quantitative variables
#' (sensory descriptors)
#' @param col.p the position of one categorical variables of interest (the
#' \emph{product} variable)
#' @param col.j the position of one categorical variables of interest (the
#' \emph{session} variable)
#' @param firstvar the position of the first endogenous variable
#' @param lastvar the position of the last endogenous variable (by default the
#' last column of \code{donnee})
#' @param numr the number of graphs per row (by default 2)
#' @param numc the number of graphs per column (by default 2)
#' @return If the variables of interest are the \emph{product} and the
#' \emph{session} variables, a list containing the following components:
#' \item{prod}{a data frame of dimension (\emph{p,q}), the means over the
#' panelists and the sessions for the \emph{p} products and the \emph{q}
#' sensory descriptors} \item{seance}{as many matrices of dimension
#' (\emph{p,q}) as there are sessions, the means over the panelists for the
#' \emph{p} products, the \emph{q} sensory descriptors and for each session}
#' The graphical display of the interaction for each sensory descriptor.
#' @author F Husson, S Le
#' @seealso \code{\link{aov}}
#' @references P. Lea, T. Naes, M. Rodbotten. \emph{Analysis of variance for
#' sensory data}. \cr H. Sahai, M. I. Ageel. \emph{The analysis of variance}.
#' @keywords models
#' @examples
#' 
#' \dontrun{
#' data(chocolates)
#' graphinter(sensochoc, col.p = 4, col.j = 2, firstvar = 5, lastvar = 12,
#'     numr = 1, numc = 1)
#' }
#' 
#' @export graphinter
graphinter <- function(donnee, col.p, col.j, firstvar, lastvar=ncol(donnee), numr=2,numc=2) {

     for (j in 1:(firstvar-1)) donnee[,j] <- as.factor(donnee[,j])
     nbprod <- length(levels(donnee[,col.p]))
     nbdesc <- lastvar-firstvar+1
     nbseance <- length(levels(donnee[,col.j]))
     labprod <- levels(donnee[,col.p])
          
#     moy <- matrix(0,nbprod,nbdesc)
#     for (p in 1:nbprod) moy[p,] <- mean(donnee[(donnee[,col.p]==levels(donnee[,col.p])[p]),(firstvar:lastvar)],na.rm=TRUE)
	 moy <- matrix(unlist(by(donnee[,firstvar:lastvar,drop=FALSE],donnee[,col.p],apply,2,mean,na.rm=TRUE)),nrow=nlevels(donnee[,col.p]),byrow=TRUE)
     dimnames(moy) <- list(labprod,colnames(donnee)[firstvar:lastvar])

     moyS <- array(0,c(nbprod,nbdesc,nbseance))
#     for (p in 1: nbprod) {
         for (s in 1:nbseance) {
#           moyS[p,,s] <- mean(donnee[((donnee[,col.p]==levels(donnee[,col.p])[p])&(donnee[,col.j]==levels(donnee[,col.j])[s])),(firstvar:lastvar)],na.rm=TRUE) 
           moyS[,,s] <- matrix(unlist(by(donnee[donnee[,col.j]==levels(donnee[,col.j])[s],firstvar:lastvar,drop=FALSE],donnee[donnee[,col.j]==levels(donnee[,col.j])[s],col.p],apply,2,mean,na.rm=TRUE)),nrow=nlevels(donnee[,col.p]),byrow=TRUE)
#         }
     }
     dimnames(moyS) <- list(levels(donnee[,col.p]),colnames(donnee)[firstvar:lastvar],levels(donnee[,col.j]))

mult <- nbdesc %/% (numr*numc)
if (nbdesc==(nbdesc %/% (numr*numc))*(numr*numc)) mult=mult-1
for (m in 0:mult) {
    par(mfrow = c(numr,numc))
    for (nbd in 1:(numr*numc)) {
          nb <- (m*(numr*numc)+nbd)
          if (nb <= nbdesc) {
              xmin = ymin <- min(moy[,nb],moyS[,nb,],na.rm=TRUE)-0.2
              xmax = ymax <- max(moy[,nb],moyS[,nb,],na.rm=TRUE)+0.2
            for (s in 1:nbseance) {
                if (s==1) {
                  plot(moy[order(moy[,nb]),nb],moyS[order(moy[,nb]),nb,s],type="o",xlab=paste("Mean on the whole ",colnames(donnee)[col.j],"s",sep=""), ylab=paste("Mean per ",colnames(donnee)[col.j],sep=""),
                     cex.lab = 0.8, asp = 1, pch = 20, xlim = c(xmin,xmax), ylim = c(ymin,ymax), col = "violetred4")
                  for (i in 1:nrow(moy))  text(moy[i,nb],max(moyS[i,nb,],na.rm=TRUE),label=labprod[i], pos = 3,offset = 0.4, font = 1)
                }
                else  points(moy[order(moy[,nb]),nb],moyS[order(moy[,nb]),nb,s],type="o",pch=20,col=s)
              title(colnames(moy)[nb])
            }
        } 
    
  legend("topleft", legend = paste(colnames(donnee)[col.j],levels(donnee[, col.j]), sep = " "),
               text.col = 1:length(levels(donnee[, col.j])),cex = 0.8, bg = "white")     
    }
if (m < mult) dev.new()
}
moyenne <- list()
moyenne$col.p <- moy
moyenne$col.j <- moyS
return(moyenne)
}
