#' Histogram for each descriptor
#' 
#' Computes automatically histograms for a set of quantitative variables.
#' 
#' Displays histograms with a common Y-axis as well as the local estimator of
#' the density for each descriptor, hence the \code{adjust} parameter to fill
#' in. Displays also the normal distribution with mean and variance the
#' respective values estimated for each descriptor.
#' 
#' @param donnee a data frame
#' @param firstvar the position of the first endogenous variable
#' @param lastvar the position of the last endogenous variable (by default the
#' last column of \code{donnee})
#' @param numr the number of histograms per row (by default 2)
#' @param numc the number of histograms per column (by default 2)
#' @param adjust the bandwidth used is actually 'adjust*bw'. This makes it easy
#' to specify values like "half the default" bandwidth.
#' @author S Le
#' @seealso \code{\link{density}}, \code{\link{hist}}
#' @keywords univar
#' @examples
#' 
#' data(chocolates)
#' histprod(sensochoc, firstvar = 5, lastvar = 10)
#' 
#' @export histprod
histprod<-function(donnee,firstvar,lastvar=ncol(donnee),numr = 2,numc = 2,adjust=1) {

nbdesc <- lastvar-firstvar+1

xquant<-donnee[,firstvar:lastvar]
xrange<-max(apply(xquant,2,max,na.rm=TRUE))
#yrange<-max(hist(donnee[,firstvar],plot=FALSE)$density)
yrange<-max(density(donnee[,firstvar], na.rm = TRUE,adjust=adjust)$y)

for (i in 2:nbdesc){
#yrangeinter<-max(hist(donnee[,i+firstvar-1],plot=FALSE)$density)
yrangeinter<-max(density(donnee[,i+firstvar-1], na.rm = TRUE,adjust=adjust)$y)
yrange<-max(yrange,yrangeinter)
           }


mult <- nbdesc %/% (numr*numc)
if (nbdesc==(nbdesc %/% (numr*numc))*(numr*numc)) mult=mult-1
for (m in 0:mult) {
    par(mfrow = c(numr,numc))
    for (nbd in 1:(numr*numc)) {
          nb <- (m*(numr*numc)+nbd)
          if (nb <= nbdesc)       {

hist(donnee[,nb+firstvar-1],col=grey(0.9),border = grey(0.8),xlab=names(donnee[nb+firstvar-1]),main = paste("Histogram of" , names(donnee[nb+firstvar-1])),xlim=c(0,xrange),ylim=c(0,yrange),proba=TRUE)
step <- seq(from = 0, to = xrange, length = 100)
lines(step, dnorm(step, mean(donnee[,nb+firstvar-1], na.rm = TRUE),sd(donnee[,nb+firstvar-1], na.rm = TRUE)),lty=2)
lines(density(donnee[,nb+firstvar-1], na.rm = TRUE,adjust=adjust), lwd = 1,col="red")
                              } 
                   }
if (m < mult) dev.new()
          }  #for (m in 0:mult) {
                                  }
