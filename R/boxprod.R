#' Boxplot per category with respect to a categorical variable and a set of
#' quantitative variables
#' 
#' Returns as many boxplots as there are categories for a given categorical
#' variable of interest (in most cases, the \emph{product} variable). The
#' boxplots are automatically generated for all the quantitative variables (in
#' our type of applications, variables are often sensory descriptors).
#' 
#' Missing values are ignored when forming boxplots.
#' 
#' @param donnee a data frame
#' @param col.p the position of the categorical variable of interest
#' @param firstvar the position of the first endogenous variable
#' @param lastvar the position of the last endogenous variable (by default the
#' last column of \code{donnee})
#' @param numr the number of boxplots per row (by default 2)
#' @param numc the number of boxplots per column (by default 2)
#' @author F Husson \email{husson@@agrocampus-ouest.fr} \cr S Le
#' \email{Sebastien.Le@@agrocampus-ouest.fr}
#' @seealso \code{\link{boxplot}} which does the computation, \code{\link{bxp}}
#' for the plotting and more examples; and \code{\link{stripchart}} for an
#' alternative (with small data sets).
#' @references Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The
#' New S Language}. Wadsworth & Brooks/Cole.\cr Chambers, J. M., Cleveland, W.
#' S., Kleiner, B. and Tukey, P. A. (1983) \emph{Graphical Methods for Data
#' Analysis}. Wadsworth & Brooks/Cole.
#' @keywords univar
#' @examples
#' 
#' data(chocolates)
#' boxprod(sensochoc, col.p = 4, firstvar = 5, numr = 2, numc = 2)
#' 
#' @export boxprod
boxprod<-function(donnee,col.p,firstvar,lastvar=ncol(donnee),numr = 2,numc = 2) {

nbdesc <- lastvar-firstvar+1

mult <- nbdesc %/% (numr*numc)

if (nbdesc==(nbdesc %/% (numr*numc))*(numr*numc)) mult=mult-1
for (m in 0:mult) {
    par(mfrow = c(numr,numc))

    for (nbd in 1:(numr*numc)) {
    nb <- (m*(numr*numc)+nbd)
          if (nb <= nbdesc)       {
boxplot(donnee[,nb+firstvar-1]~donnee[,col.p],col="orchid3",main = names(donnee[nb+firstvar-1]),boxfill= "light gray",outpch = 21:25, outlty = 2,bg = "pink",lwd = 0.5, medcol = "dark blue", medcex = 1, medpch=15)
                              } 
                   }
if (m < mult) dev.new()
          }  #for (m in 0:mult) {

                                  }
