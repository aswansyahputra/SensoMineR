#' Barplot per row with respect to a set of quantitative variables
#' 
#' Returns as many barplots as there are rows in a matrix. The barplots are
#' automatically generated for all the quantitative variables.
#' 
#' Missing values are ignored when forming barplots.
#' 
#' @param donnee a data frame of dimension (\emph{p,q}), where p is the number
#' of products and \emph{q} is the number of sensory descriptors for instance
#' @param numr the number of barplots to be displayed per row (by default 2)
#' @param numc the number of barplots to be displayed per column (by default 2)
#' @param numchar the number of character used to write the boxplot labels (by
#' default 8)
#' @param color the color of the barplots (by default "lightblue")
#' @param title the title used in the graphs
#' @author S Le \email{Sebastien.Le@@agrocampus-rennes.fr}
#' @seealso \code{\link{plot}}
#' @references Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The
#' New S Language}. Wadsworth & Brooks/Cole.\cr Chambers, J. M., Cleveland, W.
#' S., Kleiner, B. and Tukey, P. A. (1983) \emph{Graphical Methods for Data
#' Analysis}. Wadsworth & Brooks/Cole.
#' @keywords univar
#' @examples
#' 
#' data(chocolates)
#' resdecat<-decat(sensochoc, formul = "~Product+Panelist", firstvar = 5, 
#'     graph = FALSE)
#' \dontrun{
#' barrow(resdecat$tabT)
#' barrow(resdecat$coeff, color = "orange")
#' }
#' 
#' @export barrow
barrow<-function(donnee,numr = 2,numc = 2,numchar=8,color="lightblue",title=NULL) {
dev.new()
nbprod <- dim(donnee)[1]

mult <- nbprod %/% (numr*numc)

if (nbprod==(nbprod%/% (numr*numc))*(numr*numc)) mult=mult-1
for (m in 0:mult) {
    par(las=3)
    par(mfrow = c(numr,numc))

    for (nbd in 1:(numr*numc)) {
    nb <- (m*(numr*numc)+nbd)
    if (nb <= nbprod){
        if (length(title)==0){
          main=paste(rownames(donnee)[nb])
          subtitle=NULL
        }
        else {
          main=title
          subtitle=paste(rownames(donnee)[nb])
        }
      barplot(donnee[nb,],width=1,col=color,ylim=c(min(0,min(donnee,na.rm=TRUE)),max(donnee,na.rm=TRUE)),border="black",main=main,sub=subtitle,cex.names=0.8,names.arg=substr(colnames(donnee),1,numchar))
    }}
if (m < mult) dev.new()
          }
par(las=0)
}
