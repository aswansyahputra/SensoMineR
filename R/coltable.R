#' Color the cells of a data frame according to 4 threshold levels
#' 
#' 
#' Return a colored display of a data frame according to 4 threshold levels.
#' 
#' This function is very useful especially when there are a lot of values to
#' check.
#' 
#' @param matrice a data frame (or a matrix) with only quantitative variables
#' @param col.mat a data frame (or a matrix) from which the cells of the
#' \code{matrice} data frame are colored; by default,
#' \code{col.mat}=\code{matrice}
#' @param nbrow the number of rows to be displayed (by default,
#' \code{nrow(matrice)})
#' @param nbcol the number of columns to be displayed (by default,
#' \code{ncol(matrice)})
#' @param level.lower the threshold below which cells are colored in
#' \code{col.lower}
#' @param col.lower the color used for \code{level.lower}
#' @param level.upper the threshold above which cells are colored in
#' \code{col.upper}
#' @param col.upper the color used for \code{level.upper}
#' @param cex cf. function \code{\link{par}} in the \pkg{graphics} package
#' @param nbdec the number of decimal places displayed
#' @param main.title title of the graph(s)
#' @param level.lower2 the threshold below which cells are colored in
#' \code{col.lower2}; this level should be less than level.lower
#' @param col.lower2 the color used for \code{level.lower2}
#' @param level.upper2 the threshold above which cells are colored in
#' \code{col.upper2}; this level should be greater than level.upper
#' @param col.upper2 the color used for \code{level.upper2}
#' @param novalue boolean, if TRUE the values are not written
#' @author F Husson, S Le
#' @keywords color
#' @examples
#' 
#' ## Example 1
#' data(chocolates)
#' resdecat<-decat(sensochoc, formul = "~Product+Panelist", firstvar = 5,
#'     graph = FALSE)
#' resaverage<-averagetable(sensochoc, formul = "~Product+Panelist", 
#'     firstvar = 5)
#' resaverage.sort = resaverage[rownames(magicsort(resdecat$tabT)),
#'     colnames(magicsort(resdecat$tabT))]
#' coltable(resaverage.sort, magicsort(resdecat$tabT), 
#'     level.lower = -1.96, level.upper = 1.96,
#'     main.title = "Average by chocolate")
#' 
#' ## Example 3
#' \dontrun{
#' data(chocolates)
#' resperf<-paneliperf(sensochoc, 
#'     formul = "~Product+Panelist+Product:Panelist", 
#'     formul.j = "~Product", col.j = 1, firstvar = 5, lastvar = 12, 
#'     synthesis = FALSE, graph = FALSE)
#' resperfprob<-magicsort(resperf$prob.ind, method = "median")
#' coltable(resperfprob, level.lower = 0.05, level.upper = 1, 
#'     main.title = "P-value of the F-test (by panelist)")
#' 
#' resperfr2<-magicsort(resperf$r2.ind, method = "median", 
#'     ascending = FALSE)
#' coltable(resperfr2, level.lower = 0.00, level.upper = 0.85, 
#'     main.title = "Adjusted R-square (by panelist)")
#' }
#' 
#' @export coltable
coltable <-function(matrice,col.mat=matrice,nbrow=nrow(matrice),nbcol=ncol(matrice),level.lower=0.05,col.lower="mistyrose",level.upper=1.96,col.upper="lightblue",cex=0,nbdec=4,main.title=NULL,level.lower2=-1e10,col.lower2="red",level.upper2=1e10,col.upper2="blue",novalue=FALSE) {

################################################################
fill <- function(matrice,col.mat=matrice,nbrow,nbcol,pol,level.lower,col.lower="mistyrose",level.upper,col.upper="lightblue",main.title=NULL,level.lower2,col.lower2,level.upper2,col.upper2){

#cadre
dim1 <- dim(matrice)[1] 
dim2 <- dim(matrice)[2] 
for (i in 0:dim1) rect(0,1-i*(1/(nbrow+1)),1/(nbcol+1),1-(i+1)*(1/(nbrow+1)),col="white",border=NULL)
for (j in 1:(dim2-1)) rect(j*(1/(nbcol+1)),1,(j+1)*(1/(nbcol+1)),1-(1/(nbrow+1)),col="white",border=NULL)

for (j in 1:(dim2-1)){ for (i in 1:dim1){
if (is.na(col.mat[i,j+1])) { rect(j*(1/(nbcol+1)),1-i*(1/(nbrow+1)),(j+1)*(1/(nbcol+1)),1-(i+1)*(1/(nbrow+1)),col="gray",border=NULL) }
  else { if (col.mat[i,j+1]<=level.lower2) { rect(j*(1/(nbcol+1)),1-i*(1/(nbrow+1)),(j+1)*(1/(nbcol+1)),1-(i+1)*(1/(nbrow+1)),col=col.lower2,border=NULL)}
         else {if (col.mat[i,j+1]<=level.lower) { rect(j*(1/(nbcol+1)),1-i*(1/(nbrow+1)),(j+1)*(1/(nbcol+1)),1-(i+1)*(1/(nbrow+1)),col=col.lower,border=NULL)}
             else { if (col.mat[i,j+1]>=level.upper2) { rect(j*(1/(nbcol+1)),1-i*(1/(nbrow+1)),(j+1)*(1/(nbcol+1)),1-(i+1)*(1/(nbrow+1)),col=col.upper2,border=NULL)}
                   else { if (col.mat[i,j+1]>=level.upper) {rect(j*(1/(nbcol+1)),1-i*(1/(nbrow+1)),(j+1)*(1/(nbcol+1)),1-(i+1)*(1/(nbrow+1)),col=col.upper,border=NULL)}
                          else rect(j*(1/(nbcol+1)),1-i*(1/(nbrow+1)),(j+1)*(1/(nbcol+1)),1-(i+1)*(1/(nbrow+1)),col="white",border=NULL)
                   }
             }
         }
  }
}
}
#fill
    dim1 <- dim(matrice)[1]
    dim2 <- dim(matrice)[2]
    for (i in 1:dim1) text((0.5)*(1/(nbcol+1)),1-(i+0.5)*(1/(nbrow+1)),matrice[i,1],cex=pol)
if (!novalue){
    for (i in 1:dim1){
     for (j in 1:(dim2-1)) text((j+0.5)*(1/(nbcol+1)),1-(i+0.5)*(1/(nbrow+1)),matrice[i,j+1],cex=pol)
      }
    }
#titre

    for (j in 0:nbcol) text((j+0.5)*(1/(nbcol+1)),1-(1/(nbrow+1))/2,names(matrice)[j+1],cex=pol)

}
################################################################

################################################################
police <- function(matrice,nbrow,nbcol,nbdec) {
dev.new()
    def.par <- par(no.readonly = TRUE)
    par(mar=c(0,0,2,0))
    plot.new(); title(main=main.title);
    a <- c(rownames(matrice),colnames(matrice))
    nb=NULL
    for (i in 1:nbdec) nb <- paste(nb,"0",sep="")
    nb <- paste(nb,"0.e-00")
    a <- c(a,nb)
    b <- min(nbcol,15)
    return(list(size=(round((1/(b+1))/max(strwidth(a)),2)*100-5)/100,def.par=def.par))
}
################################################################

if (sum(dim(matrice)==dim(col.mat))!=2) stop("The matrices matrice and col.mat should have the same dimensions")
if (level.lower2 > level.lower) stop("level.lower2 should be less than level.lower")
if (level.upper2 < level.upper) stop("level.upper2 should be greater than level.upper")
if (is.numeric(matrice)) matrice <- signif(matrice,nbdec)
matrice=cbind.data.frame(rownames(matrice),matrice)
if (is.numeric(col.mat)) col.mat <- signif(col.mat,nbdec)
col.mat=cbind.data.frame(rownames(col.mat),col.mat)
colnames(matrice)[1]=" "
dim1 <- nrow(matrice)
dim2 <- ncol(matrice)
dim2 <- dim2-1
size <- cex
if (nbrow>dim1){ nbrow <- dim1 }
if (nbcol>dim2){ nbcol <- dim2 }
if (dim2%/%nbcol==dim2/nbcol) {
 for (j in 0:(dim2%/%nbcol-1)) {
    for (i in 0:(dim1%/%nbrow-1)){
        A <- data.frame(matrice[(i*nbrow+1):((i+1)*nbrow),1])
        names(A)=names(matrice)[1]
        B <- matrice[(i*nbrow+1):((i+1)*nbrow),(1+j*nbcol+1):(1+(j+1)*nbcol)]
        B <- cbind(A,B)
        A.col <- data.frame(col.mat[(i*nbrow+1):((i+1)*nbrow),1])
        B.col <- col.mat[(i*nbrow+1):((i+1)*nbrow),(1+j*nbcol+1):(1+(j+1)*nbcol)]
        B.col <- cbind(A.col,B.col)
        if (size==0) {
		  pol <- police(matrice,nbrow,nbcol,nbdec)
		  size <- pol$size
		  def.par <- pol$def.par
		}  else{
          dev.new()
          def.par <- par(no.readonly = TRUE)
          par(mar=c(0,0,2,0))
          plot.new(); title(main=main.title);
        }
        fill(B,B.col,nbrow,nbcol,size,level.lower,col.lower,level.upper,col.upper,main.title=main.title,level.lower2,col.lower2,level.upper2,col.upper2)
		par(def.par)
    }
    if ((dim1%/%nbrow)*nbrow != dim1){
      A<-data.frame(matrice[(dim1%/%nbrow*nbrow+1):dim1,1])
      names(A)=names(matrice)[1]
      B<-data.frame(matrice[(dim1%/%nbrow*nbrow+1):dim1,(1+j*nbcol+1):(1+(j+1)*nbcol)])
      names(B)=names(matrice)[(1+j*nbcol+1):(1+(j+1)*nbcol)]
      B<-cbind(A,B)
      A.col<-data.frame(col.mat[(dim1%/%nbrow*nbrow+1):dim1,1])
      B.col<-data.frame(col.mat[(dim1%/%nbrow*nbrow+1):dim1,(1+j*nbcol+1):(1+(j+1)*nbcol)])
      B.col<-cbind(A.col,B.col)
        if (size==0) {
		  pol <- police(matrice,nbrow,nbcol,nbdec)
		  size <- pol$size
		  def.par <- pol$def.par
		}  else{
         dev.new()
         def.par <- par(no.readonly = TRUE)
         par(mar=c(0,0,2,0))
         plot.new(); title(main=main.title);
        }
      fill(B,B.col,nbrow,nbcol,size,level.lower,col.lower,level.upper,col.upper,main.title=main.title,level.lower2,col.lower2,level.upper2,col.upper2)
      par(def.par)
    }
  }
}
else {
    for (j in 0:(dim2%/%nbcol-1)){ #blocs de descripteurs entiers
      for (i in 0:(dim1%/%nbrow-1)){ #blocs de juges entiers
        A<-data.frame(matrice[(i*nbrow+1):((i+1)*nbrow),1])
        names(A)=names(matrice)[1]
        B<-matrice[(i*nbrow+1):((i+1)*nbrow),(1+j*nbcol+1):(1+(j+1)*nbcol)]
        B<-cbind(A,B)
        A.col<-data.frame(col.mat[(i*nbrow+1):((i+1)*nbrow),1])
        B.col<-col.mat[(i*nbrow+1):((i+1)*nbrow),(1+j*nbcol+1):(1+(j+1)*nbcol)]
        B.col<-cbind(A.col,B.col)
        if (size==0) {
		  pol <- police(matrice,nbrow,nbcol,nbdec)
		  size <- pol$size
		  def.par <- pol$def.par
		}  else{
          dev.new()
          def.par <- par(no.readonly = TRUE)
          par(mar=c(0,0,2,0))
          plot.new(); title(main=main.title);
        }
        fill(B,B.col,nbrow,nbcol,size,level.lower,col.lower,level.upper,col.upper,main.title=main.title,level.lower2,col.lower2,level.upper2,col.upper2)
       par(def.par)
      }
      if ((dim1%/%nbrow)*nbrow != dim1){
        A<-data.frame(matrice[(dim1%/%nbrow*nbrow+1):dim1,1])
        names(A)=names(matrice)[1]
        B<-matrice[(dim1%/%nbrow*nbrow+1):dim1,(1+j*nbcol+1):(1+(j+1)*nbcol)]
        B<-cbind(A,B)
        A.col<-data.frame(col.mat[(dim1%/%nbrow*nbrow+1):dim1,1])
        B.col<-col.mat[(dim1%/%nbrow*nbrow+1):dim1,(1+j*nbcol+1):(1+(j+1)*nbcol)]
        B.col<-cbind(A.col,B.col)
        if (size==0) {
		  pol <- police(matrice,nbrow,nbcol,nbdec)
		  size <- pol$size
		  def.par <- pol$def.par
		}  else{
          dev.new()
          def.par <- par(no.readonly = TRUE)
          par(mar=c(0,0,2,0))
          plot.new(); title(main=main.title);
        }
        fill(B,B.col,nbrow,nbcol,size,level.lower,col.lower,level.upper,col.upper,main.title=main.title,level.lower2,col.lower2,level.upper2,col.upper2)
        par(def.par)
      }
    }
    for (i in 0:(dim1%/%nbrow-1)){#pour les blocs d'individus entiers les variables qui manquent
      A<-data.frame(matrice[(i*nbrow+1):((i+1)*nbrow),1])
      names(A)=names(matrice)[1]
      B<-matrice[(i*nbrow+1):((i+1)*nbrow),(1+dim2%/%nbcol*nbcol):dim2+1]
      if (is.null(dim(B)))    B<-data.frame(B)
      names(B)=names(matrice)[(1+dim2%/%nbcol*nbcol):dim2+1]
      B<-cbind(A,B)
      A.col<-data.frame(col.mat[(i*nbrow+1):((i+1)*nbrow),1])
      B.col<-col.mat[(i*nbrow+1):((i+1)*nbrow),(1+dim2%/%nbcol*nbcol):dim2+1]
      if (is.null(dim(B)))    B.col<-data.frame(B.col)
      B.col<-cbind(A.col,B.col)
        if (size==0) {
		  pol <- police(matrice,nbrow,nbcol,nbdec)
		  size <- pol$size
		  def.par <- pol$def.par
		}  else{
          dev.new()
          def.par <- par(no.readonly = TRUE)
          par(mar=c(0,0,2,0))
          plot.new(); title(main=main.title);
        }
      fill(B,B.col,nbrow,nbcol,size,level.lower,col.lower,level.upper,col.upper,main.title=main.title,level.lower2,col.lower2,level.upper2,col.upper2)
      par(def.par)
    }
    if ((dim1%/%nbrow)*nbrow != dim1){
      A<-data.frame(matrice[(dim1%/%nbrow*nbrow+1):dim1,1]) #les individus qui manquent et les variables qui manquent
      names(A)=names(matrice)[1]
      B<-data.frame(matrice[(dim1%/%nbrow*nbrow+1):dim1,(1+dim2%/%nbcol*nbcol):dim2+1])
      if (is.null(dim(B)))    B<-data.frame(B)
      names(B)=names(matrice)[(1+dim2%/%nbcol*nbcol):dim2+1]
      B<-cbind(A,B)
      A.col<-data.frame(col.mat[(dim1%/%nbrow*nbrow+1):dim1,1]) #les individus qui manquent et les variables qui manquent
      B.col<-data.frame(col.mat[(dim1%/%nbrow*nbrow+1):dim1,(1+dim2%/%nbcol*nbcol):dim2+1])
      if (is.null(dim(B)))    B.col<-data.frame(B.col)
      names(B.col)=names(matrice)[(1+dim2%/%nbcol*nbcol):dim2+1]
      B.col<-cbind(A.col,B.col)
        if (size==0) {
		  pol <- police(matrice,nbrow,nbcol,nbdec)
		  size <- pol$size
		  def.par <- pol$def.par
		}  else{
          dev.new()
          def.par <- par(no.readonly = TRUE)
          par(mar=c(0,0,2,0))
          plot.new(); title(main=main.title)
        }
      fill(B,B.col,nbrow,nbcol,size,level.lower,col.lower,level.upper,col.upper,main.title=main.title,level.lower2,col.lower2,level.upper2,col.upper2)
      par(def.par)
}
  }
  par(def.par)
}
