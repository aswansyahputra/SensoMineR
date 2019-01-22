#' Returns a sorted data matrix
#' 
#' Sort the rows and columns of a matrix in a "magic" order or by ascending (or
#' descending) mean or median or geometrical mean.
#' 
#' Very useful function to compare results.
#' 
#' @param matrice a data matrix to sort
#' @param sort.mat sort the rows and columns according to the result of the PCA
#' made on this matrix (by default the \code{matrice})
#' @param method four types of calculations, magic (\code{"magic"}),
#' (\code{"median"}), arithmetical (\code{"mean"}) or geometrical
#' (\code{"geo"}) mean (by default magic)
#' @param byrow boolean, if TRUE then data are sorted over the rows
#' @param bycol boolean, if TRUE then data are sorted over the columns
#' @param ascending boolean, if TRUE then data are sorted in ascending order
#' @author F Husson, S Le
#' @keywords manip
#' @examples
#' 
#' ## Example 1
#' data(chocolates)
#' resdecat<-decat(sensochoc, formul = "~Product", firstvar = 5, 
#'     graph = FALSE)
#' coltable(magicsort(resdecat$tabT), level.lower = -1.96, 
#'     level.upper = 1.96, main.title = "Products' description")
#' 
#' ## Example 2
#' data(chocolates)
#' resperf<-paneliperf(sensochoc, 
#'     formul = "~Product+Panelist+Product:Panelist",
#'     formul.j = "~Product", col.j = 1, firstvar = 5, lastvar = 12,
#'     synthesis = FALSE, graph = FALSE)
#' res.sort=magicsort(resperf$prob.ind, method = "median")
#' coltable(res.sort, main.title = "P-values of the F-test by panelist")
#' 
#' @export magicsort
"magicsort" <- function(matrice,sort.mat=matrice,method="magic",byrow=TRUE,bycol=TRUE,ascending=TRUE){

  if ((method!="magic")&(method!="mean")&(method!="geo")&(method!="median")) stop("The method is unknown")
  if (length(dim(sort.mat))==0){         # Useful if there is only one columns
    if ((length(sort.mat) == ncol(matrice))&(length(sort.mat) == nrow(matrice))) {
      if (byrow) sort.mat = cbind(sort.mat)
      if (bycol) sort.mat = rbind(sort.mat)
    }
    else {
      if (length(sort.mat) == ncol(matrice)) sort.mat=rbind(sort.mat)
      if (length(sort.mat) == nrow(matrice)) sort.mat=cbind(sort.mat)
    }
  }
  if ((nrow(sort.mat)!=nrow(matrice))&(byrow)) stop("Number of rows must be the same in matrice and in sort.mat: you can use the option byrow=FALSE")
  if ((ncol(sort.mat)!=ncol(matrice))&(bycol)) stop("Number of columns must be the same in matrice and in sort.mat: you can use the option bycol=FALSE")
  if (method=="magic"){
    for (j in 1:ncol(sort.mat)) sort.mat[,j] <- replace(sort.mat[,j],is.na(sort.mat[,j]),mean(sort.mat[,j],na.rm=TRUE))
    res.pca <- PCA(sort.mat,ncp=2,graph=FALSE)
  }
  if (byrow==TRUE) {
    if (method=="geo") matrice=cbind(matrice,exp(apply(log(sort.mat),1,mean,na.rm=TRUE)))
    if (method=="mean") matrice=cbind(matrice,apply(sort.mat,1,mean,na.rm=TRUE))
    if (method=="median") matrice=cbind(matrice,apply(sort.mat,1,median,na.rm=TRUE))
    if (method=="magic") matrice=cbind(matrice,res.pca$ind$coord[,1])
    oo=order(matrice[,ncol(matrice)])
    if (ascending==TRUE) matrice=matrice[oo,-ncol(matrice)]
    if (ascending!=TRUE) matrice=matrice[rev(oo),-ncol(matrice)]
  }
  if (bycol==TRUE){
    matrice=t(matrice)
    sort.mat=t(sort.mat)
    if (method=="geo") matrice=cbind(matrice,exp(apply(log(sort.mat),1,mean,na.rm=TRUE)))
    if (method=="mean") matrice=cbind(matrice,apply(sort.mat,1,mean,na.rm=TRUE))
    if (method=="median") matrice=cbind(matrice,apply(sort.mat,1,median,na.rm=TRUE))
    if (method=="magic") matrice <- cbind(matrice,res.pca$var$coord[,1])
    oo=order(matrice[,ncol(matrice)])
    if (ascending==TRUE) matrice=matrice[oo,]
    if (ascending!=TRUE) matrice=matrice[rev(oo),]
    matrice <- t(matrice[,-ncol(matrice)])
  }
  if (byrow==TRUE) {
    if (method=="geo") matrice=cbind(matrice,exp(apply(log(matrice),1,mean,na.rm=TRUE)))
    if (method=="mean") matrice=cbind(matrice,apply(matrice,1,mean,na.rm=TRUE))
    if (method=="median") matrice=cbind(matrice,apply(matrice,1,median,na.rm=TRUE))
    if (method=="mean") colnames(matrice)[ncol(matrice)]="mean"
    if (method=="median") colnames(matrice)[ncol(matrice)]="median"
    if (method=="geo") colnames(matrice)[ncol(matrice)]="geo"
  }
  if (bycol==TRUE) {
    if (method=="geo") matrice=rbind(matrice,exp(apply(log(matrice),2,mean,na.rm=TRUE)))
    if (method=="geo") rownames(matrice)[nrow(matrice)]="geo"
    if (method=="mean") matrice=rbind(matrice,apply(matrice,2,mean,na.rm=TRUE))
    if (method=="mean") rownames(matrice)[nrow(matrice)]="mean"
    if (method=="median") matrice=rbind(matrice,apply(matrice,2,median,na.rm=TRUE))
    if (method=="median") rownames(matrice)[nrow(matrice)]="median"
  }
return(matrice)
}
