#' Coordinates of individuals and illustrative individuals for PCA or MFA
#' 
#' This function is especially designed to be used in a sensory data analysis
#' context. Returns the coordinates of the products when performing either PCA
#' or MFA and the coordinates of the "partial" products when performing MFA.
#' Returns also the panelists' coordinates when projected as illustrative rows
#' onto the products' space. Produces graphs of products and descriptors from
#' the output of PCA or MFA.
#' 
#' 
#' The input data set is an object of class \code{data.frame}, for which the
#' two first columns are qualitative variables (the first variable refers to
#' the \emph{panelist} variable and the second to the \emph{product} variable)
#' and the others are quantitative. \cr
#' 
#' The ouptut of this function is a list with one element when performing PCA
#' and two elements when performing MFA. The first element is the data frame of
#' the coordinates of the products according to the whole panel (Panelist=0)
#' and to the panelists.  The second element is the data frame of the
#' coordinates of the "partial products" according to the whole panel
#' (Panelist=0) and to the panelists. \cr
#' 
#' This function is necessary when calculating confidence ellipses for
#' products.
#' 
#' @param matrice a data.frame made up of at least two qualitative variables
#' (the \emph{panelist} and the \emph{product} variables), the others are
#' sensory descriptors used to perform an MFA or a PCA if group = NULL
#' @param coord a length 2 vector specifying the components to plot
#' @param scale.unit boolean, if \code{TRUE} the descriptors are scaled to unit
#' variance
#' @param group the number of variables in each group of variables when
#' multiple factor analysis is performed (by default this parameter equals NULL
#' and a PCA is performed)
#' @param name.group the names of the groups of variables when mfa is performed
#' (if \code{group} differs from NULL)
#' @param centerbypanelist center the data by panelist before the construction
#' of the axes
#' @param scalebypanelist scale the data by panelist before the construction of
#' the axes
#' @param method the method to replace the missing values: "average" or "coeff"
#' (coefficients of the \emph{product} variable in the anova model)
#' @return A list containing the following elements: \item{eig}{a matrix with
#' the component of the factor analysis (in row) and the eigenvalues, the
#' inertia and the cumulative inertia for each component} \item{moyen}{the
#' coordinates of the products with respect to the panel and to each panelists}
#' \item{partiel}{the coordinates of the \emph{partial} products with respect
#' to the panel and to each panelists} Returns also a correlation circle as
#' well as a graph of individuals
#' @author Francois Husson
#' @seealso \code{\link[FactoMineR]{MFA}}
#' @references Escofier, B. and Pages, J. (1990) \emph{Analyses factorielles
#' simples et multiples: objectifs, methodes et interpretation} Dunod, Paris.
#' 1--267. \cr
#' 
#' Escofier, B. and Pages, J. (1994) Multiple factor analysis (AFMULT package).
#' \emph{Computational Statistics and Data Analysis}, \bold{18}, 121--140.
#' @keywords multivariate
#' @examples
#' 
#' ## Example1: PCA 
#' data(chocolates)
#' donnee <- cbind.data.frame(sensochoc[,c(1,4,5:18)])
#' axe <- construct.axes(donnee, scale.unit = TRUE)
#'  
#' ## Example2: MFA (two groups of variables)
#' data(chocolates) 
#' donnee <- cbind.data.frame(sensochoc[,c(1,4,5:18)])
#' axe <- construct.axes(donnee, group = c(6,8), 
#'     name.group = c("A-F","T-S"),scale.unit = TRUE)
#' 
#' @export construct.axes
"construct.axes" <- function(matrice,coord=c(1,2),scale.unit=TRUE,group=NULL,name.group=NULL,centerbypanelist=FALSE,scalebypanelist=FALSE,method="coeff"){

  nbcoord=max(coord)
  oo <- order(matrice[,2])
  matrice <- matrice[oo,]
  oo <- order(matrice[,1])
  matrice <- matrice[oo,]

  nbjuge <- nlevels(matrice[,1])
  if (0%in% summary(matrice[,1])) nbjuge <- nbjuge-1
  nbprod <- length(levels(matrice[,2]))
  nbdesc <- ncol(matrice)-2

  moy.aux=scalebypanelist(matrice,col.j=1,col.p=2,firstvar=3,center=centerbypanelist,scale=scalebypanelist,method=method)
  rownames(moy.aux) <- paste("i",1:nrow(moy.aux),sep="")
  rownames(moy.aux)[1:nbprod] <- as.character(moy.aux[1:nbprod,2])
  ###AF with active data the averages for all the panelist 
  axe <- list()
  if (is.null(group)){
    res.af <- PCA(moy.aux[,-c(1,2)],ind.sup = (nbprod+1):nrow(moy.aux), scale.unit = scale.unit, ncp = nbcoord,graph=FALSE)
    axe$moyen <- data.frame(rbind(res.af$ind$coord,res.af$ind.sup$coord),as.factor(moy.aux[,2]),as.factor(moy.aux[,1]))
    plot(res.af,choix="var",axes=coord)
  }
  else {
    if (scale.unit) res.af <- MFA(moy.aux[,-c(1,2)],ind.sup = (nbprod+1):nrow(moy.aux), group = group, name.group = name.group, type = rep("s",length(group)), ncp = nbcoord,graph=FALSE)
    else res.af <- MFA(moy.aux[,-c(1,2)],ind.sup = (nbprod+1):nrow(moy.aux), group = group, name.group = name.group, type = rep("c",length(group)), ncp = nbcoord,graph=FALSE)
    axe$moyen <- data.frame(rbind(res.af$ind$coord,res.af$ind.sup$coord),as.factor(moy.aux[,2]),as.factor(moy.aux[,1]))
    axe$partiel <- data.frame(rbind(t(matrix(t(as.matrix(res.af$ind$coord.partiel)),nrow=nbcoord*length(group),byrow=FALSE)),t(matrix(t(as.matrix(res.af$ind.sup$coord.partiel)),nrow=nbcoord*length(group),byrow=FALSE))),as.factor(moy.aux[,2]),as.factor(moy.aux[,1]))
    dimnames(axe$partiel)[2][[1]][(dim(axe$partiel)[2]-1):dim(axe$partiel)[2]] <- c("Product","Panelist")
    for (i in 1:length(group)) dimnames(axe$partiel)[2][[1]][((i-1)*nbcoord+1):(i*nbcoord)]<-paste("Dim", 1:nbcoord, sep = "",name.group[i])
    plot(res.af,choix="var",habillage="group",axes=coord)
  }
  plot(res.af,choix="ind",invisible="ind.sup",axes=coord)
  dev.new()
	plot(res.af,choix="var",axes=coord)
  dimnames(axe$moyen)[2][[1]]<-c (paste("Dim", 1:nbcoord, sep = ""),"Product","Panelist")
  axe$eig = res.af$eig
  return(axe)
}
