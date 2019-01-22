#' Preference Mapping Techniques
#' 
#' Performs preference mapping techniques based on multidimensional exploratory
#' data analysis.
#' 
#' The preference mapping methods are commonly used in the fields of market
#' research and research and development to explore and understand the
#' structure and tendencies of consumer preferences, to link consumer
#' preference information to other data and to predict the behavior of
#' consumers in terms of acceptance of a given product.\cr This function refers
#' to the method introduced by M. Danzart. A response surface is computed per
#' consumer; then according to certain threshold preference zones are delimited
#' and finally superimposed.
#' 
#' @param Mat a data frame corresponding to the axes of the map
#' @param MatH a data frame in which each row represent a product and each
#' column represent the hedonic scores of a given consumer for the products
#' @param level the number of standard deviations used in the calculation of
#' the preference response surface for all the consumers
#' @param regmod the type of regression model used in the calculation of the
#' preference response surface for all the consumers. \code{regmod} = 1:
#' quadratic model, \code{regmod} = 2: vector model, \code{regmod} = 3:
#' circular model, \code{regmod} = 4: elliptical model
#' @param coord a vector of length 2, the rank of the axis used to display the
#' results if \code{"manual"} is not assigned to the \code{option} parameter
#' @param asp if 1 is assigned to that parameter, the graphic displays are
#' output in an orthonormal coordinate system
#' @param cex cf. function \code{\link{par}} in the \pkg{graphics} package
#' @param col cf. function \code{\link{par}} in the \pkg{graphics} package
#' @param font cf. function \code{\link{par}} in the \pkg{graphics} package
#' @param clabel cf. the \pkg{ade4} package
#' @param label.j boolean, if T then the labels of the panelists who gave the
#' hedonic scores are displayed
#' @param resolution resolution of the map
#' @param nb.clusters number of clusters to use (by default, 0 and the optimal
#' numer of clusters is calculated
#' @param graph.tree boolean, if TRUE plots the tree in 2 dimensions
#' @param graph.corr boolean, if TRUE plots the variables factor map
#' @param graph.carto boolean, if TRUE plots the preference map
#' @param main an overall title for the plot
#' @param col.min define the color which match to the low levels of preference
#' @param col.max define the color which match to the high levels of preference
#' @author Francois Husson \email{husson@@agrocampus-ouest.fr} \cr Sebastien Le
#' \email{Sebastien.Le@@agrocampus-ouest.fr}
#' @seealso \code{\link[FactoMineR]{MFA}}, \code{\link[FactoMineR]{GPA}}
#' @references
#' 
#' Danzart M., Sieffermann J.M., Delarue J. (2004). New developments in
#' preference mapping techniques: finding out a consumer optimal product, its
#' sensory profile and the key sensory attributes.  \emph{7th Sensometrics
#' Conference, July 27-30, 2004, Davis, CA.}\cr
#' @keywords multivariate models
#' @examples
#' 
#' \dontrun{
#' ## Example 1: carto for the sensory descriptors
#' data(cocktail)
#' res.pca <- PCA(senso.cocktail)
#' res.carto <- carto(res.pca$ind$coord[,1:2], hedo.cocktail)
#' 
#' ## Example 2
#' data(cocktail)
#' res.mfa <- MFA(cbind.data.frame(senso.cocktail,compo.cocktail),
#'     group=c(ncol(senso.cocktail),ncol(compo.cocktail)),
#'     name.group=c("senso","compo"))
#' res.carto <- carto(res.mfa$ind$coord[,1:2], hedo.cocktail)
#' }
#' 
#' @export carto
carto <- function (Mat, MatH, level = 0, regmod = 1,   
    	coord = c(1, 2), asp = 1, cex = 1.3, col = "steelblue4", font = 2, clabel = 0.8,label.j=FALSE,resolution=200,nb.clusters=0,
		  graph.tree=TRUE,graph.corr=TRUE,graph.carto=TRUE,main=NULL,col.min=7.5,col.max=0)
{
#col : 0=red 
#	1=orange
#	2=red
#	3=green
#	4=green
#	5=grenn/blue
#	6=ligth blue
#	7=blue
#	8=dark blue
#	9=purple
#	10=pink
#	11=pink
# 	12=red

cm.colors2=function (n, alpha = 1) {
    	if ((n <- as.integer(n[1L])) > 0) {
        	even.n <- n%%2 == 0
        	k <- n%/%2
        	l1 <- k + 1 - even.n
        	l2 <- n - k + even.n
        	c(if (l1 > 0) hsv(h = col.min/12, s = seq.int(0.8, ifelse(even.n, 
            	0.5/k, 0), length.out = l1), v = 1, alpha = alpha), 
            if (l2 > 1) hsv(h = col.max/12, s = seq.int(0, 0.8, length.out = l2)[-1L], 
                v = 1, alpha = alpha))
    	}
    	else character(0L)
}

#    op <- par(no.readonly = TRUE)
#    on.exit(par(op))
    predire <- function(n1, n2, coeff) {
        coeff[1] + coeff[2] * n1 + coeff[3] * n2 + coeff[4] *
            n1 * n1 + coeff[5] * n2 * n2 + coeff[6] * n1 * n2
    }
    if (!is.data.frame(MatH)) stop("Non convenient selection for MatH")
	if (any(is.na(MatH))){
	  missing <- which(is.na(MatH))
	  MatH[missing] <- (matrix(rep(apply(MatH,1,mean,na.rm=T),ncol(MatH)),ncol=ncol(MatH))
	  +matrix(rep(apply(MatH,2,mean,na.rm=T),each=nrow(MatH)),ncol=ncol(MatH))-matrix(rep(mean(MatH,na.rm=TRUE),ncol(MatH)*nrow(MatH)),ncol=ncol(MatH)))[missing]
	}
    matrice <- cbind(row.names(MatH), Mat[rownames(MatH),],MatH)
    classif <- cluster::agnes(dist(t(scale(MatH,scale=FALSE))),method="ward")
if(graph.tree){
    dev.new()
    plot(classif,main="Cluster Dendrogram",xlab="Panelists",which.plots=2)
}
    if (nb.clusters==0){
       classif2 <- as.hclust(classif)
       nb.clusters = which.max(rev(diff(classif2$height))) + 1
    }
    aux=kmeans(t(MatH),centers=nb.clusters)$cluster
    mat <- matrix(0,nb.clusters,nrow(MatH))
    dimnames(mat) <- list(1:nb.clusters,rownames(MatH))
    for (i in 1:nb.clusters){
      mat[i,] <- apply(t(MatH[,aux==i]),2,mean)
      rownames(mat)[i] <- paste("cluster",i)
    } 
    ab=cor(t(mat),matrice[,2:3],use="pairwise.complete.obs")
    aa=cor(matrice[,4:ncol(matrice)],matrice[,2:3],use="pairwise.complete.obs")
if(graph.corr){
    dev.new()
    plot(0,0,xlab=paste("Dim",coord[1]),ylab=paste("Dim",coord[2]),xlim=c(-1,1),ylim=c(-1,1),col="white",asp=1,main="Correlation circle")
    x.cercle <- seq(-1, 1, by = 0.01)
    y.cercle <- sqrt(1 - x.cercle^2)
    lines(x.cercle, y = y.cercle)
    lines(x.cercle, y = -y.cercle)
    abline(v=0,lty=2, cex=cex)
    abline(h=0,lty=2, cex=cex)
    for (v in 1:nrow(aa)){
      arrows(0, 0, aa[v, 1], aa[v, 2], length = 0.1, angle = 15, code = 2, lty=2)
      if (label.j){
        if (aa[v, 1] >= 0) pos <- 4
        else pos <- 2
        text(aa[v, 1], y = aa[v, 2], labels = rownames(aa)[v], pos = pos, offset=0.2)
      }
    }
    for (v in 1:nrow(ab)){
      arrows(0, 0, ab[v, 1], ab[v, 2], length = 0.1, angle = 15, code = 2, col="blue")
      if (ab[v, 1] >= 0) pos <- 4
      else pos <- 2
      text(ab[v, 1], y = ab[v, 2], labels = rownames(ab)[v], pos = pos, offset=0.2, col="blue")
    }
}

    matrice[, 4:ncol(matrice)] <- scale(matrice[, 4:ncol(matrice)], center =TRUE, scale =FALSE)[, ]
    nbconso <- ncol(matrice) - 3
    x1 <- matrice[, 2]
    x2 <- matrice[, 3]
    x12 <- scale(x1, center =TRUE, scale =FALSE)[, ]^2
    x22 <- scale(x2, center =TRUE, scale =FALSE)[, ]^2
    x12plusx22 <- x12 + x22
    x3 <- scale(x1, center =TRUE, scale =FALSE)[, ] * scale(x2, center =TRUE,
        scale =FALSE)[, ]
    XX <- cbind(x1, x2, x12, x22, x3)
    etendue.x1 <- diff(range(x1))
    etendue.x2 <- diff(range(x2))
    pas <- max(etendue.x1, etendue.x2)/resolution
    f1 <- seq((min(x1)-etendue.x1*0.05),(max(x1)+etendue.x1*0.05), pas)
    f2 <- seq((min(x2)-etendue.x2*0.05),(max(x2)+etendue.x2*0.05), pas)
    depasse <- matrix(0, nrow = length(f1), ncol = length(f2))
    abscis <- NULL
    ordon <- NULL
    for (i in 1:nbconso) {
        if (regmod == 1)
            coeff <- lm(matrice[, i + 3] ~ XX[, 1] + XX[, 2] +
                XX[, 3] + XX[, 4] + XX[, 5],na.action=na.omit)$coef
        if (regmod == 2) {
            coeff <- lm(matrice[, i + 3] ~ XX[, 1] + XX[, 2],na.action=na.omit)$coef
            coeff <- c(coeff, 0, 0, 0)
        }
        if (regmod == 3) {
            coeff <- lm(matrice[, i + 3] ~ x1 + x2 + x12plusx22,na.action=na.omit)$coef
            coeff <- c(coeff, coeff[4], 0)
        }
        if (regmod == 4) {
            coeff <- lm(matrice[, i + 3] ~ XX[, 1] + XX[, 2] +
                XX[, 3] + XX[, 4],na.action=na.omit)$coef
            coeff <- c(coeff, 0)
        }
        predites <- outer(f1, f2, predire, coeff)
        if(sd(as.vector(predites),na.rm =TRUE)!=0) predites <- (predites - mean(predites, na.rm =TRUE))/sd(as.vector(predites),
            na.rm =TRUE)
        depasse <- depasse + matrix(as.numeric(predites > level),
            nrow = length(f1), ncol = length(f2))
        abscis <- c(abscis, f1[rev(order(predites))[1] - length(f1) *
            as.integer((rev(order(predites))[1] - 0.5)/length(f1))])
        ordon <- c(ordon, f2[as.integer(1 + (rev(order(predites))[1] -
            0.5)/length(f1))])
    }
    nb.depasse <- depasse
    depasse <- round(depasse/nbconso * 100)
    dimnames(depasse) <- list(as.character(f1), as.character(f2))
if(graph.carto){
    dev.new()
    col = cm.colors2(100)
    if (is.null(main)) main = "Preference mapping"
    image(f1, f2, depasse, col = col, xlab=paste("Dim",coord[1]), ylab=paste("Dim",coord[2]), main = main, font.main = font,
        , cex.main = cex, asp = asp)
    contour(f1, f2, depasse, nlevels = 9, levels = c(20, 30,
        40, 50, 60, 70, 80, 90, 95), add =TRUE, labex = 0)
    for (i in 1:nrow(matrice)) {
        points(matrice[i, 2], matrice[i, 3],pch=15)
        text(matrice[i, 2], matrice[i, 3], matrice[i,1],pos = 4, offset = 0.2,)
    }
    points(abscis, ordon, pch = 20)
}
    don <- cbind.data.frame(as.factor(aux),t(MatH))
    colnames(don) <- c("clusters",paste("Prod",rownames(MatH),sep="."))
    resdecat <- decat(don,formul="~clusters",firstvar=2,proba=0.05,graph=FALSE)
    res <- list()
    res$clusters <- aux
    res$prod.clusters <- resdecat$resT
    res$nb.depasse <- nb.depasse
    res$f1<-f1
    res$f2<-f2
    res$abscis<-abscis
    res$ordon<- ordon
    res$matrice<- matrice
    return(res)
}
