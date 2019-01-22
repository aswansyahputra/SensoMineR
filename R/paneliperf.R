#' Panelists' performance according to their capabilities to dicriminate
#' between products
#' 
#' Computes automatically P-values, Vtests, residuals, r-square for each
#' category of a given qualitative variable (e.g. the \emph{panelist}
#' variable);\cr Computes he agreement between each panelist and the panel
#' results;\cr Gives the panel results (optional).
#' 
#' The \code{formul} parameter must be filled in by an analysis of variance
#' model and must begin with the categorical variable of interest (e.g. the
#' product effect) followed by the different other factors of interest (and
#' their combinations).  E.g.:\code{formul = "~Product+Session"}.
#' 
#' @param donnee a data frame made up of at least two qualitative variables
#' (\emph{product}, \emph{panelist}) and a set of quantitative variables
#' (sensory descriptors)
#' @param formul the aov model used for the panel
#' @param formul.j the aov model used for each panelist (no \emph{panelist}
#' effect allowed)
#' @param col.j the position of the \emph{panelist} variable
#' @param firstvar the position of the first endogenous variable
#' @param lastvar the position of the last endogenous variable (by default the
#' last column of \code{donnee}
#' @param synthesis boolean, the possibility to have the anova results for the
#' panel model
#' @param random boolean, the status of the Panelist variable in the anova
#' model for the panel
#' @param graph boolean, draws the PCA and MFA graphs
#' @return A list containing the following components: \item{prob.ind}{a matrix
#' which rows are the panelist, which columns are the endogenous variables (in
#' most cases the sensory descriptors) and which entries are the P-values
#' associated to the AOV model} \item{vtest.ind}{a matrix which rows are the
#' panelist, which columns are the endogenous variables (in most cases the
#' sensory descriptors) and which entries are the Vtests associated to the AOV
#' model} \item{res.ind}{a matrix which rows are the panelist, which columns
#' are the endogenous variables (in most cases the sensory descriptors) and
#' which entries are the residuals associated to the AOV model} \item{r2.ind}{a
#' matrix which rows are the panelist, which columns are the endogenous
#' variables (in most cases the sensory descriptors) and which entries are the
#' R-square associated to the AOV model} \item{signif.ind}{a vector with the
#' number of significant descriptors per panelist} \item{agree.ind}{a matrix
#' with as many rows as there are panelists and as many columns as there are
#' descriptors and the entries of this matrix are the correlation coefficients
#' between the product coefficients for the panel and for the panelists} \cr
#' \item{complete}{a matrix with the v-test corresponding to the p.value (see
#' \code{p.values} below), the median of the agreement (see \code{agree}
#' upper), the standard deviation of the panel anova model (see \code{res}
#' below)} \item{p.value}{a matrix of dimension (\emph{k,m}) of \emph{P-values}
#' associated with the F-test for the \emph{k} descriptors and the \emph{m}
#' factors and their combinations considered in the analysis of variance model
#' of interest} \item{variability}{a matrix of dimension (\emph{k,m}) where the
#' entries correspond to the percentages of variability due to the effects
#' introduced in the analysis of variance model of interest} \item{res}{a
#' vector of dimension \emph{k} of residual terms for the analysis of variance
#' model of interest} \item{r2}{a vector of dimension \emph{k} of r-squared for
#' the analysis of variance model of interest}
#' 
#' The usual graphs when MFA is performed on the data.frame resulting from
#' vtest.ind and agree.ind. \cr The PCA graphs for the complete output.\cr
#' @author F Husson, S Le
#' @seealso \code{\link{panelperf}}, \code{\link{aov}}
#' @references P. Lea, T. Naes, M. Rodbotten. \emph{Analysis of variance for
#' sensory data}. H. Sahai, M. I. Ageel. \emph{The analysis of variance}.
#' @keywords models
#' @examples
#' 
#' \dontrun{
#' data(chocolates)
#' res<-paneliperf(sensochoc, formul = "~Product+Panelist+Session+
#'   Product:Panelist+Product:Session+Panelist:Session",
#'   formul.j = "~Product", col.j = 1, firstvar = 5, synthesis = TRUE)
#' resprob<-magicsort(res$prob.ind, method = "median")
#' coltable(resprob, level.lower = 0.05, level.upper = 1,
#'     main.title = "P-value of the F-test (by panelist)")
#' hist(resprob,main="Histogram of the P-values",xlab="P-values")
#' 
#' resr2<-magicsort(res$r2.ind, method = "median", ascending = FALSE)
#' coltable(resr2, level.lower = 0.00, level.upper = 0.85,
#'     main.title = "Adjusted R-square (by panelist)")
#' 
#' resagree<-magicsort(res$agree, sort.mat = res$r2.ind, method = "median")
#' coltable(resagree, level.lower = 0.00, level.upper = 0.85,
#'     main.title = "Agreement between panelists")
#' hist(resagree,main="Histogram of the agreement between panelist and panel",
#'     xlab="Correlation coefficient between the product effect for 
#'     panelist and panel")
#' 
#' coltable(magicsort(res$p.value, sort.mat = res$p.value[,1], bycol = FALSE,
#'     method = "median"),
#'     main.title = "Panel performance (sorted by product P-value)")
#' }
#' 
#' @export paneliperf
"paneliperf" <- function(donnee,formul,formul.j="~Product",col.j,firstvar,lastvar=ncol(donnee),synthesis=FALSE,random=TRUE,graph=FALSE){

old.contr = options()$contrasts
options(contrasts=c("contr.sum", "contr.sum"))
  for (j in 1 :(firstvar-1))  donnee[,j] <- as.factor(donnee[,j])
  formul.j = as.formula(formul.j)
  lab.sauv <- lab <- colnames(donnee)
  for (i in 1:length(lab)) lab[i]=gsub(" ",".",lab[i])
  colnames(donnee) = lab
  equation <- as.character(formul.j)
  lab.j <- levels(donnee[,col.j])

  dim.donnee <- ncol(donnee)

  Terms=attr(terms(as.formula(equation)),"term.labels")
  equation = paste("~",Terms[1])
  if (length(Terms) > 1) for (i in 2:length(Terms)) equation <- paste(equation,"+",Terms[i])
  equation <- as.character(as.formula(equation))
  
  for (i in 1:dim.donnee) {
    if (gsub(" ","",strsplit(equation,split="+",fixed=TRUE)[[2]][1])==lab[i]) col.p <- i
  }
  r2 <- prob <- vtest <- res <- matrix(0,length(levels(donnee[,col.j])),lastvar+1-firstvar)

  for (j in 1:length(lab.j)){
    for (varendo in firstvar:lastvar) {
      formule <- paste(lab[varendo],"~",equation[2])
##      formule <- paste(lab[varendo],"~ C(")
##      aux2 <- equation[2]
##      aux3 <- strsplit(aux2,"+",fixed=TRUE)[[1]]
##
##      for (i in 1:length(aux3)) {
##        if (any(grep("%",aux3[i]))) {
##              formule <- paste(formule,strsplit(aux3[i],"%",fixed=TRUE)[[1]][1],",sum)%in%C(",strsplit(aux3[i],"%",fixed=TRUE)[[1]][3],",sum)")
##        }
##        else
##        if (any(grep(":",aux3[i]))) {
##           formule <- paste(formule,strsplit(aux3[i],":",fixed=TRUE)[[1]][1],",sum) : C(",strsplit(aux3[i],":",fixed=TRUE)[[1]][2],",sum)")
##        }
##        else {
##        formule <- paste(formule,aux3[i],",sum)")
##        }
##        if (i < length(aux3))  formule <- paste (formule, "+C(")
##      }

      formule <- as.formula(formule)
      aux <- summary(aov( formule , data = donnee, subset=(donnee[,col.j]==levels(donnee[,col.j])[j]),na.action =na.omit))[[1]]
      prob[j,varendo-firstvar+1] <- aux[1,5]
      vtest[j,varendo-firstvar+1] <- -qnorm(aux[1,5]/2)
      res[j,varendo-firstvar+1] <- sqrt(aux[nrow(aux),3])
      r2[j,varendo-firstvar+1] <- summary.lm(aov( formule , data = donnee, subset=(donnee[,col.j]==levels(donnee[,col.j])[j]),na.action =na.omit))$r.squared
    }
}

  dimnames(prob) <-   dimnames(r2) <-    dimnames(vtest) <-  dimnames(res) <- list(levels(donnee[,col.j]),lab.sauv[firstvar:lastvar])

  aa <- averagetable(donnee,formul=formul,firstvar=firstvar,lastvar=lastvar)
  agree <- matrix(NA,length(lab.j),lastvar-firstvar+1)
  for (j in 1:length(lab.j)){
      bb <- averagetable(donnee,formul=formul.j,subset=(donnee[,col.j]==lab.j[j]),firstvar=firstvar,lastvar=lastvar)
      for (k in 1:ncol(bb)) if (var(bb[,k],na.rm=TRUE)>1e-20) agree[j,k]=cor(aa[,k],bb[,k],use="pairwise.complete.obs")
    }
  dimnames(agree) <- list(lab.j,lab.sauv[firstvar:lastvar])  

 if (graph){
  aux.agree=agree
  colnames(aux.agree)=paste(colnames(agree),".a",sep="")
  aux.vtest= -qnorm(prob/2)
  colnames(aux.vtest)=paste(colnames(prob),".p",sep="")
  aux.graph = MFA(cbind.data.frame(aux.agree,aux.vtest),group=c(ncol(aux.agree),ncol(aux.vtest)),name.group=c("Agree","Prob"),graph=FALSE)
  plot(aux.graph,choix = "ind",col.hab=rep("black",nrow(aux.graph$ind$coord)))
  plot(aux.graph,choix="ind",partial="all",habillage="group")
  plot(aux.graph,choix="var", habillage="group")
  plot(aux.graph,choix="group")
 }
  paneliperf = list() 
  paneliperf$prob.ind = prob
  paneliperf$vtest.ind = vtest
  paneliperf$res.ind = res
  paneliperf$r2.ind = r2
  paneliperf$signif.ind=apply(matrix(as.integer(prob<0.05),nrow=nrow(prob)),1,sum,na.rm=TRUE) # nb de descripteur significatif / juge
  paneliperf$agree.ind = agree

  if (synthesis){
    bb=panelperf(donnee,formul=formul,subset=NULL,firstvar=firstvar,lastvar=lastvar,random=random)
    aux=cbind(-qnorm(bb$p.value/2),apply(agree,2,median,na.rm=TRUE),apply(prob,2,median,na.rm=TRUE),bb$res,bb$r2)
    colnames(aux)=c(colnames(bb$p.value),"median(agree)","median(prob.ind)","stdev Res","R2")
    if (graph) {
      res.pca = PCA(aux,quanti.sup=(ncol(aux)-3):ncol(aux),graph=FALSE)
      plot(res.pca,title="PCA on the P-values issued from the AOV model")
      plot(res.pca,choix="var",title="PCA on the P-values issued from the AOV model")
    }
    rownames(aux) <- rownames(bb$p.value) <- rownames(bb$variab) <- rownames(bb$res) <- lab.sauv[firstvar:lastvar]
    paneliperf$complete = aux
    paneliperf$p.value = bb$p.value
    paneliperf$variability = bb$variab
    paneliperf$res = bb$res
  }
  return(paneliperf)
  options(contrasts=old.contr)
}
