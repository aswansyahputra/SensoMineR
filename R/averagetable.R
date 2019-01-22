#' Computes a (products,descriptors) matrix
#' 
#' Returns the (products,descriptors) matrix with entries the means over
#' panelists and sessions. \cr Computes analyses of variance automatically for
#' a given model and a set of quantitative variables.  Returns a data matrix
#' where each row is associated with each category of a given categorical
#' variable (in most cases, the categorical variable is the \emph{product}
#' variable), each column is associated with a quantitative variable, and each
#' cell is the corresponding adjusted mean or mean.\cr Computes the average
#' data table with respect to a categorical variable and a set of quantitative
#' variables.
#' 
#' 
#' The \code{formul} parameter can be filled in for a given analysis of
#' variance model.  The \code{formul} parameter must begin with the categorical
#' variable of interest (generally the \emph{product} variable) followed by the
#' different other factors (and eventually their interactions) of interest.
#' Classicially, one can used \code{formul =
#' "~Product+Panelist+Product:Panelist"}.  In practise and in our type of
#' applications, this function is very useful to obtain a data matrix in which
#' rows represent products and columns represent sensory descriptors. \cr If
#' "mean" is assigned to the \code{method} parameter, then the \code{formul}
#' parameter can be restricted to the sole variable of interest (generally the
#' \emph{product} variable). \cr If data are balanced, the two options "mean"
#' and "coeff" give the same results.
#' 
#' @param donnee a data frame made up of at least two qualitative variables
#' (\emph{product}, \emph{panelist}) and a set of quantitative variables
#' (sensory descriptors)
#' @param formul the model with respect to which the factor levels of the
#' categorical variable of interest are calculated
#' @param subset an optional vector specifying a subset of observations to be
#' used in the fitting process
#' @param method two possibilities, "coeff" (by default) or "mean"
#' @param firstvar the position of the first endogenous variable
#' @param lastvar the position of the last endogenous variable (by default the
#' last column of \code{donnee})
#' @param file the name of the output file (by default, NULL and results are
#' not in a file)
#' @return Return a matrix of dimension (\emph{p,q}), where \emph{p} is the
#' number of categories of the qualitative variable of interest (in most cases,
#' \emph{p} is the number of products) and \emph{q} is the number of (sensory)
#' descriptors. If "coeff" is assigned to the \code{method} parameter then the
#' function \emph{averagetable} returns the matrix of the adjusted means; if
#' "mean" is assigned to the \code{method} parameter then the function
#' averagetable returns the matrix of the means per category.
#' @author Francois Husson \email{husson@@agrocampus-ouest.fr}
#' @seealso \code{\link{aov}}
#' @references P. Lea, T. Naes, M. Rodbotten. \emph{Analysis of variance for
#' sensory data}. \cr H. Sahai, M. I. Ageel. \emph{The analysis of variance}.
#' @keywords models
#' @examples
#' 
#' data(chocolates)
#' resaverage<-averagetable(sensochoc, formul = "~Product+Panelist",
#'     firstvar = 5)
#' coltable(magicsort(resaverage), level.upper = 6,level.lower = 4,
#'     main.title = "Average by chocolate")
#' 
#' res.pca = PCA(resaverage, scale.unit = TRUE)
#' 
#' @export averagetable
"averagetable" <- function(donnee,formul,subset=NULL,method="coeff",firstvar,lastvar=ncol(donnee),file=NULL){

if ((method!="coeff")&(method!="mean")) stop(paste("The method",method,"is unknown. Use coeff or mean"))

for (j in 1:(firstvar-1))  donnee[,j] <- as.factor(donnee[,j])

formul = as.formula(formul)
lab.sauv <- lab <- colnames(donnee)
for (i in 1:length(lab)) lab[i] = chartr("(), ?;/:'!$=+\n;{}<>[]-","......................", lab[i])
colnames(donnee) = lab
equation <- as.character(formul)
Terms=attr(terms(as.formula(equation)),"term.labels")
equation = paste("~",Terms[1])
if (length(Terms) > 1) for (i in 2:length(Terms)) equation <- paste(equation,"+",Terms[i])
equation <- as.character(as.formula(equation))

dim.donnee <- ncol(donnee)
for (i in 1:dim.donnee) {
  if (gsub(" ","",strsplit(equation,split="+",fixed=TRUE)[[2]][1])==lab[i]) col.p <- i
  if (gsub(" ","",strsplit(equation,split="*",fixed=TRUE)[[2]][1])==lab[i]) col.p <- i
}
nbprod <- length(levels(donnee[,col.p]))
tab<-matrix(NA,nbprod,lastvar-firstvar+1)
row.names(tab) = levels(donnee[,col.p])

if (method =="mean"){
#  for (j in firstvar:lastvar){
    for (i in 1:nbprod){             
      if (length(subset)==0) tab[i,(firstvar:lastvar)-firstvar+1]<-colMeans(donnee[donnee[,col.p]==levels(donnee[,col.p])[i],firstvar:lastvar],na.rm=TRUE)
      if (length(subset)!=0) tab[i,(firstvar:lastvar)-firstvar+1]<-colMeans(donnee[subset&(donnee[,col.p]==levels(donnee[,col.p])[i]),firstvar:lastvar],na.rm=TRUE)
    }
#  }
}

old.contr = options()$contrasts
if (method =="coeff"){
  options(contrasts=c("contr.sum", "contr.sum"))
  for (varendo in firstvar:lastvar) {
    formule <- as.formula(paste(lab[varendo],"~",equation[2]))
    if (length(subset)!=0) aa=tapply(donnee[subset,varendo],donnee[subset,col.p],mean,na.rm=TRUE)
    if (length(subset)==0) aa=tapply(donnee[,varendo],donnee[,col.p],mean,na.rm=TRUE)
    if (!any(is.na(aa))) {
      aux <- summary.lm(aov( formule, data = donnee, subset=subset,na.action =na.exclude))$coef  
      tab[-nbprod,varendo-firstvar+1] <- aux[2:nbprod,1]
      tab[nbprod,varendo-firstvar+1] <-  - sum(tab[,varendo-firstvar+1],na.rm=TRUE)
      tab[,varendo-firstvar+1] <-  tab[,varendo-firstvar+1]+aux[1,1]
    }
    if (any(is.na(aa))) {
      bb = sum(is.na(aa))
      aux <- summary.lm(aov( formule, data = donnee, subset=subset,na.action =na.exclude))$coef 
      tab[!is.na(aa),varendo-firstvar+1][-nrow(aa[!is.na(aa)])] <- aux[2:(nbprod-bb),1]
      tab[!is.na(aa),varendo-firstvar+1][nrow(aa[!is.na(aa)])] <-  - sum(tab[,varendo-firstvar+1],na.rm=TRUE)
      tab[,varendo-firstvar+1] <-  tab[,varendo-firstvar+1]+aux[1,1]
    }
  }
}

dimnames(tab) = list(levels(donnee[,col.p]),lab.sauv[firstvar:lastvar])
tab=as.data.frame(tab)
if (length(file)!=0) write.csv2(tab,file=file,sep=";",dec=",")
return(tab)
options(contrasts=old.contr)
}
