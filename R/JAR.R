#' JAR
#' 
#' Just About Right
#' 
#' Perform the penalty analysis. Two models are constructed.\cr The
#' one-dimensional model is constructed descriptor by descriptor. For
#' descriptor_j the model is:\cr Hedonic score = Descriptor_j_Not enough+
#' Descriptor_j_Too much \cr The multi-dimensional model is constructed with
#' all descriptors simultaneously:\cr Hedonic score = Descriptor_1_Not enough+
#' Descriptor_1_Too much +...+ Descriptor_p_Not enough+ Descriptor_p_Too much+
#' Product + Judge\cr
#' 
#' @param x data.frame
#' @param col.p the position of the \emph{product} variable
#' @param col.j the position of the \emph{panelist} variable
#' @param col.pref the position of the \emph{preference} variable
#' @param jarlevel a string corresponding to the jar level (the level must be
#' the same for all the jar variables)
#' @return Returns a list of 3 objects. \cr The penalty1 object corresponds to
#' the one-dimensional penalty results: a data-frame with the penalty
#' coefficient in the first column, the standard deviation and the p-value for
#' the test that the penalty is significantly different from 0.\cr The penalty2
#' object corresponds to the mutli-dimensional penalty results: a data-frame
#' with the penalty coefficient in the first column, the standard deviation and
#' the p-value for the test that the penalty is significantly different from 0.
#' The Frequency object gives the percentage of times the non-jar categories
#' are given for each product: a matrix with the non-jar categories in rows and
#' the products in columns
#' @author Francois Husson
#' @seealso \code{\link{plot.JAR}}
#' @keywords multivariate dplot
#' @examples
#' 
#' \dontrun{
#' data(JAR)
#' res.jar <- JAR(JAR,col.p=13,col.j=1,col.pref=2)
#' plot(res.jar,name.prod="284", model=1)
#'  }
#' 
#' @export JAR
JAR <- function(x, col.p, col.j, col.pref, jarlevel="jar"){

  fct.delete.first <- function(x){x[-1]}
  ind.jar <- (1:ncol(x))[-c(col.p,col.j,col.pref)]
  nbjar <- length(ind.jar)
  for (i in ind.jar){
    x[,i]=relevel(x[,i],jarlevel)
  }

  nbmod=rep(0,ncol(x))
  for(j in ind.jar){
    nbmod[j]=nlevels(x[,j])-1
  }
  nbmodtot=sum(nbmod)
  varesid=rep(1,nbjar+1)

  penal1 <- penal2 <- matrix(NA,nrow=nbmodtot,ncol=3)
  nommod=rep("a",nbmodtot)
  colnames(penal1)=c("One-dimension penalty (all products)","Standard error","p-value")

  ifin=0
  for(j in ind.jar){
    res=lm(x[,col.pref]~x[,j])
    varesid[j]= anova(res)[2,3]
    ideb=ifin+1
    ifin=ideb+nbmod[j]-1
    npar=nbmod[j]+1
    penal1[c(ideb:ifin),1]=-res$coefficients[2:npar]
    penal1[c(ideb:ifin),2]=summary(res)[[4]][2:npar,2]
    nommod[c(ideb:ifin)]=levels(x[,j])[2:npar]
    penal1[c(ideb:ifin),3]=summary(res)[[4]][2:npar,4]
  }
 res <- lm(x[,col.pref]~.,data=x[,c(ind.jar,col.p,col.j)])
 penal2 <- summary(res)$coef[2:(nbmodtot+1),c(1,2,4)]
 colnames(penal2)[1]="Multidimensional penalty (all products)"
 rownames(penal1)=rownames(penal2)=nommod
 Frequency <- matrix(NA,nrow=nbmodtot,ncol=nlevels(x[,col.p]))
 for (j in 1:ncol(Frequency)) Frequency[,j] <- unlist(lapply(lapply(x[x[,col.p]==levels(x[,col.p])[j],ind.jar],table),fct.delete.first))
 Frequency <- sweep(Frequency,2,table(x[,col.p]),FUN="/")*100
 rownames(Frequency)=nommod
 colnames(Frequency)=levels(x[,col.p])

 res <- list(penalty1 = penal1,penalty2 = penal2, Frequency=Frequency)
 class(res) <- c("JAR", "list ")
 return(res)
}
