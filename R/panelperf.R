#' Panel's performance according to its capabilities to dicriminate between
#' products
#' 
#' Computes automatically P-values associated with the F-test as well as the
#' residual term for a given analysis of variance model.
#' 
#' The \code{formul} parameter must be filled in by an analysis of variance
#' model and must begin with the categorical variable of interest (e.g. the
#' product effect) followed by the different other factors of interest (and
#' their combinations).  E.g.:\code{formul = "~Product+Session"}.
#' 
#' @param donnee a data frame
#' @param formul the model that is to be tested
#' @param subset cf. function \code{\link{lm}} in the \pkg{stats} package
#' @param firstvar the position of the first endogenous variable
#' @param lastvar the position of the last endogenous variable (by default the
#' last column of \code{donnee}
#' @param random boolean, effect should be possible as fixed or random (default
#' as random)
#' @return A list containing the following components: \item{p.value}{a matrix
#' of dimension (\emph{k,m}) of \emph{P-values} associated with the F-test for
#' the \emph{k} descriptors and the \emph{m} factors and their combinations
#' considered in the analysis of variance model of interest}
#' \item{variability}{a matrix of dimension (\emph{k,m}) where the entries
#' correspond to the percentages of variability due to the effects introduced
#' in the analysis of variance model of interest} \item{res}{a vector of
#' dimension \emph{k} of residual terms for the analysis of variance model of
#' interest} \item{r2}{a vector of dimension \emph{k} of r-squared for the
#' analysis of variance model of interest}
#' @author F Husson, S Le
#' @seealso \code{\link{paneliperf}}, \code{\link{aov}}
#' @references P. Lea, T. Naes, M. Rodbotten. \emph{Analysis of variance for
#' sensory data}.\cr H. Sahai, M. I. Ageel. \emph{The analysis of variance}.
#' @keywords models
#' @examples
#' 
#' data(chocolates)
#' res=panelperf(sensochoc, firstvar = 5, formul = "~Product+Panelist+
#'     Session+Product:Panelist+Session:Product+Panelist:Session")
#' ## Sort results by product p.values.
#' coltable(magicsort(res$p.value, sort.mat = res$p.value[,1], bycol = FALSE,
#'     method = "median"), main.title = "Panel performance (sorted by product P-value)")
#' 
#' @export panelperf
"panelperf" <- function(donnee,formul,subset=NULL,firstvar,lastvar=ncol(donnee),random=TRUE){

old.contr = options()$contrasts
options(contrasts=c("contr.sum", "contr.sum"))

  for (j in 1 :(firstvar-1))  donnee[,j] <- as.factor(donnee[,j])
  formul = as.formula(formul)
  lab.sauv <- lab <- colnames(donnee)
  for (i in 1:length(lab)) lab[i]=gsub(" ",".",lab[i])
  colnames(donnee) = lab
  equation <- as.character(formul)

  Terms=attr(terms(as.formula(equation)),"term.labels")
  equation = paste("~",Terms[1])
  if (length(Terms) > 1) for (i in 2:length(Terms)) equation <- paste(equation,"+",Terms[i])
  equation <- as.character(as.formula(equation))

  dim.donnee <- ncol(donnee)
  for (i in 1:dim.donnee) {
    if (gsub(" ","",strsplit(equation,split="+",fixed=TRUE)[[2]][1])==lab[i]) col.p <- i
    if (gsub(" ","",strsplit(equation,split="+",fixed=TRUE)[[2]][2])==lab[i]) col.j <- i
  }
  res <- matrix(0,lastvar+1-firstvar,1)
  r2 <- matrix(0,lastvar+1-firstvar,1)
  variab <- perf <- matrix(0,lastvar+1-firstvar,length(strsplit(equation[2],"+",fixed=TRUE)[[1]]))

  for (varendo in firstvar:lastvar) {
      formule <- paste(lab[varendo],"~",equation[2])
    formule <- as.formula(formule)
    aux1 <- aov( formule , data = donnee, subset=subset,na.action =na.exclude)
    aux <- summary(aux1)[[1]]
    perf[varendo-firstvar+1,] <- aux[-nrow(aux),5]
    variab[varendo-firstvar+1,] <- aux[-nrow(aux),2]/sum(aux[,2])
    res[varendo-firstvar+1,] <- sqrt(aux[nrow(aux),3])
    r2[varendo-firstvar+1,] <- summary.lm(aux1)$r.squared
 
    if (random) {
      panelist=colnames(donnee)[col.j]
      for (i in 1:length(Terms)){
        if (any(grep(panelist,Terms[i]))){ 
          if (any(grep(":",Terms[i]))){
            facteur = gsub(":","",Terms[i])
            facteur = gsub(panelist,"",facteur)
            for (k in 1:nrow(aux)) if(gsub(" ","",rownames(aux)[k])==facteur) nrow.facteur = k
            perf[varendo-firstvar+1,nrow.facteur] <- pf(aux[nrow.facteur,3]/aux[i,3],aux[nrow.facteur,1],aux[i,1],lower.tail=FALSE)
          }
        }
      }
    }
  }
aa <- strsplit(as.character(formule),split="~",fixed=TRUE)[[3]]
dimnames(variab) <- dimnames(perf) <- list(lab.sauv[firstvar:lastvar],rownames(aux)[-nrow(aux)])
dimnames(res) <- list(lab.sauv[firstvar:lastvar],"stdev residual")
dimnames(r2) <- list(lab.sauv[firstvar:lastvar],"r2")

panelperf = list() 
panelperf$p.value = perf
panelperf$variability = variab
panelperf$res = res
panelperf$r2 = r2
return(panelperf)
options(contrasts=old.contr)
}
