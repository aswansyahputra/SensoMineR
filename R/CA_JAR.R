#' Make a correspondence analysis on the JAR data
#' 
#' Plot the CA graph.
#' 
#' 
#' @param x data.frame
#' @param col.p the position of the \emph{product} variable
#' @param col.j the position of the \emph{panelist} variable
#' @param col.pref the position of the \emph{preference} variable
#' @param jarlevel a string corresponding to the jar level (the level must be
#' the same for all the jar variables)
#' @return Draw a CA graph with the preference data as supplementary
#' qualitative variables, the products as rows, and the categories of the jar
#' variables as columns
#' @author Francois Husson
#' @keywords dplot
#' @examples
#' 
#' \dontrun{
#' data(JAR)
#' res <- CA_JAR(x = JAR, col.p = 13, col.j = 1, col.pref = 2, jarlevel = "jar")
#' plot.CA(res$res.CA, invisible = "row", cex = 0.8)
#'  }
#' 
#' @export CA_JAR
CA_JAR <- function(x, col.p, col.j, col.pref, jarlevel="jar"){

  ind.jar <- (1:ncol(x))[-c(col.p,col.j,col.pref)]
  nbjar <- length(ind.jar)
  for (i in ind.jar){
    x[,i]=relevel(x[,i],jarlevel)
    levels(x[,i])[1]=paste(colnames(x)[i],jarlevel,"_")
  }
  x[,col.pref]=as.factor(x[,col.pref])
  modtot <- unlist(lapply(x[,c(ind.jar,col.pref)],nlevels))
  Frequency <- matrix(NA,nrow=sum(modtot),ncol=nlevels(x[,col.p]))
  for (j in 1:ncol(Frequency)) Frequency[,j] <- unlist(lapply(x[x[,col.p]==levels(x[,col.p])[j],c(ind.jar,col.pref)],table))
  rownames(Frequency)=unlist(lapply(x[,c(ind.jar,col.pref)],levels))
  colnames(Frequency)=levels(x[,col.p])
  res <- CA(Frequency,row.sup=(sum(modtot[-length(modtot)])+1):sum(modtot), graph = FALSE)
  plot(res, cex=0.8, col.row=rep(3:(1+length(modtot)),modtot[-length(modtot)]),col.row.sup=rep("black",modtot[length(modtot)]),title="CA on the table product x jar variables")
  return(list(Frequency=Frequency,res.CA=res))
}
