#' Search for discriminating descriptors
#' 
#' This function is designed to select the significant descriptors in a data
#' frame
#' 
#' 
#' @param matrice a data frame made up of at least two qualitative variables
#' (\emph{product}, \emph{panelist}) and a set of quantitative variables
#' (sensory descriptors)
#' @param col.j the position of the categorical variable which make the
#' variability, panelist for sensory studies. The value of \code{col.j} can
#' also be NULL if no categorical variables make the variability.
#' @param col.p the position of the categorical variable of interest, product
#' for sensory studies
#' @param firstvar the position of the first endogenous variable
#' @param lastvar the position of the last endogenous variable (by default the
#' last column of \code{donnee}
#' @param level the threshold (P-value) below which variables are considered as
#' discriminating for the following analysis of variance model:
#' \code{descriptor=col.p+col.j}
#' @return Returns a data frame with all the qualitative variables and only
#' discriminating variables
#' @author Francois Husson
#' @keywords models
#' @examples
#' 
#' data(chocolates)
#' ## In this example, all the descriptos are discriminated
#' interesting.desc <- search.desc(sensochoc, col.j = 1, col.p = 4, 
#'     firstvar = 5, level = 0.5)
#' 
#' @export search.desc
search.desc<- function (matrice, col.j, col.p, firstvar, lastvar = ncol(matrice),level = 0.5){
    lab.sauv <- lab <- colnames(matrice)
    #for (i in 1:length(lab)) lab[i] = gsub(" ", ".", lab[i])
    #! Correction sdb du 04/09/2006 09:17:42
    for (i in 1:length(lab)) lab[i] = chartr(" '", "..", lab[i])
    #! fin
    colnames(matrice) = lab
    nomdescripteur <- lab.sauv[firstvar:lastvar]
    for (i in 1:(firstvar - 1)) matrice[, i] <- as.factor(matrice[, i])
    tab.F <- matrix(0, (lastvar - firstvar + 1), 1)
    for (i in firstvar:lastvar) {
        if (!is.null(col.j))  aux <- round(summary(aov(as.formula(paste(lab[i], "~", lab[col.p], "+", lab[col.j])), data = matrice, na.action = na.exclude))[[1]],10)
        else aux <- round(summary(aov(as.formula(paste(lab[i], "~", lab[col.p] )), data = matrice, na.action = na.exclude))[[1]],10)
        tab.F[i - firstvar + 1] <- pf(aux[1, 4], aux[1, 1], aux[(dim(aux)[[1]]), 1], lower.tail = FALSE)
    }
    dimnames(tab.F) <- list(nomdescripteur, NULL)
    resF <- vector("list", length = 1)
    select <- (1:nrow(tab.F))
    resF <- data.frame(Variables = as.factor(dimnames(tab.F)[[1]][rev(order(tab.F))][select]), Proba = as.numeric(tab.F[rev(order(tab.F))][select]))
    mat.analyse <- data.frame(as.factor(matrice[, 1]))
    for (i in 2:(firstvar - 1)) mat.analyse <- cbind.data.frame(mat.analyse, matrice[, i])
    name.var = NULL
    for (i in firstvar:lastvar) {
        if (!is.na(tab.F[i - firstvar + 1])){
         if (tab.F[i - firstvar + 1] < level) {
          mat.analyse <- cbind(mat.analyse, matrice[, i])
          name.var = c(name.var,lab.sauv[i])
         }
        }
    }
    colnames(mat.analyse)[1:(firstvar - 1)] <- lab.sauv[1:(firstvar - 1)]
    colnames(mat.analyse)[firstvar:ncol(mat.analyse)] <- name.var
    return(mat.analyse)
}
