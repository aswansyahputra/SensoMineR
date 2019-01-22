#' Plot panelists' tableclothe
#' 
#' Plot panelists' tableclothe.
#' 
#' The data used here refer to a specific experiment, where panelists are asked
#' to position products on a tableclothe of dimension \code{lim}, by default
#' (60,40).
#' 
#' @param donnee a data frame of dimension (\emph{p,2j}), where \emph{p}
#' represents the number of products and \emph{j} the number of panelists
#' @param numr the number of tableclothe per row (by default 2)
#' @param numc the number of tableclothe per column (by default 2)
#' @param color the color used to display the products
#' @param lim the size of the tableclothe
#' @return Returns as many graphs as there are panelists, each graph represents
#' products positioned by a given panelist on a tablecloth
#' @author Francois Husson
#' @seealso \code{\link{napping}}, \code{\link{pmfa}}, \code{\link{indscal}}
#' @references Pages J. (2005). Collection and analysis of perceived product
#' inter-distances using multiple factor analysis; application to the study of
#' ten white wines from the Loire Valley. \emph{Food Quality and Preference}.
#' 16 (7) pp. 642-649.
#' @keywords multivariate
#' @examples
#' 
#' \dontrun{
#' data(napping)
#' nappeplot(napping.don)
#' }
#' 
#' @export nappeplot
nappeplot <- function (donnee, numr = 2, numc = 2, color = "blue", lim = c(60, 40)) {
  nbjuge <- ncol(donnee)/2
  mult <- nbjuge%/%(numr * numc)
  if (nbjuge == (nbjuge%/%(numr * numc)) * (numr * numc))   mult = mult - 1
  for (m in 0:mult) {
    par(mfrow = c(numr, numc)) ###
    for (nbd in 1:(numr * numc)) {
      nb <- (m * (numr * numc) + nbd)
      if (nb <= nbjuge) {
        plot(donnee[, (2 * (nb - 1) + 1):(2 * nb)], col = color,
             xlim = c(0, lim[1]), ylim = c(0, lim[2]), xlab = "", 
             ylab = "", main = paste(colnames(donnee)[2 * nb]), type = "n", asp = 1)
        points(donnee[, (2 * (nb - 1) + 1):(2 * nb)], col = color, cex = 0.8, pch = 20)
        text(donnee[, (2 * (nb - 1) + 1):(2 * nb)], label = rownames(donnee), col = color, cex = 0.8, pos = 4, offset = 0.2)
      }
    }
    if (m < mult) dev.new()
  }
  par(las = 0)
  par(mfrow = c(1, 1))
}
