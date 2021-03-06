#' Construct a design for triangle tests
#' 
#' Construct a design to make triangle tests.
#' 
#' Triangle test: panelists receive three coded samples. They are told that two
#' of the sample are the same and one is different. Panelists are asked to
#' identify the odd sample.
#' 
#' @param nbprod number of products to compare
#' @param nbpanelist number of panelists who make the triangle test
#' @param bypanelist number of expermient that each panelist can done (by
#' default each panelist make all the comparisons between the products
#' @param labprod name of the products (by default, the product are coded from
#' 1 to the number of products
#' @param labpanelist name of the panelists (by default, the panelists are
#' coded from 1 to the number of panelists
#' @return Returns an data.frame of dimension (\emph{t,3}), where \emph{t} is
#' the number of experiments. In column 1, 2 and 3 the product to test are
#' given. The product in column 1 is by coded "X", in column 2 is coded by "Y"
#' and in column 3 is coded by "Z". Panelist should start by product "X", then
#' "Y" and then by "Z".
#' @author Francois Husson
#' @seealso \code{\link{triangle.test}}, \code{\link{triangle.pair.test}}
#' @keywords models
#' @examples
#' 
#' ##Example 1
#' design1 = triangle.design (nbprod = 4, nbpanelist = 8)
#' 
#' ##Example 2
#' design2 = triangle.design(nbprod = 4, nbpanelist = 6, bypanelist = 3,
#'   labprod=c("prod1","prod2","prod3","prod4"),
#'   labpanelist=c("John","Audrey","Peter","Martina","James","Lisa"))
#'   
#' 
#' @export triangle.design
triangle.design <- function(nbprod, nbpanelist, bypanelist = nbprod * (nbprod -
                              1) / 2, labprod = 1:nbprod, labpanelist = 1:nbpanelist) {
  aux <- as.data.frame(matrix(0, nbprod * (nbprod - 1) / 2, 2))
  numligne <- 0
  for (i in 1:(nbprod - 1)) {
    for (j in (i + 1):nbprod) {
      numligne <- numligne + 1
      aux[numligne, 1] <- labprod[i]
      aux[numligne, 2] <- labprod[j]
    }
  }
  aux <- aux[sample(nrow(aux)), ]
  plan <- as.data.frame(matrix(0, nbpanelist * bypanelist, 4))
  num.test <- 0
  ligne.aux <- 0
  for (i in 1:nbpanelist) {
    for (j in 1:bypanelist) {
      ligne.aux <- ligne.aux + 1
      if (ligne.aux == nrow(aux) + 1) {
        ligne.aux <- 1
      }
      num.test <- num.test + 1
      plan[num.test, 1] <- labpanelist[i]
      plan[num.test, 2:3] <- aux[ligne.aux, ]
    }
  }
  plan <- plan[order(plan[, 2], plan[, 3]), ]
  for (i in 1:(nrow(plan))) {
    if (i %% 6 == 1) {
      plan[i, 4] <- plan[i, 2]
    }
    if (i %% 6 == 2) {
      plan[i, 4] <- plan[i, 3]
    }
    if (i %% 6 == 3) {
      plan[i, 4] <- plan[i, 3]
      plan[i, 3] <- plan[i, 2]
    }
    if (i %% 6 == 4) {
      plan[i, 4] <- plan[i, 2]
      plan[i, 2] <- plan[i, 3]
      plan[i, 3] <- plan[i, 4]
    }
    if (i %% 6 == 5) {
      plan[i, 4] <- plan[i, 3]
      plan[i, 3] <- plan[i, 2]
      plan[i, 2] <- plan[i, 4]
    }
    if (i %% 6 == 0) {
      plan[i, 4] <- plan[i, 2]
      plan[i, 2] <- plan[i, 3]
    }
  }
  plan <- plan[order(plan$V1), ]
  if (is.integer(labpanelist)) {
    row.names(plan) <- paste0("Panelist", plan$V1, ".Test", 1:bypanelist)
  } else {
    row.names(plan) <- paste0(plan$V1, ".Test", 1:bypanelist)
  }
  plan <- plan[, -1]
  colnames(plan) <- c("Product X", "Product Y", "Product Z")
  plan <- as.data.frame(plan)
  return(plan)
}
