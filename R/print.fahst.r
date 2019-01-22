#' Print Factorial Approach for Hierarchical Sorting Task data (FAHST) results
#' 
#' Print Factorial Approach for Hierarchical Sorting Task data (FAHST) results.
#' 
#' 
#' @param x an object of class fahst
#' @param file A connection, or a character string naming the file to print to.
#' If NULL (the default), the results are not printed in a file
#' @param sep character string to insert between the objects to print (if the
#' argument file is not NULL
#' @param \dots further arguments passed to or from other methods
#' @author Marine Cadoret, S\'ebastien L\^e
#' \email{sebastien.le@@agrocampus-ouest.fr}
#' @seealso \code{\link{fahst}}
#' @keywords print
#' @examples
#' 
#' \dontrun{
#' data(cards)
#' group<-c(2,3,3,2,2,4,2,3,2,1,3,2,3,3,3,2,3,3,2,3,3,3,3,3,3,3,3,3,3,3)
#' res.fast <- fahst(cards,group,graph=F)
#' print.fahst(res.fahst, file="c:/fahst.csv", sep = ";")
#' }
#' 
#' @export print.fahst
print.fahst=function (x, file = NULL, sep = ";", ...){
    res.trih <- x
    if (!inherits(res.trih, "fahst"))
        stop("non convenient data")

    cat("**Results for the hierarchical sorting data**\n\n")
    cat("The analysis was done on", nrow(res.trih$ind$coord), "objects, described by",
        nrow(res.trih$group$coord), "subjects\n\n")
    cat("*The results are available in the following objects:\n\n")
    res <- array("", c(17, 2), list(1:17, c("name", "description")))
    res[1, ] <- c("$eig", "eigenvalues")
    res[2, ] <- c("$var", "results for the groups and levels")
    res[3, ] <- c("$var$coord", "coordinates for the groups (whatever the level)")
    res[4, ] <- c("$var$cos2", "cos2 for the groups (whatever the level)")
    res[5, ] <- c("$var$contrib", "contributions for the groups (whatever the level)")
    res[6, ] <- c("$var$vtest", "vtest for the groups (whatever the level)")
    res[7, ] <- c("$var$coord.lev", "coordinates for the levels")
    res[8, ] <- c("$ind", "results for the objects")
    res[9, ] <- c("$ind$coord", "coordinates for the objects")
    res[10, ] <- c("$ind$cos2", "cos2 for the objects")
    res[11, ] <- c("$ind$contrib", "contributions for the objects")
    res[12, ]  <- c("$group", "results for the subjects")
    res[13, ]  <- c("$group$coord", "coordinates for the subjects")
    res[14, ]  <- c("$group$cos2", "cos2 for the subjects")
    res[15, ]  <- c("$group$contrib", "contributions for the subjects")
    res[16, ]  <- c("$validity", "results for the elements of validity")
    res[17, ]  <- c("$call", "some statistics")

    print(res[1:17, ])
    if (!is.null(file)) {
        write.infile(res.trih, file = file, sep = sep)
        print(paste("All the results are in the file", file))
    }
}
