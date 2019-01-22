#' Make a Triangle test for two products
#' 
#' Make a Triangle test for two products.
#' 
#' Triangle test: panelists receive three coded samples. They are told that two
#' of the sample are the same and one is different.  Panelists are asked to
#' identify the odd sample.
#' 
#' @param nb.good number of panelists who identify the odd sample
#' @param nb.answer number of panelists who make the triangle test
#' @return Returns \item{p.value}{the p-value of the Triangle test;}
#' \item{Estimation}{estimation by Maximum Likelihood of the number of
#' panelists who really perceive the difference between the products;}
#' \item{ML}{Maximum Likelihood of the estimation of the number of panelists
#' who really perceive the difference between the products;}
#' \item{minimum}{minimum of panelists who should detect the odd product to can
#' say that panelists perceive the difference between the products.}
#' @author Francois Husson
#' @seealso \code{\link{triangle.test}}, \code{\link{triangle.design}}
#' @keywords models
#' @examples
#' 
#' triangle.pair.test (11, 20)  
#' 
#' @export triangle.pair.test
triangle.pair.test <- function(nb.good,nb.answer){
  if (nb.good > nb.answer) stop("nb.good should be less than nb.answer")
  p.value = pbinom(nb.good-1,nb.answer,1/3,lower.tail=FALSE)
  print(paste("P-value of the Triangle test:",round(p.value,5)))
  if (p.value<0.05) print("At the 95% level, one can say that the panelists make the difference between the two products")
  if (p.value>0.05) print("At the 95% level, one can say that the panelists do not make the difference between the two products")
  aux = matrix(0,nb.good+1,1)
  for (k in 0:nb.good) aux[k+1] = dbinom(nb.good-k,nb.answer-k,1/3)
  minimum = 0
  for (k in round(nb.answer/3):nb.answer) if(minimum==0 & (pbinom(k-1,nb.answer,1/3,lower.tail=FALSE)<0.05)) minimum=k
  print(paste("The estimation (by  Maximum Likelihood) of panelist which really perceived the difference between the two products is:", rev(order(aux))[1]-1))
  print(paste("The  Maximum Likelihood is: ",round(max(aux),5)))
  print(paste("The minimum of panelists who should detect the odd product to can say that panelists perceive the difference between the products is:",minimum))
  res = list(p.value=p.value,Estimation=which.max(round(aux*1e10,0))-1,ML=round(max(aux),5),minimum=minimum)
  return(res)
}
