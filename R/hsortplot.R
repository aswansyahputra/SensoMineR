#' Plot consumers' hierarchical sorting
#' 
#' Plot consumers' hierarchical sorting
#' 
#' The data used here refer to a specific experiment, where children were asked
#' to provide hierarchical sorting (several nested partitions) from 16 cards.
#' 
#' @param don a data frame with n rows (products) and p columns (nested
#' partitions for all consumers)
#' @param group a list indicating the number of levels (nested partitions) for
#' each consumer
#' @param numr the number of hierarchical sorting per row (by default 2)
#' @param numc the number of hierarchical sorting per column (by default 2)
#' @return Returns as many graphs as there are consumers, each graph represents
#' hierarchical sorting provided by a consumer
#' @author Marine Cadoret, S\'ebastien L\^e
#' \email{sebastien.le@@agrocampus-ouest.fr}
#' @seealso \code{\link{fahst}}
#' @keywords multivariate
#' @examples
#' 
#' \dontrun{
#' data(cards)
#' group.cards<-c(2,3,3,2,2,4,2,3,2,1,3,2,3,3,3,2,3,3,2,3,3,3,3,3,3,3,3,3,3,3)
#' hsortplot(cards,group.cards)
#' }
#' 
#' @export hsortplot
hsortplot=function(don,group,numr=2,numc=2){
nb.juge=length(group)
gp=0
for(i in 1:nb.juge){
distance=group[i]-tab.disjonctif(don[,(gp+1):(gp+group[i])])%*%t(tab.disjonctif(don[,(gp+1):(gp+group[i])]))
cah=cluster::agnes(distance,method="single",diss=TRUE)
if (((i-1)%%(numc*numr))==0){
dev.new()
par(mfrow = c(numr, numc))
}
plot(cah,main=paste("H-Sorting of subject",i,sep=" "),xlab="",sub="",which.plots=2)
gp=gp+group[i]
}}
