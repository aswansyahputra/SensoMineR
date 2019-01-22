#' Calcul de moyenne
#' 
#' Fonction intermediaire
#' 
#' axeACP: 1st col: identificateur, colonne suivante: coord sur axe, produit,
#' juge (avec 0 pour moyenne) les premieres lignes correspondent aux moyennes
#' tous juges confondus les suivantes aux notes individuelles, classees par
#' juge cree tab: tableau avec en ligne les juges, en colonne les produits (ou
#' si c'est une AFM les individus moyens ou partiels ou les 2) en profondeur
#' les coord sur les differents axes
#' 
#' @param axeACP matrix
#' @author F Husson
#' @keywords internal
#' @export calculate.average
"calculate.average" <- function(axeACP){
nbcoord <- ncol(axeACP)-2
nbjuge <- -1+length(levels(axeACP[,dim(axeACP)[[2]]]))
nbprod <- length(levels(axeACP[,dim(axeACP)[[2]]-1]))
  tab<-array(0,dim=c(nbjuge,nbprod,nbcoord))
  moy<-matrix(0,nbcoord,nbprod)
  for (j in 1:nbprod){
  for (k in 1:nbcoord){
    moy[k,j]<-axeACP[j,k]
    }}
  for (k in 1:nbcoord){
  for (i in 1:nbjuge){
  for (j in 1:nbprod){
    tab[i,j,k]<-axeACP[nbprod+((i-1)*nbprod+j),k]
    }}}
  calc.moy <- list()
  calc.moy$tab <- tab
  calc.moy$moy <- moy
  return(calc.moy)
}
