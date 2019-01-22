#' Word-Count based methods Analysis (WordCountAna)
#' 
#' Sensory methods as labelled sorting task, check-all-that-apply (CATA),
#' ultra-flash profiling (UFP) and open-ended questions can be used to collect
#' free-text descriptions of products through word-count based methods. A data
#' frame with rows-products and columns-panellists is considered for the
#' analysis. WordCountAna performs a multiple factor analysis for contingency
#' tables keeping all the information in the comparison of the products. The
#' identification of the consensual words which have the same meaning for most
#' of the panellists eases the interpretation of the word-count based methods
#' and solves the problems arising from the large diversity of vocabulary as
#' the different meanings possibly associated to a same word. A test, based on
#' resampling techniques, allows for assessing the significance of the
#' consensus.
#' 
#' 
#' @param base a data frame with n rows (products) and p columns (panellists).
#' Each cell corresponds to a free-text description used to describe a product
#' by a panellist
#' @param sep.word a string with all the characters which correspond to
#' separator of words (by default, NULL and is considered equal to ";
#' (),?./:'!$=+;<>[]@-")
#' @param ncp number of dimensions kept in the results and to compute the
#' within-inertia
#' @param nb.panel minimum number of panellists who used the same word in order
#' to define consensual words (by default 3)
#' @param nb.simul number of bootstrap simulations (by default 500)
#' @param proba significance threshold considered to define consensual words
#' (by default 0.05)
#' @param graph boolean, if TRUE a graph is displayed
#' @param axes a length 2 vector specifying the components to plot
#' @return \item{mfact}{a list of matrices containing all the results for
#' multiple factor analysis for contingency tables} \item{dist.words}{a matrix
#' containing the results for distinct words (number of times that used and
#' number of panellists that pronounced)} \item{centroids}{a matrix containing
#' the coordinates of the centroids of distinct-words} \item{cons}{a matrix
#' containing the results of bootstrap resampling for distinct-words pronounced
#' by at least "nb.panel" panellists (number of times that used, number of
#' panellists that pronounced and the significance of the consensus)}
#' \item{cons.words}{a vector of consensual words assessed by bootstrap
#' resampling}
#' 
#' Returns the products factor map, panellists factor map, distinct-words
#' factor map and consensual words factor map.
#' @author Belchin Kostov \email{badriyan@@clinic.ub.es}, Francois Husson
#' \email{husson@@agrocampus-ouest.fr}, Monica Becue-Bertaut
#' @seealso \code{\link{textual}}, \code{\link{MFA}},
#' \code{\link{plot.WordCountAna}}
#' @references Becue-Bertaut, M. and Pages, J. (2008). Multiple factor analysis
#' and clustering of a mixture of quantitative, categorical and frequency data.
#' \emph{Computational Statistice and Data Analysis}, 52, 3255-3268. Kostov,
#' B., Becue-Bertaut, M. and Husson, F. (2012). Multiple Factor Analysis for
#' Contingency Tables in FactoMineR Package. \emph{The R journal} Kostov, B.,
#' Becue-Bertaut, M., Husson, F., Pages, J., Cadoret, M., Torrens, J. and Urpi,
#' P. (2012). A tool for detecting words with consensual meaning in
#' verbalization tasks. 11th Sensometrics Conference, July 10-13, 2012, Rennes,
#' France.
#' @keywords multivariate
#' @examples
#' 
#' data(perfume)
#' res<-WordCountAna(base=perfume,sep.word=";")
#' 
#' @export WordCountAna
WordCountAna<-function(base,sep.word=NULL,ncp=Inf,nb.panel=3,nb.simul=500,proba=0.05,graph=TRUE,axes=c(1,2)){
	within.inertia<-function(coord,weight) sum(t(t(coord)-apply(coord*weight,2,sum)/sum(weight))^2*weight)
	base<-as.data.frame(base)
	if (!inherits(base, "data.frame")) stop("base should be a data.frame")
	groups<-numeric()
	ind.words<-numeric()
	data <- matrix(0,nrow(base),0)
	for (i in 1:ncol(base)){
		ind.table<-textual(tab=cbind.data.frame(rownames(base),base[,i]),num.text=2,contingence.by=1,maj.in.min = TRUE,sep.word=sep.word)$cont.table
		ind.words<-c(ind.words,colnames(ind.table))
		colnames(ind.table)<-paste(colnames(ind.table),i,sep="")
		groups[i]<-ncol(ind.table)
		data<-cbind.data.frame(data,as.data.frame(ind.table))
	}
	dist.words<-levels(as.factor(ind.words))
	size.dist.words<-sapply(dist.words,function(dist.word,ind.words) sum(data[,which(ind.words==dist.word)]),ind.words)
	length.dist.words<-sapply(dist.words,function(dist.word,ind.words) length(which(ind.words==dist.word)),ind.words)
	info.dist.words<-as.data.frame(matrix(ncol=2,nrow=length(dist.words),dimnames=list(dist.words,c("Nb times","Nb panellists"))))
	info.dist.words[,1]<-size.dist.words
	info.dist.words[,2]<-length.dist.words
	info.dist.words<-info.dist.words[order(info.dist.words[,2],decreasing = TRUE),]
	res.mfact<-MFA(data,type=rep("f",ncol(base)),group=groups,ncp=ncp,graph=FALSE,name.group=1:ncol(base))
	sel.words<-names(which(sapply(dist.words,function(dist.word,ind.words) length(which(ind.words==dist.word)),ind.words)>=nb.panel))
	within<-sapply(sapply(sel.words,function(sel.word,ind.words) which(ind.words==sel.word),ind.words),function(pos) within.inertia(res.mfact$freq$coord[pos,],res.mfact$global.pca$call$col.w[pos]))
	within.dist<-list()
	size.sel.words<-sapply(sel.words,function(sel.word,ind.words) sum(data[,which(ind.words==sel.word)]),ind.words)
	length.sel.words<-sapply(sel.words,function(sel.word,ind.words) length(which(ind.words==sel.word)),ind.words)
	levels.length<-levels(as.factor(length.sel.words))
	for(i in 1:length(levels.length)){
		Rsamples<-replicate(nb.simul,sample(1:nrow(res.mfact$freq$coord),levels.length[i],replace=TRUE))
		within.dist[[i]]<-apply(Rsamples,2,function(pos) within.inertia(res.mfact$freq$coord[pos,],res.mfact$global.pca$call$col.w[pos]))
	}
	names(within.dist)<-levels.length
	pvalues<-numeric()
	for (i in 1:length(sel.words)) pvalues[i]<-length(which(within.dist[[which(names(within.dist)==length.sel.words[i])]]<within[i]))/nb.simul
	consensus<-as.data.frame(matrix(ncol=3,nrow=length(sel.words),dimnames=list(sel.words,c("Nb times","Nb panellists","Pvalue"))))
	consensus[,1]<-size.sel.words
	consensus[,2]<-length.sel.words
	consensus[,3]<-pvalues
	consensual.words<-which(consensus[,3]<proba)
	consensus<-consensus[order(consensus[,3]),]
	centroids<-t(sapply(sapply(dist.words,function(dist.word,ind.words) which(ind.words==dist.word),ind.words),function(pos) apply(sweep(as.data.frame(res.mfact$freq$coord)[pos,],1,res.mfact$global.pca$call$col.w[pos],"*"),2,sum)/sum(res.mfact$global.pca$call$col.w[pos])))
	resultats<-list(mfact=res.mfact,dist.words=info.dist.words,centroids=centroids,cons=consensus,cons.words=sel.words[consensual.words])
	class(resultats) <- c("WordCountAna", "list")
	if (graph){
		plot.WordCountAna(resultats,choix="prod")
		plot.WordCountAna(resultats,choix="panel")
		plot.WordCountAna(resultats,choix="dist")
		plot.WordCountAna(resultats,choix="cons")
	}
	return(resultats)
}
