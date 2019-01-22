#' Word-Count based methods Analysis (WordCountAna) graphs
#' 
#' Draw the Word-Count based methods Analysis (WordCountAna) graphs
#' 
#' 
#' @param x an object of class VerbAna
#' @param axes a length 2 vector specifying the components to plot
#' @param choix a string corresponding to the graph that you want to do ("prod"
#' for the products graph, "panel" for the panellists graph, "dist" for the
#' distinct-words graph, "cons" for the consensual words graph)
#' @param lab boolean, if TRUE, the labels are drawn
#' @param color the color to use to draw the graph
#' @param pch either an integer specifying a symbol or a single character to be
#' used as the default in plotting points. See points for possible values and
#' their interpretation
#' @param proba significance threshold considered to define consensual words
#' (by default 0.05)
#' @param xlim range for the plotted 'x' values, defaulting to the range of the
#' finite values of 'x'
#' @param ylim range for the plotted 'y' values, defaulting to the range of the
#' finite values of 'y'
#' @param cex numerical value giving the amount by which plotting text and
#' symbols should be magnified relative to the default (by default 1)
#' @param title string corresponding to the title of the graph you draw
#' @param new.plot boolean, if TRUE, a new graphical device is created
#' @param \dots further arguments passed to or from other methods, such as cex,
#' cex.main, ...
#' @return Returns one of the four following factor maps depending on the
#' choice: products, panellists, distinct-words and consensual words.
#' @author Belchin Kostov \email{badriyan@@clinic.ub.es}, Francois Husson
#' \email{husson@@agrocampus-ouest.fr}, Monica Becue-Bertaut
#' @seealso \code{\link{WordCountAna}}
#' @references Kostov, B., Becue-Bertaut, M., Husson, F., Pages, J., Cadoret,
#' M., Torrens, J. and Urpi, P. (2012). A tool for detecting words with
#' consensual meaning in verbalization tasks. 11th Sensometrics Conference,
#' July 10-13, 2012, Rennes, France.
#' @keywords multivariate
#' @examples
#' 
#' data(perfume)
#' res<-WordCountAna(base=perfume,sep.word=";",graph=FALSE)
#' plot.WordCountAna(res,choix="prod")
#' plot.WordCountAna(res,choix="panel")
#' plot.WordCountAna(res,choix="dist")
#' plot.WordCountAna(res,choix="cons")
#' plot.WordCountAna(res,choix="cons",proba=0.1)
#' 
#' @export plot.WordCountAna
plot.WordCountAna<-function(x,axes=c(1,2),choix="prod",lab=TRUE,color=NULL,pch=NULL,proba=0.05,xlim=NULL,ylim=NULL,cex=1,title=NULL,new.plot=TRUE,...){
	res.WCA<-x
	if (!inherits(res.WCA,"WordCountAna")) stop("non convenient data")
	if (choix=="prod"){
		if (is.null(title)) title<-"Products"
        	coord<-res.WCA$mfact$ind$coord[,axes,drop=FALSE]
	  	if (is.null(color)) color="blue"
	  	if (is.null(pch)) pch=16
	}
	if (choix=="panel"){
		if (is.null(title)) title<-"Panellists"
        	coord<-res.WCA$mfact$group$coord[,axes,drop=FALSE]
        	if (is.null(color)) color="brown"
	  	if (is.null(pch)) pch=17
	  	xlim<-ylim<-c(0,1)
	}
	if (choix=="dist"){
	  if (is.null(title)) title<-"Distinct-words"
        coord<-res.WCA$centroids[,axes,drop=FALSE]
        if (is.null(color)) color="black"
	  if (is.null(pch)) pch=18
	}
	if (choix=="cons"){	
		if (is.null(title)) title<-"Consensual words"
        	coord<-res.WCA$centroids[which(rownames(res.WCA$centroids)%in%rownames(res.WCA$cons)[res.WCA$cons[,3]<proba]),axes,drop=FALSE]
        	if (is.null(color)) color="red"
	  	if (is.null(pch)) pch=15
	}
	lab.x<-paste("Dim ",axes[1]," (",signif(res.WCA$mfact$eig[axes[1],2],4)," %)",sep="")
	lab.y<-paste("Dim ",axes[2]," (",signif(res.WCA$mfact$eig[axes[2],2],4)," %)",sep="")
    	if (new.plot) dev.new()
	if (is.null(xlim)) {
            xmin<-xmax<-0
            xmin<-min(xmin,coord[,1])
            xmax<-max(xmax,coord[,1])
		xlim<-c(xmin,xmax)*1.2
	}
	if (is.null(ylim)) {
   		ymin<-ymax<-0
         	ymin<-min(ymin,coord[,2])
         	ymax<-max(ymax,coord[,2])	
		ylim<-c(ymin,ymax)*1.2
	}
	plot(coord,xlab=lab.x,ylab=lab.y,xlim=xlim,ylim=ylim,pch=pch,col=color,cex=cex,main=title,asp=1,...)
	if (lab) text(coord[,1],coord[,2],labels=rownames(coord),pos=3,cex=cex,col=color)
	if (choix!="panel") abline(v=0,h=0,lty=2)
}
