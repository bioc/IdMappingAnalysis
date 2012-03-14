
###########################################################################/**
# @RdocClass Mixture
# \encoding{latin1}
#
# @title "The Mixture class"
#
# \description{
#  @classhierarchy
#
# The constructor creates a model from a single Corr object using the number
# of clusters defined by G determining the optimal number of clusters by default
# and optionally using the Fisher transform.
# }
# 
# @synopsis
#
# \arguments{
# \item{corr}{ Corr object on wich mixture modeling is performed.}
# \item{G}{ number of components in mixture model. If G is a vector, the optimal
# number of components is determined. G is a vector (1:5) by default.}
# \item{Fisher}{ if @TRUE, the Fisher transform of correlation data is performed before
# the model is fitted. Default is @FALSE.}
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used.}
# }
#
# \value{
# The resulting Mixture object encapsulates a data member '.model'
# containing the results of mixture modeling represented by the @list
# with following components:
# \item{corr}{the correlation data}
# \item{clust}{the clustering results data structure returned by Mclust()}
# \item{sd}{standard deviation derived from clust$parameters$variance$sigmasq}
# \item{density}{the correlation density distribution}
# \item{marginalDensity}{the marginal density}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \examples{
# mixture<-Mixture(examples$corr,G=c(1:4),Fisher=TRUE,verbose=TRUE);
# class(mixture);
# names(mixture$.model)
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setConstructorS3("Mixture",function(corr=NULL,G=c(1:5),Fisher=FALSE,verbose=FALSE,...) {
	if(is.null(corr)){
		return(extend(Object(),"Mixture",
				.primaryKey=NULL,
				.secondaryKey=NULL,
				.model=NULL
		))
	}

	if (verbose)
		cat("computing mixture model for",G,"components...\n");

	corrData<-as.numeric(corr[,3]);
	model<-list();

	if (Fisher){
		corrData<-DataFilter$fisherTransform(corrData);
		model$clust<-Mclust(corrData,G=G);
	} else {
		model$clust<-Mclust(corrData,G=G,model="V");
	}
	model$corr<-(corrData);
	model$sd = sqrt(model$clust$parameters$variance$sigmasq);
	if(length(model$sd )==1)
		model$sd= rep(model$sd, model$clust$G);

	model$density<-density(model$corr)

	densities = outer(1:model$clust$G, model$corr,
		function(i, x) {
			dnorm(x, model$clust$parameters$mean[i], model$sd[i])*model$clust$parameters$pro[i];	}
		)

	model$marginalDensity<-apply(densities,2,sum);
	model$uniquePairs<-corr[,c(1:2)];
	
	extend(Object(),"Mixture",
		.primaryKey=primaryKey(corr),
		.secondaryKey=secondaryKey(corr),
		.model=model
	)
})

###########################################################################/**
# @RdocMethod primaryKey
# 
# @title "Retrieves a primary key for a given Mixture object"
#
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{\item{...}{Not used}}
#
# \value{A @character string representing primary key for given Mixture object }
#
# \examples{
# examples$mixture$primaryKey();
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("primaryKey","Mixture",function(this,...){
	return(this$.primaryKey);
})


###########################################################################/**
# @RdocMethod secondaryKey
# 
# @title "Retrieves a secondary key for a given Mixture object"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{\item{...}{Not used}}
#
# \value{A @character string representing secondary key for given Mixture object}
#
# \examples{
# examples$mixture$primaryKey();
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("secondaryKey","Mixture",function(this,...){
	return(this$.secondaryKey);
})


###########################################################################/**
# @RdocMethod getData
# 
# @title "Extract mixture component data from the Mixture object"
#
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{G}{Component number. If NULL (default), the highest component is returned.} 
# \item{...}{Not used}
# }
#
# \value{
# @numeric vector of mixture component data with 'Component' attribute
# indicating the component number retrieved.
# }
# 
# \examples{
# dat<-examples$mixture$getData();
# dat[1:20];
# attr(dat,"Component");
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getData","Mixture",function(this,G=NULL,...){
	if(is.null(G)){
		G<-this$.model$clust$G;
	} else {
		G<-max(1,min(G,ncol(this$.model$clust$z)));
	}

	res<-this$.model$clust$z[,G];
	attr(res,"Component")<-G;
	return(res);
})


###########################################################################/**
# @RdocMethod clust
# 
# @title "Retrieve the custering results data structure"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{...}{Not used}
# }
#
# \value{The clustering results data structure returned by Mclust()}
#
# \examples{
# cls<-examples$mixture$clust();
# names(cls);
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("clust","Mixture",function(this,...){
	return(this$.model$clust);
})


###########################################################################/**
# @RdocMethod getStats
# 
# @title "Get mixture component model summary info"
#
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{...}{Not used}
# }
#
# \value{
# A @data.frame with columns containing  mean, sd, weight and range of prevalence 
# (range.low,range.high), one column for each model component
# }
#
# \examples{
# examples$mixture$getStats();
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getStats","Mixture",function(this,...){
	stats<-NULL;
	groups<-unique(this$.model$clust$cl);
	for(group in groups){
		corSeqGroup<-(this$.model$corr[this$.model$clust$cl==group]);

		group.stats<-c(
			this$.model$clust$parameters$mean[group],
			this$.model$sd[group],
			this$.model$clust$parameters$pro[group],
			min(corSeqGroup),
			max(corSeqGroup)
		);
		stats<-cbind(stats,group.stats);
	}
	colnames(stats)<-paste("comp",groups,sep=".");
	rownames(stats)<-c("mean","sd","weight","range.low","range.high");
	return(as.data.frame(stats,stringsAsFactors=FALSE));
})


#' @export plot.Mixture
###########################################################################/**
# @RdocMethod plot
# 
# @title "Plot the results of mixture modeling"
#
# \description{
# Plot the correlation densities of the
# empirical fit, mixture fit and each of the mixture components
# }
# 
# @synopsis
#
# \arguments{
# \item{title}{ main plot title. Default is "nonparametric density fit".}
# \item{cex.main}{ Main title font size. Default is 1.2.}
# \item{cex.lab}{ X and Y titles font size. Default is 1.}
# \item{cex.axis}{ X and Y axis labels font size. Default is 1.}
# \item{cex.legend}{ font size for the plot legend. Default is 1.}
# \item{lineWidths}{ the vector of length 3 defing the line width for mixture fit, 
# empirical fit and the micture components curves correspondingly. Default is c(3,3,2).} 
# \item{lineTypes}{ the vector of length 3 defing the line type for mixture fit, 
# empirical fit and the micture components curves correspondingly. Default is c(1,2,3).}
# \item{plot.crossover}{If @TRUE (default), the vertical lines corresponding to pairwise intersection
# of mixture components are plotted.}
# \item{par.zoom}{ graphics parameters zoom factor. Scales the graphical parameters
# like cex, lwd, mai etc.}
# \item{...}{Additional graphical parameters}
# }
#
# \examples{
# examples$mixture$plot();
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setMethodS3("plot","Mixture",function(x,title="mixture density fit",
	cex.main=1.2,cex.lab=1,cex.axis=1,cex.legend=1,
	lineWidths=c(3,3,2),lineTypes=c(1,2,3),plot.crossover=TRUE,par.zoom=1,...){

	model<-x$.model;

	if(length(lineWidths)<3)
		lineWidths=c(3,3,2);

	if(length(lineTypes)<3)
		lineTypes=c(1,2,3);

   	op<-Display$zoom.pars(par.zoom);
   	lineWidths<-lineWidths*par.zoom;
 

	plot.new();
    	plot.window(c(min(model$density$x),max(model$density$x)),
			c(min(model$marginalDensity),max(model$marginalDensity)));


	axis(1,lwd=par("lwd"),cex.axis=cex.axis,...);
	axis(2,lwd=par("lwd"),cex.axis=cex.axis,...);
	box();
	title(main=title,xlab="correlation",ylab="density",cex.main=cex.main,cex.lab=cex.lab,...);
	
	Display$line.unsorted(model$corr,model$marginalDensity,lwd=lineWidths[1]);
	lines( density(model$corr),	col="orange", lty=2, lwd=lineWidths[2]);

	names<-c("mixture fit","empirical fit");
	cols<-c("black","orange");
	ltys<-lineTypes[1:2];
	lwds<-lineWidths[1:2];

	groups=sort(unique(model$clust$cl));

	for(group in groups){
		corSeqGroup <- (model$corr[model$clust$cl==group]);
		rug(corSeqGroup, col=group+1)
		Display$line.unsorted(model$corr, 
			dnorm(model$corr, 
				mean=model$clust$parameters$mean[group],
				sd=model$sd[group])
			* model$clust$parameters$pro[group],
			col=group+1,
			lwd=lineWidths[3],
			lty=lineTypes[3])
		names<-c(names,paste("component",group));
		cols<-c(cols,group+1);
		ltys<-c(ltys,3);
		lwds<-c(lwds,lineWidths[3]);
	}

	if(plot.crossover){
		for(i in 1:(length(groups)-1)){
			for (j in (i+1):length(groups)){
				clusters = sapply(c(i,j), function(group)
						dnorm(model$corr, 
							mean = model$clust$parameters$mean[group], 
            					sd = model$sd[group]) * model$clust$parameters$pro[group]);

				inds<-order(model$corr);
				clusters<-clusters[inds,];
				corr.sorted<-model$corr[inds];

				diffs<-clusters[,1] - clusters[,2];
				crossover<-(diffs > 0);
				crossover<-c(0,diff(crossover));
				crossover<-which(crossover != 0);
				if(length(crossover)>1)
					crossover<-crossover[which(clusters[crossover,1]==
						max(clusters[crossover,1]))];				

				lines(as.numeric(rbind(corr.sorted[crossover],corr.sorted[crossover])),
				as.numeric(rbind(rep(0,length(crossover)),clusters[crossover,1])),
					lty=1, lwd=2, col="grey");
			}
		}
	}

	legend(x="topright",legend=names,inset=0.05,col=cols,lwd=lwds,lty=ltys,cex=cex.legend);
	abline(v=0);
	par(op);

},createGeneric=FALSE)



