


###########################################################################/**
# @RdocClass Corr
# \encoding{latin1}
#
# @title "The Corr class"
#
# \description{
#  @classhierarchy
#
# Create the Corr object by performing correlations on the CorrData object
# using the correlation algorithm defined by the method argument.
# The Corr object encapsulates a @data.frame containing three columns: 
# the first two are unique pairs and the third is a correlation results with a column name
# reflecting the correlation method ('pearson', 'spearman' or 'kendall').
# }
# 
# @synopsis
#
# \arguments{
# \item{corrData}{CorrData object on which correlation is performed
#  or a @data.frame compliant with the Corr object internal data frame format.}
# \item{method}{Correlation method ('pearson', 'spearman' or 'kendall').
# Default is 'pearson'.}
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used.}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \examples{
# corr<-Corr(examples$corrData,method="spearman",verbose=TRUE);
# class(corr);
# corr[1:10,];
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setConstructorS3("Corr",function(corrData=NULL,method="pearson",verbose=FALSE,...){
	if(is.null(corrData)){ 
		return(extend(IdMapBase(),"Corr"));
	}

	if(inherits(corrData,"data.frame") && ncol(corrData)==3){
		return(extend(IdMapBase(corrData,secondaryKey=colnames(corrData)[2]),"Corr"));
	}

	if(!inherits(corrData,"CorrData"))
		throw("CorrData constructor: the first argument is not of class CorrData");

 	# performing correlations on analysis data
	if (verbose)
		cat("Performing correlations...\n");

	analysisStructure<-corrData$.data;

	rows<-dim(analysisStructure[[1]])[1];
	cols<-dim(analysisStructure[[1]])[2];
	datacols<-c(2:cols);

	X<-data.matrix(analysisStructure[[1]][,datacols]);
	Y<-data.matrix(analysisStructure[[2]][,datacols]);
	correlations<-array(NA,dim=c(rows,3));
	for (i in 1:rows) {
		correlations[i,3]<-cor(X[i,],Y[i,],use="na.or.complete",method=method);
	}
	correlations[,1]<-(analysisStructure[[1]][,1]);
	correlations[,2]<-(analysisStructure[[2]][,1]);

	colnames(correlations)<-c(names(analysisStructure),method);
	res<-as.data.frame(correlations,stringsAsFactors=FALSE);
	res[,3]<-as.numeric(res[,3]);

	extend(IdMapBase(res,secondaryKey=secondaryKey(corrData)),"Corr");
})



###########################################################################/**
# @RdocMethod getUniquePairs
# 
# @title "Extract unique pairs from the Corr object"
#
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{...}{Not used}
# }
#
# \value{UniquePairs object}
#
# \examples{
# uniquePairs<-examples$corr$getUniquePairs();
# uniquePairs[1:10,];
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################



setMethodS3("getUniquePairs","Corr",function(this,...){
	return(UniquePairs(this[,c(1,2)]));
})



###########################################################################/**
# @RdocMethod getData
# 
# @title "Extract correlation results from the Corr object"
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
# @numeric vector of correlation results.
# }
#
# \examples{
# dat<-examples$corr$getData();
# dat[1:20];
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getData","Corr",function(this,...){
	return(this[,3]);
})


#' @export plot.Corr
###########################################################################/**
# @RdocMethod plot
# 
# @title "Plot the density distributions for correlation object(s)"
#
# \description{
# Plot the density distributions for a given Corr object or list of Corr objects.
# The method can be used as object specific as well as static. In the latter case 
# the method can accept a list of Corr objects using Corr$plot(corrList) call signature. 
# }
# 
# @synopsis
#
# \arguments{
# \item{dataList}{ the list of Corr objects (used if static version of method invoked).
# Default is $NULL.}
# \item{title}{ main plot title. Default is 'nonparametric density fit'.}
# \item{lineColors}{ the vector of line colors (recycled if necessary) 
# for plotting the distributions of different Corr objects.
# If @NULL (default), the predefined set of colors is used.}
# \item{lineStyles}{ the vector of line styles (recycled if necessary) 
# for plotting the distributions of different Corr objects.
# If @NULL (default), the predefined set of line styles is used.}
# \item{lineWidths}{ the vector of line widths (recycled if necessary) 
# for plotting the distributions of different Corr objects. Default is 2.}
# \item{verbose}{ if @TRUE enables diagnostic messages.Default is @FALSE.}
# \item{cex.main}{ Main title font size. Default is 1.2.}
# \item{cex.lab}{ X and Y titles font size. Default is 1.}
# \item{cex.axis}{ X and Y axis labels font size. Default is 1.}
# \item{cex.legend}{ font size for the plot legend. Default is 1.}
# \item{par.zoom}{ graphics parameters zoom factor. Scales the graphical parameters like cex, lwd, mai etc.
# Used to plot into the file based device with dimensions 'zoom' times bigger than for on-screen device.} 
# \item{...}{additional graphical parameters}
# }
#
# \examples{
# examples$corr$plot();
#
# #create a set of corr. objects for a given DB subset treating subset
# #as a full group and then plot the correlation densities including union
# corrSet<-examples$jointUniquePairs$getCorr(examples$corr,
#		groups=c("union","EnVision_Q","NetAffx_Q","DAVID_Q","EnVision_Q"),
#		full.group=TRUE,verbose=TRUE);
# Corr$plot(corrSet);
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("plot","Corr",function(x,dataList=NULL,title="nonparametric density fit",
   lineColors=NULL,lineStyles=NULL,lineWidths=2,verbose=FALSE,
   cex.main=1.2,cex.lab=1,cex.axis=1,cex.legend=1,par.zoom=1,...) {

    if(!is.null(x$.df))
		dataList<-x;

    #create a contrasting palette;
    	if(is.null(lineColors))
	  	lineColors=c("black",brewer.pal(9,"Set1"));

    #create a set of line styles
	if(is.null(lineStyles))
	    	lineStyles<-c(1:6);


    if(verbose)
		cat("plotting the density distribution...\n");

   op<-Display$zoom.pars(par.zoom);
   lineWidths<-lineWidths*par.zoom;
 
   if (!inherits(dataList,"list"))
	   dataList<-list(dataList);

   #get density list and the x,y range
	xmin<-NULL;
	xmax<-NULL;
	ymin<-NULL;
	ymax<-NULL;

	densityList<-list();

	for (i in 1:length(dataList)) {
		data<-getData(dataList[[i]]);
		dens<-density(as.numeric(as.matrix(data)));
		xmin<-min(xmin,dens$x);
		xmax<-max(xmax,dens$x);
		ymin<-min(ymin,dens$y);
		ymax<-max(ymax,dens$y);
		densityList[[i]]<-dens;
	}
	names(densityList)<-names(dataList);

    #prepare plot
    	plot.new();
    	plot.window(c(xmin,xmax),c(ymin,ymax));
    	corrMethod<-colnames(dataList[[1]])[3];
    	title(main=title,xlab=paste("correlation (",corrMethod,")",sep=""),ylab="density",
		cex.main=cex.main,cex.lab=cex.lab,...);
    	axis(1,lwd=par("lwd"),cex.axis=cex.axis,...);
    	axis(2,lwd=par("lwd"),cex.axis=cex.axis,...);
    	box();

    #density drawing    
    	lcolors<-NULL;
    	lstyles<-NULL;
    	lwidths<-NULL;
    	for (i in 1:length(densityList)){
		dens<-densityList[[i]];
		color<-lineColors[((i-1) %% length(lineColors))+1];
		lstyle<-lineStyles[((i-1) %% length(lineStyles))+1];
		lwidth<-lineWidths[((i-1) %% length(lineWidths))+1];
		lines(dens,cex=.2,col=color,lty=lstyle,lwd=lwidth);
		lcolors<-c(lcolors,color);
		lstyles<-c(lstyles,lstyle);
		lwidths<-c(lwidths,lwidth);
    	}

    #legend drawing
	if (!is.null(names(dataList)))
		legend(x="topright",inset=0.05,legend=names(dataList),col=lcolors,lty=lstyles,lwd=lwidths,cex=cex.legend);

    #<0,0> axises
    abline(h=0);
    abline(v=0);

    par(op);	
    invisible();
},createGeneric=FALSE)


