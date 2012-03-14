
###########################################################################/**
# @RdocClass IdMapCounts
# \encoding{latin1}
#
# @title "The IdMapCounts class"
#
# \description{
#  @classhierarchy
#
# An IdMapCounts object enapsulates a @data.frame 
# where the first column contains the primary ID set 
# while the rest of columns contain the counts 
# of secondary IDs for each Id Map in a given idMapList object, 
# one column per ID Map, each ID Map related column 
# having a name representing the given DB data source (i.e. 'NetAffx', 'EnVision' etc.)
# The constructor creates the IdMapCounts object from the list of ID Maps aligned by
# the primary IDs and primary and secondary keys.
# The easest way to obtain the list of properly aligned IdMap objects is to create a JointIdMap
# object from a set of un-aligned ID maps and then invoke the getIdMapList() method on
# this object. The IdMapCounts object can also be created directly from JointIdMap object
# by using the JointIdMap.$getCounts() method. 
# }
# 
# @synopsis
#
# \arguments{
# \item{idMapList}{The @list of ID Maps aligned on primary IDs.}
# \item{verbose}{If @TRUE enables diagnostic messages.Default is @FALSE}
# \item{...}{Not used.}
# }
#
# \examples{
# idMaps<-IdMap$as.list(examples$identDfList[[1]]);
# cnts<-IdMapCounts(IdMap(examples$identDfList[[1]]));
# cnts[1:20,];
#
# #create IdMapCounts object from aligned IdMap list.
# jointIdMap<-JointIdMap(examples$identDfList);
# idMaps<-jointIdMap$getIdMapList(verbose=TRUE);
# cnts<-IdMapCounts(idMaps);
# cnts[1:20,];
#
# #create IdMapCounts object directly from the JointIdMap object
# jointIdMap<-JointIdMap(examples$identDfList);
# cnts<-jointIdMap$getCounts(verbose=TRUE);
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setConstructorS3("IdMapCounts",function(idMapList=NULL,verbose=FALSE,...) {
	if(is.null(idMapList)){
		res<-NULL;
		secondaryKey="";
	} else {
		if(verbose)
			cat("creating Count object...\n");

		if(!inherits(idMapList,"list"))
			idMapList<-list(matches=idMapList);

		idMap<-idMapList[[1]];
		primaryKey<-primaryKey(idMap);
		secondaryKey<-secondaryKey(idMap);

		cnts<-array(0,dim=(c(nrow(idMap),length(idMapList))));
		cnts<-as.data.frame(cnts);
	
		for (i in 1:length(idMapList)){
			if (verbose)
				cat(names(idMapList)[i],"\n");
			idMap<-idMapList[[i]];	
	
			if (nrow(idMap$.df)!=nrow(cnts))
				throw("IdMapCounts constructor: ID Map dimensions mismatch");
			if (primaryKey(idMap)!=primaryKey)
				throw("IdMapCounts constructor: ID map primary key mismatch");
			if (secondaryKey(idMap)!=secondaryKey)
				throw("IdMapCounts constructor: ID map secondary key mismatch");
		
			cnts[,i]<-getCounts(idMap);
		}
		res<-as.data.frame(cbind(as.data.frame(idMapList[[1]])[,1],cnts),stringsAsFactors=FALSE);
		colnames(res)<-c(primaryKey,names(idMapList));
		rownames(res)<-res[,1];
	}
   
   	extend(IdMapBase(res,secondaryKey=secondaryKey),"IdMapCounts")
})



###########################################################################/**
# @RdocMethod getStats
# 
# @title "Retrieves a set of unique counts of secondary IDs"
# \description{
# @get "title"
# over the all ID Maps and then computes the number
# of entries and percentage corresponding to each count
# for all ID Maps within the given object
# }
# 
# @synopsis
#
# \arguments{
# \item{idCounts}{ IdMapCounts object or a list of objects on which summary is computed}
# \item{percent}{ logical indicating wherether the percentage column(s) should be included into summary}
# \item{digits}{ integer indicating the number of decimal places in percentage column}
# \item{summary}{ if @TRUE, the summary of counts is generated. Default is @FALSE.}
# \item{cutoff}{ If summary is @TRUE, all data for counts greater than cutoff are collapsed (summarized) into one row. Default is 3.}
# \item{verbose}{If @TRUE enables diagnostic messages.Default is @FALSE}
# \item{...}{Not used}
# }
#
# \value{
# Data frame representing a set of unique counts of secondary IDs.
# if summary is FALSE, the resulting data frame represent the number of entries and percentage corresponding to each count
# for all ID Maps within the object. If summary is TRUE, the resulting data frame represent the formatted summary of count statistics,
# including summarized data for counts grater than cutoff, maximum number of secondary IDs per primary ID
# and the total number of secondary IDs for each DB within the IdMapCounts  object.
# }
#
# \examples{
# jointIdMap<-JointIdMap(examples$identDfList);
# cnts<-jointIdMap$getCounts(verbose=TRUE);
# cnts$getStats();
# cnts$getStats(summary=TRUE,cutoff=4);
# }
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################



setMethodS3("getStats","IdMapCounts",function(this,percent=TRUE,digits=1,summary=FALSE,cutoff=3,verbose=FALSE,...){

	if(verbose)
		cat("getting Counts object summary...\n");

	cnts<-NULL;
	for (i in 2:ncol(this)){
		cnts<-c(cnts,this[,i]);		
	}
	cnts<-sort(unique(cnts));
	stats<-array(0,dim=c(length(cnts),ncol(this)-1));
	rownames(stats)<-as.character(cnts);
	colnames(stats)<-colnames(this)[-1];
	for (name in colnames(stats)){
		freqs<-table(this[,name]);
		stats[names(freqs),name]<-as.numeric(freqs);
	}

	if (percent || summary){
		stats2<-stats;
		colnames(stats2)<-paste(colnames(stats),"(%)");
		for (i in 1:ncol(stats2))
			stats2[,i]<-round(stats2[,i]/sum(stats2[,i])*100,digits);
		if (percent)
			stats<-Misc$interleave(stats,stats2);
	}

	stats<-cbind(cnts,stats);
	stats<-as.data.frame(stats,stringsAsFactors=FALSE);

	
	if (summary){

		res<-list(stats=stats);
		cutoff<-max(0,min(max(stats[,1]),cutoff));

		#stats (%) for cnts<=cutoff
		sums<-t(stats2[cnts<=cutoff,]);

		#stats (%) for cnts>cutoff
		sums<-cbind(sums,100-colSums(as.matrix(stats2[cnts<=cutoff,])));
		colnames(sums)[ncol(sums)]<-paste(">",cutoff,sep="");

		#max counts
		max_counts<-rep(0,times=ncol(this)-1);
		for (i in 2:ncol(this))
			max_counts[i-1]<-max(this[,i]);
		
		sums<-cbind(sums,max_counts);
		colnames(sums)[ncol(sums)]<-"Max.count";
		
		#total count
		sums<-cbind(sums,colSums(as.matrix(this[,-1])));
		colnames(sums)[ncol(sums)]<-"Total.count";

		rownames(sums)<-colnames(this)[-1];

		res<-sums;
	} else {
		res<-stats;
	}

	return(res);
})


#' @export plot.IdMapCounts
###########################################################################/**
# @RdocMethod plot
# 
# @title "Compute and plot the (inversed) ecdf for each ID Map count entry within the IdMapCounts object"
# \description{
# @get "title".
# }
# 
# @synopsis
#
# \arguments{
# \item{idMapNames}{ optional subset of ID Map names within the counts object
# for which ecdf's should be plotted. If @NULL (default), the ecdf's for all ID Maps 
# within the object are plotted.}
# \item{complement}{ @logical indicating wherether the complementary ecdf should be plotted
# The default is @TRUE}
# \item{log}{ @logical indicating if log scale should be used. The default is @TRUE.}
# \item{lineColors}{ Recyclable vector of line colors. If @NULL (default), a set of predefined colors is used.} 
# \item{pointSymbols}{ Character or numeric recyclable vector of symbols used to plotting points.
# If @NULL (default), a set of predefined symbols is used.}
# \item{pointColors}{ Recyclable vector of point colors. If NULL (default), a set of predefined colors is used.} 
# \item{cex.main}{ Font size for plot main title. Default is 1.2.}
# \item{cex.lab}{ Font size for X and Y axis titles. Default is 1.}
# \item{cex.axis}{ Font sixe for axis labels. Default is 1.}
# \item{cex.legend}{ Legend font size. Default is 1.}
# \item{par.zoom}{ graphics parameters zoom factor. Scales the graphical parameters like cex, lwd, mai etc.}
# \item{...}{Additional graphical parameters}
# }
#
# \examples{
# jointIdMap<-JointIdMap(examples$identDfList);
# cnts<-jointIdMap$getCounts();
# cnts$plot();
# }
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setMethodS3("plot","IdMapCounts",function(x,idMapNames=NULL,complement=TRUE,log=TRUE,
	lineColors=NULL,pointSymbols=NULL,pointColors=NULL,cex.main=1.2,cex.lab=1,cex.axis=1,cex.legend=1,par.zoom=1,...){

	cnts<-x;

	default_colors<-brewer.pal(9,"Set1");
	default_names<-colnames(cnts$.df)[-1];
	primaryKey<-primaryKey(cnts);


	if (is.null(idMapNames))
		idMapNames<-colnames(cnts)[-1];

	if(is.list(idMapNames)){		
		mapNames<-names(idMapNames)[names(idMapNames) %in% default_names];
		decoratedNames<-unlist(idMapNames[mapNames]);
	} else {
		mapNames<-idMapNames[idMapNames %in% default_names];
		decoratedNames<-mapNames;
	}

	if(is.null(lineColors))
		lineColors<-default_colors;
	if(is.null(pointSymbols))
		pointSymbols <-c(1,2,5,6,11,15,16,17,18,20);
	if(is.null(pointColors))
		pointColors<-default_colors;

	
	summary<-getStats(cnts,percent=FALSE,verbose=FALSE);

	countsDF<-as.data.frame(cnts[,mapNames]);
	colnames(countsDF)<-mapNames;
	nrows<-nrow(countsDF);

	xmax<-max(countsDF);
	if(log){
		ymax=1;
	}else{ 
		ymax<-nrows;
	}

    	#prepare plot
	op<-Display$zoom.pars(par.zoom);
    	plot.new();
    	plot.window(c(0,xmax),c(0,1));

	xlab<-paste("# of '",secondaryKey(cnts),"' matching ' ",primaryKey(cnts),"'"); 
		
	if(complement){
    		title(main="Complementary ECDF",
			xlab=xlab,,
			ylab="proportion (complement of cumulative)",cex.main=cex.main,cex.lab=cex.lab,...);
	}else{
    		title(main="ECDF",
			xlab=xlab,
			ylab="cumulative proportion",cex.main=cex.main,cex.lab=cex.lab,...);
	}

    	axis(1,lwd=par("lwd"),cex.axis=cex.axis,...);
	if (log) {
		at<-seq(from=1e-4,to=1,length.out=5);
    		axis(2,lwd=par("lwd"),at=at,labels=c("1e-04","1e-03","1e-02","1e-01","1e+00"),cex.axis=cex.axis,...);
	} else {
   		axis(2,lwd=par("lwd"),cex.axis=cex.axis,...);
	}
    	box();

  	#plot ecdf
	lcolors<-NULL;
	scolors<-NULL;
	labels<-NULL;
	for (i in 1:ncol(countsDF)){
		name<-mapNames[i];
		f<-ecdf(countsDF[,name]);
		x<-sort(unique(countsDF[,name]));
		y<-f(x);
		if (complement){
			y<-1-y;
			if (log)
				y<-log2(nrows*y+1)/log2(nrows+1);
		} else if (log){
				y<-1-y;
				y<-log2(nrows*y+1)/log2(nrows+1);
				y<-1-y;
		}

		lcolor<-lineColors[(i-1)%%length(lineColors)+1];
		scolor<-pointColors[(i-1)%%length(pointColors)+1];
		label<-pointSymbols [(i-1)%%length(pointSymbols )+1];

		lines(x,y,type="s",col=lcolor,...);

		if(log){
			lines(x[-length(x)],y[-length(y)],type="p",pch=label,col=scolor,...);
		}else{
			lines(x,y,type="p",pch=label,col=scolor,...);
		}

		lcolors<-c(lcolors,lcolor);
		scolors<-c(scolors,scolor);
		labels<-c(labels,label);
		

	}
	
	#plot legend

	info<-legend(0,1,legend=decoratedNames,col=lcolors,lwd=par("lwd"),pch=labels,cex=cex.legend,plot=FALSE);
	w<-info$rect$w;
	h<-info$rect$h;
	if (complement){
		legend(x="topright",inset=0.07,legend=decoratedNames,col=lcolors,lwd=par("lwd"),pch=NULL,cex=cex.legend);
		legend(x="topright",inset=0.07,legend=decoratedNames,col=scolors,lty="blank",pch=labels,cex=cex.legend,bty="n",bg="transparent");
	}else{
		legend(x="bottomright",inset=0.07,legend=decoratedNames,col=lcolors,lwd=par("lwd"),pch=NULL,cex=cex.legend);
		legend(x="bottomright",inset=0.07,legend=decoratedNames,col=scolors,lty="blank",pch=labels,cex=cex.legend,bty="n",bg="transparent");
	}

	par(op);
	invisible();


},createGeneric=FALSE)




