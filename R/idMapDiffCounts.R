
###########################################################################/**
# @RdocClass IdMapDiffCounts
# \encoding{latin1}
#
# @title "The IdMapDiffCounts class"
#
# \description{
#  @classhierarchy
#
# The IdMapDiffCounts class handles statistics on IdMapDiff object.
# IdMapDiffCounts object encapsulates a data frame with row names corresponding 
# to the primary IDs and 6 columns subdivided into pairs <match(TRUE/FALSE),count> 
# each pair corresponding to the disjoint events <A-A*B,A*B,B-A*B>, where A and B
# are secondary ID lists for ID Maps A and B from the IdMapDiff object. The 'pairNames' attribute
# of the IdMapDiffCounts contains the names of the source ID Map pair from which the IdMapDiff object was created.
# }
# 
# @synopsis
#
# \arguments{
# \item{idMapDiff}{The IdMapDiff on which IdMapDiffCounts is cretated. Default is @NULL.}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used.}
# }
#
# \examples{
# #get primary IDs from an msms experiment set
# IDs<-IdMapBase$primaryIDs(examples$msmsExperimentSet);
#  
# #create JointIdMap object aligned by primaryIDs
# jointIdMap<-JointIdMap(examples$identDfList,primaryIDs=IDs);
# 
# #create IdMapDiff object
# diffs<-jointIdMap$getDiff("NetAffx_F","DAVID_Q",verbose=TRUE);
#
# # create IdMapDiffCounts object
# diffCounts<-IdMapDiffCounts(diffs);
# diffCounts[1:10,];
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setConstructorS3("IdMapDiffCounts",function(idMapDiff=NULL,verbose=FALSE,...){
	if(is.null(idMapDiff)){
		extend(IdMapBase(),"IdMapDiffCounts",
			.pairNames=c("From","To")
		);
	} else {
		if (verbose)
			cat("creating ID Map pair diff. statistics object...\n"); 

  		L0<-list(character(0));
   		rows<-nrow(idMapDiff);
		

 	  	res<-with(idMapDiff$.df, {

			data.frame (
				primaryIDs(idMapDiff),
				is_a_b=!(a_b %in% L0),
				a_b_count=sapply(1:rows,function(i) length(a_b[i][[1]])),
				is_ab=!(ab %in% L0),
				ab_count=sapply(1:rows,function(i) length(ab[i][[1]])),
				is_b_a=!(b_a %in% L0),
				b_a_count=sapply(1:rows,function(i) length(b_a[i][[1]]))
			)
		})
		colnames(res)[1]<-primaryKey(idMapDiff);
		rownames(res)<-idMapDiff[,1];

		extend(IdMapBase(res,secondaryKey=secondaryKey(idMapDiff)),"IdMapDiffCounts",
			.pairNames=idMapDiff$.pairNames
		);

	}
})

 
###########################################################################/**
# @RdocMethod getCompoundEvents
# 
# @title "Get compound events"
#
# \description{
# Transforms the IdMapDiffCounts triplet <A-A*B, A*B, B-A*B> 
# into the <excess both, excess left etc.> form suitable for using 
# within the fountain plot. Computed on the fly within the IdMapDiffCounts$summary()
# and IdMapDiffCounts$plot() functions.
# }
# 
# @synopsis
#
# \arguments{
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{
# A @data.frame with 7 columns each corresponding to a particular compaund set
# derived from the event triplet <A-A*B, A*B, B-A*B> 
# and with rows containing mathches (TRUE or FALSE) for a given primary ID.
# The columns and the compaund set description are as follows:
# \item{excess.both}{A and B have non-empty intersection and both sets have unique elements}
# \item{excess.left}{A and B have non-empty intersection and only A has unique elements}
# \item{excess.right}{A and B have non-empty intersection and only B has unique elements}
# \item{same.list}{Non-empty A and B are the same}
# \item{in.left.only}{Only A set is non-empty}
# \item{in.right.only}{Only B set is non-empty}
# \item{in.neither}{Both sets are empty}
# }
#
# \examples{
# #create IdMapDiffCounts object
# IDs<-IdMapBase$primaryIDs(examples$msmsExperimentSet);
# jointIdMap<-JointIdMap(examples$identDfList,primaryIDs=IDs);
# diffs<-jointIdMap$getDiff("NetAffx_F","DAVID_Q",verbose=TRUE);
# diffCounts<-IdMapDiffCounts(diffs);
# 
# # get compound events
# events<-diffCounts$getCompoundEvents();
# events[1:10,];
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getCompoundEvents","IdMapDiffCounts",function(this,verbose=FALSE,...){
	res<-with(this$.df,{
		res<-list();		
		res$excess.both<-(is_a_b & is_b_a);
		res$excess.left<-(is_a_b & is_ab & !is_b_a);
		res$excess.right<-(!is_a_b & is_ab & is_b_a);
		res$same.list<-(!is_a_b & is_ab & !is_b_a);
		res$in.left.only<-(is_a_b & !is_ab & !is_b_a);
		res$in.right.only<-(!is_a_b & !is_ab & is_b_a);
		res$in.neither<-(!(is_a_b | is_ab |is_b_a));
		res;
	});
	res<-as.data.frame(res);
	rownames(res)<-rownames(this);
	attr(res,"pairNames")<-this$.pairNames;
	return(res);

})


###########################################################################/**
# @RdocMethod getCompoundGroups
# 
# @title "Get counts for each compound event in IdMapDiffCounts"
#
# \description{
# @get "title". Used internally within IdMapDiffCounts$plot().
# }
# 
# @synopsis
#
# \arguments{
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{
# A list of data frames corresponding to a particular compaund event
# ('excess both', 'excess left' etc.) where each data frame has the same structure as a data frame 
# within the IdMapDiffCounts object.
# }
#
# \examples{
# #create IdMapDiffCounts object
# IDs<-IdMapBase$primaryIDs(examples$msmsExperimentSet);
# jointIdMap<-JointIdMap(examples$identDfList,primaryIDs=IDs);
# diffs<-jointIdMap$getDiff("NetAffx_F","DAVID_Q",verbose=TRUE);
# diffCounts<-IdMapDiffCounts(diffs);
# 
# # get compound events
# groups<-diffCounts$getCompoundGroups();
# names(groups);
# groups$excess.both[1:10,];
# groups$same.list[1:10,];
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getCompoundGroups","IdMapDiffCounts",function(this,verbose=FALSE,...){
	compoundEvents<-getCompoundEvents(this);
	res<-list();
	events<-colnames(compoundEvents);
	for (event in events){
		res[[event]]<-this[compoundEvents[[event]],];
	}
	return(res);
})

#' @export summary.IdMapDiffCounts
###########################################################################/**
# @RdocMethod summary
# 
# @title "Get a compaund event counts summary report"
#
# \description{
# Get a counts summary report where each row is a count of matches 
# corresponding to a particular compound event
# }
# 
# @synopsis
#
# \arguments{
# \item{...}{Not used}
# }
#
# \value{
# A @data.frame with a single 'counts' column 
# where each row is a count of matches corresponding to a particular compound event.
# }
#
# \examples{
# #create IdMapDiffCounts object
# IDs<-IdMapBase$primaryIDs(examples$msmsExperimentSet);
# jointIdMap<-JointIdMap(examples$identDfList,primaryIDs=IDs);
# diffs<-jointIdMap$getDiff("NetAffx_F","DAVID_Q",verbose=TRUE);
# diffCounts<-IdMapDiffCounts(diffs);
# 
# # get summary
# diffCounts$summary(); 
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("summary","IdMapDiffCounts",function(object,...){

	compoundEvents<-getCompoundEvents(object);
	res<-as.data.frame(colSums(compoundEvents));
	colnames(res)<-"counts";
	attr(res,"pairNames")<-object$.pairNames;

	return(res);
},createGeneric=FALSE)

#' @export plot.IdMapDiffCounts
###########################################################################/**
# @RdocMethod plot
# 
# @title "Produce a fountain plot representing the quantitative relationship of the compound events"
#
# \description{
# @get "title" 
# <'excess both', 'excess left', 'excess right', 'same list', 'in left only', 'in right only', 'in neither' >
# }
# 
# @synopsis
#
# \arguments{
# \item{valRange}{@numeric vector of length 3 where the first and second elements
# are minimum and maximum count values to be displayed on horizontal axis and the third
# element is a distance between horizontal axis tick marks. Default is c(-20,20,10).}
# \item{reverse}{logical indicating the plot orientation (top to bottom or bottom to top)}
# \item{pairLabels}{optional decorated names for ID Map pair in consideration. If @NULL (default),
# the original ID Map names are used.}
# \item{guideline.col}{guideline(horisontal group divider) lines color. Default is 'darkgrey'.}
# \item{guideline.lty}{guideline lines type. Default is 2 (dashed).}
# \item{guideline.lwd}{guideline lines color. Default is par.zoom.}
# \item{cols}{colors corresponding to the <A-A*B, A*B, B-A*B> events from which the plot is composed.}
# \item{sides}{How compound events labels and counts are placed on plot.
# Possible values are 1 or 2. If sides=1 both event labels and counts are placed on one (left) side.
# If sides=2 then labels are placed on left side and counts on the right.}
# \item{cex}{plot title font size.}
# \item{cex.side}{compound events side labels font size.}
# \item{srt}{compound events labels orientation.}
# \item{adj}{compound events labels position adjustment (0 - 1), where 0/1 corresponds 
# to the minimum/maximum labels position shift inwards the plot.}
# \item{par.zoom}{graphics parameters zoom factor. Scales the graphical parameters like cex, lwd, mai etc.}
# \item{...}{Additional graphical parameters}
# }
#
# \examples{
# #create IdMapDiffCounts object
# IDs<-IdMapBase$primaryIDs(examples$msmsExperimentSet);
# jointIdMap<-JointIdMap(examples$identDfList,primaryIDs=IDs);
# diffs<-jointIdMap$getDiff("NetAffx_F","DAVID_Q",verbose=TRUE);
# diffCounts<-IdMapDiffCounts(diffs);
# 
# # fountain plot of DB pair differences
# # two-sided labels
# diffCounts$plot(sides=2);
# # one-sided labels
# diffCounts$plot(sides=1); 
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setMethodS3("plot","IdMapDiffCounts",function(x, valRange=c(-20,20,10),reverse=FALSE, pairLabels=NULL,
	guideline.col="darkgrey",guideline.lty=2,guideline.lwd=par.zoom,
	cols=c("red","blue","green"),sides=2, cex=1, cex.side=0.75*cex, srt=0, adj=0.5,par.zoom=1,...){

	groupNames<-c("in.neither","in.right.only","in.left.only","same.list","excess.right","excess.left","excess.both");
	if (reverse)
		groupNames<-rev(groupNames);

	#recursive plot a group of values
	plotValGroup<-function(vals,oriX,y,side,col,reverse,subgroup=NULL,subcol=NULL){
		tvals<-table(vals);		
		dims<-data.frame(val=as.numeric(names(tvals)),freq=as.integer(tvals));
		if(reverse && nrow(dims)>1)
			dims<-dims[order(dims[,1],decreasing=TRUE),];

		if(nrow(dims)<1)
			return(y);

		for (i in 1:nrow(dims)){			
			left<-min(max(oriX,valRange[1]),valRange[2]);
			right<-min(max(oriX+side*dims[i,1],valRange[1]),valRange[2]);
			if(left!=right)
				rect(left,y,right,y+dims[i,2],border=col,col=col,lwd=0.5);
			if(!is.null(subgroup)){
				subvals<-subgroup[vals==dims[i,1]];
				plotValGroup(subvals,oriX+side*dims[i,1],y,side,subcol,reverse);
			}
			y<-y+dims[i,2];
		}
		
		return(y);	
	}

	#plot a compound event group
	plotGroup<-function(group,oriY,cols,reverse){
		plotValGroup(group[,"ab_count"],0,oriY,side=-1,col=cols[2],reverse,
					subgroup=group[,"a_b_count"],subcol=cols[1]);
		plotValGroup(group[,"ab_count"],0,oriY,side=1,col=cols[2],reverse,
					subgroup=group[,"b_a_count"],subcol=cols[3]);
		
	}

	####plot parameters setup
	#adj<-max(0,min(1,adj));
	two.sides=as.logical(round(max(min(sides,2),1))-1);

	nrows<-nrow(x);
	
	coeff<-(valRange[2]-valRange[1])/40;
	excess=1*coeff;
	d_wLink=2.5*coeff;
	h_wLink=2*coeff;
	wLink<-d_wLink+h_wLink;

	
	####retrieve left & right labels
	if (is.null(pairLabels))
		pairLabels<-x$.pairNames;
	if (is.null(pairLabels))
		pairLabels<-c("Left","Right");

	####retrieve side labels
	summary<-summary(x);
	total<-sum(summary[groupNames,1]);
	groupLabels<-gsub("."," ",groupNames,fixed=TRUE);
	summary_percent<-round(summary[groupNames,1]/total*100,digits=1);
	summaryLabels<-paste(summary[groupNames,1],paste("(",summary_percent,"%)",sep=""),sep=" ");

	####retrieve group dividers coordinates	
	X<-c(valRange[1]-wLink-excess,valRange[1]-d_wLink-excess,valRange[1]-excess,
		valRange[2]+excess,valRange[2]+d_wLink+excess,valRange[2]+wLink+excess);
	outerY<-seq(from=0,to=nrows,length.out=length(groupNames)+1);
	groupY<-rep(0,length(groupNames)+1);
	
	s<-0;
	for(i in 1:length(groupNames)){
		groupY[i+1]<-s<-s+summary[groupNames[i],1];
	}

	Y<-cbind(outerY,outerY,groupY,groupY,outerY,outerY);
	labelY<-(outerY[-1]+outerY[-length(outerY)])/2;

	if (!two.sides){
		X<-X[1:4];
		Y<-Y[,1:4];
	}


	####layout the plot

	op<-Display$zoom.pars(par.zoom);

	plot.new();
	if (two.sides){
		txtbox<-Display$textBoundingBox(c(groupLabels,summaryLabels),srt=srt,cex=cex.side,units="inches");	
	}else{
		txtbox<-Display$textBoundingBox(paste(groupLabels,summaryLabels,sep="\n"),srt=srt,cex=cex.side,"inches");	
	}

	mai<-par("mai");
	mai[2]<-max(mai[2],txtbox$width+0.2);
	if (two.sides)
		mai[4]<-max(mai[4],txtbox$width);
	mai<-par(mai=mai);

	plotRangeX<-c(X[1],X[length(X)]);
    	plot.window(c(X[1],X[length(X)]),c(1,nrows));

	opts<-list(...);
	main<-ifelse("main" %in% names(opts), opts$main,"Comparisons of IDs retrieved");
	xlab<-ifelse("xlab" %in% names(opts),opts$xlab,
		 paste("# of",secondaryKey(x),"matches found"));
	ylab<-ifelse("ylab" %in% names(opts),opts$ylab,primaryKey(x));

	title(main=main,xlab=xlab,cex.main=cex,cex.lab=cex);
	title(ylab=ylab,line=par()$mar[2]-1.2,cex.lab=cex);

	ats<-seq(valRange[1],valRange[2],valRange[3])
	axis(1, at=ats, labels=as.character(ats),cex.axis=0.8*cex,lwd=par("lwd"));
	mtext(paste(pairLabels[1], "                   ", pairLabels[2]),at=0,cex=cex*par.zoom)

	####plot horisontal goup dividers
	for(i in 1:nrow(Y))
		lines(X,Y[i,],lty=guideline.lty,col=guideline.col,lwd=guideline.lwd);

	####side text: two or one side
	if (two.sides){
		txtbox<-Display$textBoundingBox(c(groupLabels,summaryLabels),srt=srt,cex=cex.side);	
	}else{
		txtbox<-Display$textBoundingBox(paste(groupLabels,summaryLabels,sep="\n"),srt=srt,cex=cex.side);	
	}

	textX1<-valRange[1]-d_wLink-excess-txtbox$width/2-h_wLink*adj;

	if(two.sides){
		textX2<-plotRangeX[1]+plotRangeX[2]-textX1;
		text(textX1,labelY,labels=groupLabels,cex=cex.side,srt=srt,xpd=NA);
		text(textX2,labelY,labels=summaryLabels,cex=cex.side,srt=srt,xpd=NA);

		text(textX1,-0.55*(labelY[2]-labelY[1])/2,labels="total:",cex=cex.side,srt=srt,xpd=NA,font=2);
		text(textX2,-0.55*(labelY[2]-labelY[1])/2,labels=total,cex=cex.side,srt=srt,xpd=NA,font=2);

	}else{
		text(textX1,labelY,labels=paste(groupLabels,summaryLabels,sep="\n"),cex=cex.side,adj=c(0.5,0.5),srt=srt,xpd=NA);
		text(textX1,-0.4*(labelY[2]-labelY[1]),labels=paste("total:",total,sep="\n"),cex=cex.side,xpd=NA,font=2);
	}

	####fountain plot: comparisons by the group
	groups<-getCompoundGroups(x);
	oriY<-0;
	for (group in groups[groupNames]){
		oriY<-plotGroup(group,oriY,cols,reverse);
	}
		
	abline(v=0);

	par(mai=mai);
	par(op);	
	invisible();

},createGeneric=FALSE)


