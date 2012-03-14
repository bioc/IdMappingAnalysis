###########################################################################/**
# @set "class=JointUniquePairs"
# @RdocMethod boxplotdataJitter
# 
# @title "Plot a set of points representing the density distribution of a data set"
#
# \description{
# @get "title".
# }
# 
# @synopsis
#
# \arguments{
# \item{y}{@numeric vector representing a data set.}
# \item{round}{If @TRUE (default), then use rounding to reduce the amount of redundant plotting}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Additional graphical parameters}
# }
#
# \value{
# A @data.frame with two columns 'x' and 'y' which can be used 
# to overlay the box plot with density distribution using plot(x,y)
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("boxplotdataJitter","JointUniquePairs",function(static,y,round,verbose=FALSE,...)
{
   nCirclesPerInch = 11.48936
   linesPerYAxis = nCirclesPerInch * par("pin")[2]
   if (verbose)
       cat("linesPerYAxis ", linesPerYAxis,"\n")
   if (length(y) == 0) {
       return(cbind(x = numeric(0), y = numeric(0)))
   }
   y = sort(y)
   if (round & (max(y) > min(y)))
       ygroup = round(linesPerYAxis * (y - min(y))/(max(y) -
           min(y)))
   else ygroup = y
   ty = table(ygroup)
   maxEqual = max(ty)
   if (verbose)
       cat("maxEqual ", maxEqual,"\n")
   x = unlist(lapply(ty, function(n)cumsum((0:(n-1))*(-1)^(1:n))
   ))
   xUserPerInch = (par("usr")[2] - par("usr")[1])/par("pin")[1]
   circleWidthUser = xUserPerInch/nCirclesPerInch
   if (verbose)
       cat("circleWidthUser= ", circleWidthUser,"\n")
   xoffset = min(circleWidthUser, 2/3/maxEqual)
   if (verbose)
       cat("xoffset= ", xoffset,"\n")
   x = x * xoffset
   return(cbind(x = x, y = y))
},static=TRUE,private=TRUE)

#' @export boxplot.JointUniquePairs
###########################################################################/**
# @RdocMethod boxplot
# 
# @title " Draw a basic boxplot based on a given JointUniquePairs object and external data"
#
# \description{
# Draw a basic boxplot (without title and x/y labs) based on a given set of matches 
# from the JointUniquePairs object and auxiliary data (response.data) from another data type. 
# Reused for example within the corrboxplot() and mixture.boxplot() by providing
# specific  response.data to this function and adding the title and x/y labels after the basic plot is complete
# }
# 
# @synopsis
#
# \arguments{
# \item{response.data}{The @numeric data vector on which the distribution for each particular match group 
# is computed on the fly during the plot.}
# \item{args}{Optional @list of ID Map names within the UniquePairsMatch object to be plotted
# allowing to plot a subset of match groups. Default is @NULL.}
# \item{showNone}{If not @NA, includes a plot of subset of values not included into 
# any other match group using showNone label. The default is @NA.}
# \item{multiline}{If @TRUE the compaund expression x axis labels plotted in multiple lines,
# i.e. 'A & B' is plotted as 'A &<line break> B'. Default is @FALSE} 
# \item{plot}{if @TRUE (default) then a boxplot is produced. 
# If not, the summaries which the boxplots are based on are returned.}
# \item{group.gap}{Extra gap between boxplot groups. Default is 0.3.}
# \item{cex}{Plot symbols font size. Default is 1.}
# \item{cex.main}{Main title font size. Default is 1.2.}
# \item{cex.lab}{X and Y titles font size. Default is 1.}
# \item{cex.axis}{X and Y axis labels font size. Default is 1.}
# \item{srt}{Match group label orientation (see par('srt')). The default is 0.}
# \item{adj}{@numeric in a range (-1..1) determening how close to the x-axis
# the group labels are, negative being the closest and positive the furthest. The default is 0.} 
# \item{par.zoom}{Graphics parameters zoom factor. Scales the graphical parameters like cex, lwd, mai etc.}
# \item{addPoints}{If @TRUE (default), the points representing the data density distribution
# of a particular compaund group are plotted along with a standard box.}
# \item{col.points}{The color of the data points plotted along with a particular box.}
# \item{allAgree}{The text plotted for a compaund group corresponding 
# to intersection of all match groups.}
# \item{...}{Additional graphical parameters}
# }
#
# \value{
# The data structure with following components:
# \item{boxdata}{Same as for boxplot in graphics package.}
# \item{response.grouped}{Input data grouped by matches.}
# }
#
# \examples{
# args=list(NetAffx_Q="Affy_Q",DAVID_F="D_F",EnVision_Q="EnQ");
# data<-examples$jointUniquePairs$boxplot(examples$corr$getData(),args,srt=30,allAgree="All");
# data$boxdata;
# names(data$response.grouped);
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setMethodS3("boxplot","JointUniquePairs",function(x,response.data,args=NULL,
	showNone=NA, multiline=FALSE, plot=TRUE, truncate.labels=TRUE,
	group.gap=0.3,cex=1,cex.main=1.2,cex.lab=1,cex.axis=1,srt=0, adj=0,par.zoom=1,
	addPoints=TRUE,col.points="lightblue", allAgree="All Agree",...) {


	pairsMatch<-as.data.frame(x);

	#substitute correlation values with external data if applicable
	if (nrow(pairsMatch)!=length(response.data)){
			warning("invalid external correlation data size");
			return(NULL);
	}


	#process args list
	default_args<-colnames(pairsMatch)[-c(1:2)];


	if (is.null(args)){
		args<-default_args;
	} else {
		if(is.list(args))
			args<-args[names(args) %in% default_args]
		else
			args<-args[args %in% default_args];
	}

	if (length(args)==0){
		warning("invalid db list");
		return(NULL);
	}

	if(is.list(args)){
		shortNames<-unlist(args);
		args<-names(args);
	}else{
		shortNames<-args;
	}

	#if(multiline)
	#	srt=0;


	expr<-paste("response.data","~",paste(args,collapse=" + "),"-1");


	digits<-length(args);
	sz=2^length(args);

	stats<-with(pairsMatch,{boxplot(as.formula(expr),plot=FALSE,...);});

	if(length(stats$names)!=sz){
		subsetting<-TRUE;
		pairsMatch<-rbind(pairsMatch,pairsMatch[1,])
		pairsMatch[nrow(pairsMatch),-c(1,2)]<-rep(FALSE,times=ncol(pairsMatch)-2);
		response.data<-c(response.data,0);
		stats<-with(pairsMatch,{boxplot(as.formula(expr),plot=FALSE,...);});
	}else{
		subsetting<-FALSE;
	}

	names<-NULL;
	exprNames<-NULL;
	levels<-NULL;
	cnts<-NULL;
	toplot<-NULL;


	responseGrouped<-list();
	for (i in 0:(sz-1)){
		whichArgs<-Misc$to.binary.logical(i,digits);
		
		exprName<-shortNames;
		exprName[!whichArgs]<-paste("!",exprName[!whichArgs],sep="");

		level<-sum(whichArgs);
		name<-paste(shortNames[whichArgs],collapse=paste(" &",split="\n"));
		if  (level==0){
			name<-showNone;
		} else if (level==digits && digits>1){
			name<-allAgree;
		}
		names<-c(names,name);
		exprNames<-c(exprNames,paste(exprName,collapse=" &\n"));
	
		levels<-c(levels,level);
		
		stats.name<-paste(as.character(whichArgs),collapse=".");
		is.plotted<-stats.name %in% stats$names;
		if(is.plotted){
			count<-stats$n[stats$names==stats.name];
		} else {
			count<-0;
		}
		toplot<-c(toplot,is.plotted);
		cnts<-c(cnts,count);
	}

	splitby <- lapply(args, function(arg)pairsMatch[[arg]])
	names(splitby) <- args;
	responseGrouped <- split(response.data, splitby);


	names<-names[order(levels)];
	exprNames<-exprNames[order(levels)];
	toplot<-toplot[order(levels)];
	cnts<-cnts[order(levels)];
	responseGrouped<-responseGrouped[order(levels)];
	levels<-levels[order(levels)];

	if(!truncate.labels){
		names[-1]<-exprNames[-1];
		if(!is.na(names[1]))
			names[1]<-exprNames[1];
	}
			
	names(responseGrouped)<-gsub("\n"," ",exprNames,fixed=TRUE);
	if(!multiline)
		names<-gsub("\n"," ",names,fixed=TRUE);

	if(!plot)
		return(list(response.grouped=responseGrouped[!is.na(names)],boxdata=stats));


	#generate plot positions
	nuLevels<-ifelse(is.na(showNone),length(unique(levels))-1,length(unique(levels)));
	totalGap<-group.gap*(nuLevels-1);
	nuGrid<-ifelse(is.na(showNone),length(names)-1,length(names));
	delt<-(sum(toplot)+1-totalGap)/(nuGrid+1);


	ats<-NULL;
	pos<-delt-group.gap/2;
	level<-levels[1];
	for (i in 1:length(names)) {
		if (is.na(names[i])){
			ats<-c(ats,NA);
		} else {
			if(level!=levels[i])
				pos<-pos+group.gap;
			ats<-c(ats,pos);
			pos<-pos+delt;
		}
		level<-levels[i];
	}

   	op2<-Display$zoom.pars(par.zoom);
	
	cntbox<-Display$textBoundingBox("0123456789",cex=cex.axis,srt=0,units="inches");
	max.txtbox<-Display$textBoundingBox(names,cex=cex.lab,srt=srt,units="inches");
	txtboxes<-Display$textBoundingBox(names,cex=cex.lab,srt=0,units="inches",get.biggest=FALSE);

	mai<-par("mai");
	mai[1]<-max(mai[1],cntbox$height+max.txtbox$height+0.25*par.zoom);
	op<-par(mai=mai);

	at<-ats[toplot];
	if(!is.na(showNone)&& subsetting){
		at[1]<-NA;
		cnts[1]<-0;
	}



	xlim=range(at,na.rm=TRUE)+c(-delt,delt)*0.75;
	stats<-with(pairsMatch,{
		boxplot(as.formula(expr),
			names=rep("",length(names[toplot])),
			at=at,cex.main=cex.main,cex.lab=cex.lab,cex.axis=cex.axis,cex=cex,xlim=xlim,...);
	})

	axis(1,at=at,labels=FALSE,lwd=par("lwd"),cex.axis=cex.axis,...);
	axis(2,labels=FALSE,lwd=par("lwd"),cex.axis=cex.axis,...);

	if(addPoints) {
		for(i in 1:length(responseGrouped)){
			group = responseGrouped[[i]]
			with(as.data.frame( 
			boxplotdataJitter(x,group, round=TRUE)), 
				points(x+at[i], y, col=col.points))		}
	
		#overlay points with boxes using transparent background
		bg<-par(bg="transparent");
		stats<-with(pairsMatch,{
			boxplot(as.formula(expr),add=TRUE,
				names=rep("",length(names[toplot])),
				at=at,cex.main=cex.main,cex.lab=cex.lab,cex.axis=cex.axis,cex=cex,xlim=xlim,...);
		})
		par(bg=bg);
	}

	# label the plot
	unitsPerInch<-(par("usr")[4]-par("usr")[3])/par("pin")[2];

	text(x=ats,y=par()$usr[3]-unitsPerInch*0.8*cntbox$height,
		 adj=c(0.5,1), labels=cnts,cex=cex.axis,srt=0,xpd=NA);

	angle<-srt/180*pi;
	dX<-sin(angle)*txtboxes$height/2;
	y<-par()$usr[3]-unitsPerInch*(sin(angle)*txtboxes$width/2+(2.25+adj)*cntbox$height);

	text(x=ats-dX,y=y,adj=c(0.5,1),labels=names[toplot],cex=cex.lab,srt=srt,xpd=NA);

	par(mai=op);
	par(op2);

	return(list(response.grouped=responseGrouped[!is.na(names)],boxdata=stats));

},createGeneric=FALSE)


###########################################################################/**
# @RdocMethod corr.plot
# 
# @title "Plot the density distributions for a set of correlation objects
# derived from JointUniquePairs and Corr objects"
#
# \description{
# Plot the density distributions for a set of correlation objects derived from
# JointUniquePairs and Corr objects with optional subsetting by a group of ID Maps.
# This is achived by first creating a correlation object from the JointUniquePairs
# and Corr objects with optional subsetting by a group of ID Maps and then calling
# the Corr.plot() on a resulting set of correlation objects.
# }
# 
# @synopsis
#
# \arguments{
# \item{corr}{Corr object.}
# \item{idMapNames}{If not @NULL, defines the subset of ID Maps from JointUniquePairs
# on which the full event group is to be formed. Default is @NULL.}
# \item{plot.Union}{If @TRUE (default), plots also the density of the correlation
# object corrsesponding to the union of a set of correlation objects.}
# \item{subsetting}{If @TRUE, subsets the  Corr on a group of ID Maps 
# or uses the original Corr otherwise. Default is @FALSE.}
# \item{lineColors}{The @vector of line colors (recycled if necessary) 
# for plotting the distributions of different Corr objects.
# If @NULL (default), the predefined set of colors is used.}
# \item{lineStyles}{The @vector of line styles (recycled if necessary) 
# for plotting the distributions of different Corr objects.
# If @NULL (default), the predefined set of line styles is used.}
# \item{lineWidths}{The @vector of line widths (recycled if necessary) 
# for plotting the distributions of different Corr objects. Default is 2.}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Additional graphical parameters}
# }
#
# \value{
# The list of Corr objects which data densities are plotted
# }
#
# \examples{
# #plot the correlation densities of a Corr object (corr.spearman) on a given DB subset
# corrSet<-examples$jointUniquePairs$corr.plot(examples$corr,
#             idMapNames=c("NetAffx_Q","DAVID_Q","EnVision_Q"),
#	       plot.Union=TRUE,subsetting=TRUE,verbose=TRUE);
# names(corrSet);
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setMethodS3("corr.plot","JointUniquePairs",function(this,corr,idMapNames=NULL,
	plot.Union=TRUE,subsetting=FALSE,lineColors=NULL,lineStyles=NULL,lineWidths=2,verbose=FALSE,...){


		if(plot.Union && !("union" %in% idMapNames))
			idMapNames<-c("union",idMapNames);
		corrSet<-getCorr(this,corr,groups=idMapNames,full.group=subsetting,verbose=verbose)

		if(!plot.Union && ("union" %in% idMapNames))
			corrSet<-Corr(corrSet[-c("union")]);

		Corr$plot(corrSet,lineColors=lineColors,lineStyles=lineStyles,lineWidths=lineWidths,verbose=verbose,...);
		invisible(corrSet);
})




###########################################################################/**
# @RdocMethod mixture.plot
# 
# @title "Plot the correlation densities of the empirical fit, mixture fit and each
# of the mixture components for a mixture object derived from JointUniquePairsand Corr objects"
#
# \description{
# Plot the correlation densities of the empirical fit, mixture fit and each of the mixture components
# for a mixture object derived from UniquePairsMatch  and Corr objects with optional subsetting 
# by a group of ID Maps. This is achived by first creating a mixture model from the JointUniquePairs
# and Corr objects with optional subsetting by a group of ID Maps encapsulated within the and then calling the MixtureObject.plot()
# on a resulting Mixture object.
# }
# 
# @synopsis
#
# \arguments{
# \item{corr}{Corr object.}
# \item{idMapNames}{If not @NULL, defines the subset of ID Maps from JointUniquePairs
# on which the full event group is to be formed. Default is @NULL.}
# \item{subsetting}{If @TRUE, uses Corr subset to compute the mixture model or uses
# the original Corr otherwise. Default is @FALSE.}
# \item{G}{The number of mixture model components. If a @numeric vector,
# the optimal mixture model is computed. Default is c(1:5).}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Additional graphical parameters}
# }
#
# \value{
# The Mixture object plotted
# }
#
# \examples{
# #plot the results of mixture fit for a given DB subset and a Corr object (corr.spearman)
# mixtureSet<-examples$jointUniquePairs$mixture.plot(examples$corr,
#                idMapNames=c("NetAffx_Q","DAVID_Q","EnVision_Q"),
#	          subsetting=TRUE,G=c(1:5),verbose=TRUE);
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("mixture.plot","JointUniquePairs",function(this,corr,idMapNames=NULL,
	subsetting=FALSE,G=c(1:5),verbose=FALSE,...){

	mixture<-getMixture(this,corr,groups=idMapNames,
			full.group=subsetting,G=G,verbose=verbose);
	plot(mixture,...);
	invisible(mixture);
})



###########################################################################/**
# @RdocMethod corr.boxplot
# 
# @title "Boxplot of correlations by match group"
#
# \description{
# Creates a boxplot of correlations data from the Corr object 
# by a set of match groups from the UniquePairsMatch object, 
# utilizing the JointUniquePairs$boxplot function
# }
# 
# @synopsis
#
# \arguments{
# \item{corr}{Corr object which correlation values are used for boxplot.}
# \item{idMapNames}{Defines a subset of match group names to be plotted.
# If $NULL (default), all group names within the pairsMatch are used.}
# \item{subsetting}{If @TRUE, interprets Corr subset as a full group
# or uses the original Corr as a full group otherwise. Default is @FALSE.}
# \item{show.None}{If @TRUE, includes a plot of subset of values not included
# into any other match group  with 'None' label. Default is @FALSE.}
# \item{group.gap}{The gap between match groups. Default is 0.2.}
# \item{cex.main}{Font size for plot main title. Default is 1.2.}
# \item{cex.lab}{Font size for X and Y axis titles. Default is 1.}
# \item{srt}{Match group label orientation (see par('srt')). Default is 0.}
# \item{adj}{@numeric in a range (0..1) determening how close to the x-axis
# the group labels are, 1 being the closest and 0 is farthest. The default is 0.}
# \item{par.zoom}{Graphics parameters zoom factor. Scales the graphical
# parameters like cex, lwd, mai etc. Default is 1.}
# \item{main}{The main title. Default is 
# 'Correlations (type) by match group', where type is derived from corr object
# ('spearman' or 'pearson').}
# \item{plot}{if @TRUE (default) then a boxplot is produced. 
# If not, the summaries which the boxplots are based on are returned.}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Additional graphical parameters}
# }
#
# \value{
# Same as for boxplot in graphics package
# }
#
# \examples{
# #plot correlation probability distributions (boxplots) by match group
# mapNames=list(NetAffx_Q="Affy_Q",DAVID_F="D_F",EnVision_Q="EnQ");
# data = examples$jointUniquePairs$corr.boxplot(examples$corr,
#	        idMapNames=mapNames,subsetting=TRUE,
#              multiline=TRUE, srt=30, cex.lab=0.8, col.points="green" , main="");
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("corr.boxplot","JointUniquePairs",function(this,corr,idMapNames=NULL,
	show.None=FALSE,subsetting=FALSE,group.gap=0.2,
	cex.main=1.2,cex.lab=1,srt=0,adj=0,par.zoom=1,
	main=paste("Correlations (", colnames(corr)[3], ") by match group", sep = ""),
	plot=TRUE,	verbose=FALSE,...){

	if(subsetting){
		match_subset<-subsetGroups(this,groups=idMapNames,verbose=verbose);
		corr_subset<-subsetCorr(match_subset,corr,verbose=verbose);
	} else {
		match_subset<-this;
		corr_subset<-corr;
	}


	res<-boxplot(match_subset,corr_subset[,3],args=idMapNames,
		showNone=ifelse(show.None,"None",NA),group.gap=group.gap,
		main=main,plot=plot, ylab="corellation",
		cex.main=cex.main,cex.lab=cex.lab,srt=srt,adj=adj,par.zoom=par.zoom,...);

	if(plot)
		abline(h=0, lty=2, lwd=2*par.zoom, col="blue")
	invisible(res);
})




###########################################################################/**
# @RdocMethod mixture.boxplot
# 
# @title "Boxplot of a mixture model component by match group"
#
# \description{
# Creates a boxplot of mixture component data from the Mixture object 
# by a set of match groups from the JointUniquePairs object, 
# utilizing the JointUniquePairs$boxplot function
# }
# 
# @synopsis
#
# \arguments{
# \item{corr}{Corr object from which the Mixture object is constructed on the fly
# to use one of it'c components for the boxplot corresponding to the best fit is plotted.} 
# \item{idMapNames}{Defines a subset of match group names to be plotted.
# If @NULL (default), all group names within the pairsMatch are used.}
# \item{subsetting}{If @TRUE, interprets Corr subset as a full group
# or uses the original Corr as a full group otherwise. Default is @FALSE.}
# \item{show.None}{If not @NA, includes a plot of subset of values not included
# into any other match group  with 'None' label. Default is @NA.}
# \item{group.gap}{The gap between match groups. Default is 0.2.}
# \item{cex.main}{Font size for plot main title. Default is 1.2.}
# \item{cex.lab}{Font size for X and Y axis titles. Default is 1.}
# \item{srt}{Match group label orientation (see par('srt')). The default is 0.}
# \item{adj}{@numeric in a range (0..1) determening how close to the x-axis the group
# labels are, 1 being the closest and 0 is farthest. The default is 0.}
# \item{G}{Number of components in mixture model. If G is a @vector, the optimal
# number of component is determined. G is a vector (1:5) by default.}
# \item{plot.G}{The mixture component to be used for plotting.
# If @NULL (default), the highest-order component of the mixture model is used.}
# \item{par.zoom}{Graphics parameters zoom factor. Scales the graphical
#  parameters like cex, lwd, mai etc. Default is 1.}
# \item{main}{The main title. Default is 
# 'Component posterior probability by match group'}
# \item{plot}{if @TRUE (default) then a boxplot is produced. 
# If not, the summaries which the boxplots are based on are returned.}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Additional graphical parameters}
# }
#
# \value{
# Same as for boxplot in graphics package
# }
#
# \examples{
# #plot posterior second component probability distributions by match group
# mapNames=list(NetAffx_Q="Affy_Q",DAVID_F="D_F",EnVision_Q="EnQ");
# examples$jointUniquePairs$mixture.boxplot(examples$corr,
#	idMapNames=mapNames,
#	multiline=TRUE,subsetting=TRUE, plot.G=2,
#	srt=35, col.points="red",verbose=TRUE);
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setMethodS3("mixture.boxplot","JointUniquePairs",function(this,corr,idMapNames=NULL,
	show.None=FALSE,subsetting=FALSE,group.gap=0.2,G=c(1:5),plot.G=NULL,
	cex.main=1.2,cex.lab=1,srt=0,adj=0,par.zoom=1,
	main="Component posterior probability by match group",plot=TRUE,verbose=FALSE,...){

	if(subsetting){
		match_subset<-subsetGroups(this,groups=idMapNames,verbose=verbose);
		corr_subset<-subsetCorr(match_subset,corr,verbose=verbose);
	} else {
		match_subset<-this;
		corr_subset<-corr;
	}

	mixture<-Mixture(corr_subset,G=G,verbose=verbose);

	if(is.null(plot.G)){
		plot.G<-mixture$.model$clust$G;
	} else {
		plot.G<-max(1,min(plot.G,ncol(mixture$.model$clust$z)));
	}

	res<-boxplot(match_subset,mixture$.model$clust$z[,plot.G],args=idMapNames,
			showNone=ifelse(show.None,"None",NA),group.gap=group.gap,
			main=main,plot=plot,ylab=paste("Pr(component #",plot.G,")",sep=""),
			cex.main=cex.main,cex.lab=cex.lab,srt=srt,adj=adj,par.zoom=par.zoom,...);
	if(plot)
		abline(h=0.5, lty=2, lwd=2*par.zoom, col="blue");
	invisible(res);
})

