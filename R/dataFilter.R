###########################################################################/**
# @RdocClass DataFilter
# \encoding{latin1}
#
# @title "The DataFilter class"
#
# \description{
# @classhierarchy
# 
# Serves as a wrapper for data data filtering functions 
# define as static methods of the DataFilter class.
# }
# 
# @synopsis
#
# \arguments{
# \item{...}{Not used.}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setConstructorS3("DataFilter",function(){
	extend(Object(),"DataFilter");
})



###########################################################################/**
# @RdocMethod minCountConstraint
# 
# @title "Perform minimum count based thresholding of an input vector"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{x}{@numeric input @vector.}
# \item{filtParams}{@vector of constraint parameters. If the fraction of input elements
# which value is greater or equal filtParams[1] is less than filtParam[2],
# the whole vector is replaced be NA vector of the same length.}
# \item{...}{Not used}
# }
#
# \value{
# @numeric @vector or @vector of @NA 's depending on thresholding criteria.
# }
#
# \examples{
# #set to NA protein count rows which contain less than 50 percent of counts >=2
# fltExperimentSet<-DataFilter$do.apply(examples$msmsExperimentSet,
#   byRows=TRUE,filterFun=DataFilter$minCountConstraint,filtParams=c(2,0.5),verbose=TRUE);
#
# #print the number of rows set to NA 
# sum(is.na(rowSums(fltExperimentSet[,-1])))
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("minCountConstraint","DataFilter",function(static,x,filtParams,...){
	if (length(filtParams)==2){
		if ((sum(x>=filtParams[1])/length(x))<filtParams[2])
			x<-rep(NA,length=length(x));
			
	}else{
		stop("incorrect number of filter parameters");
	}
	return (x);
},static=TRUE)



###########################################################################/**
# @RdocMethod minCountGroupConstraint
# 
# @title "Perform minimum count based thresholding of an input vector subdivided into groups"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{X}{@numeric input @vector.}
# \item{filtParams}{@vector of constraint parameters. The filtParams[3] is a list of integer vectors,
# each containing the indexes of an input X vector data and thus defining the groups within the input vector.
# If the fraction of input elements for a given group (defined by filtParam[3]) which value is greater 
# or equal filtParams[1] is less than filtParam[2], and if all groups satisfy this condition, then the
# input vector is replaced by NAs, otherwise it is kept intact.}
# \item{...}{Not used}
# }
#
# \value{
# @numeric @vector or vector of @NA 's depending on passing the constraints criteria.
# }
#
# \examples{
# #derive sample groups from outcomes
# outcomes<-unique(examples$outcomeMap[,2]);
# groups<-list();
# for(outcome in outcomes)
#	groups[[outcome]]<-which(examples$outcomeMap[,2]==outcome);
#
# #perform filtering by groups
# fltExperimentSet<-DataFilter$do.apply(examples$msmsExperimentSet,
#   byRows=TRUE,filterFun=DataFilter$minCountGroupConstraint,filtParams=list(1,0.3,groups),verbose=TRUE);
#
# #print the number of rows set to NA 
# sum(is.na(rowSums(fltExperimentSet[,-1])))
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("minCountGroupConstraint","DataFilter",function(static,X,filtParams,...){
	if (length(filtParams)==3){
		NAs<-FALSE;
		for (group in filtParams[3][[1]]){
			x<-as.numeric(X[group]);
			NAs<-NAs || ((sum(x>=filtParams[1])/length(x))<filtParams[2])		
		}
		if(NAs){
			X<-rep(NA,length=length(X));
		}				
	}else{
		stop("incorrect number of filter parameters");
	}
	return (X);
},static=TRUE)


###########################################################################/**
# @RdocMethod minAvgCountConstraint
# 
# @title "Perform mean based thresholding of an input vector"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{x}{@numeric input @vector.}
# \item{filtParams}{@vector of constraint parameters. If the mean of an input vector
# is less than filterParams[1] the whole vector is replaced by @NA 's vector of the same length.}
# \item{...}{Not used}
# }
#
# \value{
# @numeric @vector or vector of @NA 's depending on thresholding criteria.
# }
#
# \examples{
# #set to NA protein count rows which contain less than 50 percent of counts >=2
# fltExperimentSet<-DataFilter$do.apply(examples$msmsExperimentSet,
#   byRows=TRUE,filterFun=DataFilter$minAvgCountConstraint,filtParams=0.5,verbose=TRUE);
#
# #print the number of rows set to NA 
# sum(is.na(rowSums(fltExperimentSet[,-1])))
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("minAvgCountConstraint","DataFilter",function(static,x,filtParams,...){
	if (length(filtParams)>0){
		if ((sum(x,na.rm=TRUE)/length(x))<filtParams[1]) {
			x<-rep(NA,length=length(x));
		}	
	}
	return (x);
},static=TRUE)



###########################################################################/**
# @RdocMethod log10
# 
# @title "Compute log10 of a numerical vector combined with thresholding on minimum value"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{x}{@numeric input @vector.}
# \item{filtParams}{@vector of constraint parameters. If a particular
# output element is less or equal than filtParams[1] it is assigned the filtParams[2] value.}
# \item{...}{Not used}
# }
#
# \value{
# clipped log10 of an input @numeric @vector.
# }
#
# \examples{
# #compute log10 transform of mrna experiment data replacing (clipping) 
# #the output values with log10(0.5)for input values < 0.5
# fltExperimentSet<-DataFilter$do.apply(examples$mrnaExperimentSet,
#         byRows=TRUE,filterFun=DataFilter$log10,filtParams=c(0.5,log10(0.5)),verbose=TRUE);
#
# #print the number of elements clipped 
# sum(fltExperimentSet[,-1]==log10(0.5))-sum(examples$mrnaExperimentSet[,-1]<=0.5);
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("log10","DataFilter",function(static,x,filtParams,...){
	x<-log10(x);
	if (!is.null(filtParams))
		x[x<=filtParams[1]]<-filtParams[2];
	return(x);
},static=TRUE)



###########################################################################/**
# @RdocMethod do.apply
# 
# @title "Filter experiment using constraints"
#
# \description{
# @get "title".
# The function applyies the constraint filter to the experiment @data.frame, by rows or columns,
# replacing the content of each row or column with constraint filter output. In most cases, if the
# given row or column passes the constraint check, it is left intact and replaced by @NA 's otherwise.
# One of the purposes of this function is to perform the quality control of experiment data and filter out
# series which do not pass the QC by applying the removeNASeries method as the last step.
# }
# 
# @synopsis
#
# \arguments{
# \item{experimentSet}{@data.frame containing probeID (first column/row)
# and data arranged by series (the rest of columns/rows)}
# \item{byRows}{@logical indicating if series arranged by rows (@TRUE)
# or by columns (@FALSE). Default is @TRUE.}
# \item{filterFun}{The filtering function applyied to the experiment set,
# typically the constraint filter.}
# \item{filtParams}{Filtering function parameters.}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{
# The @data.frame with data filtered by filterFun.
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("do.apply","DataFilter",function(static,experimentSet,byRows=TRUE,
			filterFun=NULL,filtParams=NULL,verbose=FALSE,...){
	if (verbose)
		cat("filtering experiment set...\n");
	
	if (is.null(filterFun))
		return(experimentSet);
 
	filtSet<-data.matrix(experimentSet[,-1]);
	
	if(byRows) {
		for (i in 1:dim(filtSet)[1])
			filtSet[i,]<-filterFun(filtSet[i,],filtParams);
	}else{
		for (i in 1:dim(filtSet)[2])
			filtSet[,i]<-filterFun(experimentSet[,i],filtParams);
		
	} 
	filtSet<-as.data.frame(filtSet,stringsAsFactors=FALSE);
	filtSet<-cbind(rownames(filtSet),filtSet,stringsAsFactors=FALSE);
	colnames(filtSet)[1]<-colnames(experimentSet)[1];
	return(filtSet);
},static=TRUE)


###########################################################################/**
# @RdocMethod removeNASeries
# 
# @title "Remove NA series from the experiment set"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{experimentSet}{@data.frame containing probeID (first column or row) 
# and data arranged by series (the rest of columns or rows)}
# \item{byRows}{@logical indicating if series arranged by rows (@TRUE)
# or by columns (@FALSE). Default is @TRUE.}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{
# The experiment @data.frame with rows or columns containing all @NA 's removed.
# }
#
# \examples{
# #set to NA protein count rows which average count is less than 50 percent
# fltExperimentSet<-DataFilter$do.apply(examples$msmsExperimentSet,
#   byRows=TRUE,filterFun=DataFilter$minAvgCountConstraint,filtParams=0.5,verbose=TRUE);
#
# #remove NA series
# fltExperimentSet<-DataFilter$removeNASeries(fltExperimentSet,byRows=TRUE,verbose=TRUE);
# dim(fltExperimentSet);
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("removeNASeries","DataFilter",function(static,experimentSet,byRows=TRUE,verbose=FALSE,...){
	if (verbose)
		cat("removing NA series...\n");
	
	filtSet<-data.matrix(experimentSet[,-1]);
	
	if (byRows){
		filtSet<-filtSet[rowSums(!is.na(filtSet))>0,];
	}else{
		filtSet<-filtSet[,colSums(!is.na(filtSet))>0];
	}
	filtSet<-cbind(rownames(filtSet),filtSet);
	colnames(filtSet)[1]<-colnames(experimentSet)[1];
	return(as.data.frame(filtSet,stringsAsFactors=FALSE));
},static=TRUE)



###########################################################################/**
# @RdocMethod fisherTransform
# 
# @title "Compute the Fisher transform"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{x}{Input @numeric @vector.}
# \item{...}{Not used}
# }
#
# \value{
# Result of the Fisher transform.
# }
#
# \examples{\dontrun{
# z<-Filter$fisherTransform(x);
# }}
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("fisherTransform","DataFilter",function(static,x,...) {
	return(log((1+x)/(1-x))/2);
},static=TRUE)



###########################################################################/**
# @RdocMethod fisherTransformInverse
# 
# @title "Compute the Fisher inversed transform"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{z}{Input @numeric @vector.}
# \item{...}{Not used}
# }
#
# \value{
# Result of the Fisher inversed transform.
# }
#
# \examples{\dontrun{
# z<-Filter$fisherTransform.inverse(x);
# }}
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("fisherTransformInverse","DataFilter",function(static,z,...) {
	return((exp(2*z) - 1)/(1 + exp(2*z)));
},static=TRUE)



###########################################################################/**
# @RdocMethod fisherTransformJacobean
# 
# @title "Compute the Fisher transform Jacobean"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{x}{Input @numeric @vector.}
# \item{...}{Not used}
# }
#
# \value{
# Fisher transform jacobean.
# }
#
# \examples{\dontrun{
# J<-DataFilter$fisherTransform.Jacobean(x);
# }}
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("fisherTransformJacobean","DataFilter",function(static,x,...) {
	return(((1+x)^(-1) + (1-x)^(-1))/2);
},static=TRUE)



