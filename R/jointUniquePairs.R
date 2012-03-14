
###########################################################################/**
# @RdocClass JointUniquePairs
# \encoding{latin1}
#
# @title "The JointUniquePairs class"
#
# \description{
#  @classhierarchy
#
# UniquePairsMatch object encapsultates a data frame the
# first two columns of which contain the unique pairs corresponding to the merge (union)
# of all ID Maps in consideration while the rest of columns contains the match (logical value) 
# between the merged unique pairs set and a unique pairs set specific to the particular 
# ID Map ('d8', 'enV', 'netAffx' etc), one column per Id Map. 
# Used in combination with correlation related data objects (CorrData, Corr, Mixture etc.)
# to aid in on-fly processing related to some classification by a particular match group
# The UniquePairsMatch constructor creates an object from the UniquePairs and an ID Map list
# computing the match (inclusions) for each particular ID Map.
# }
# 
# @synopsis
#
# \arguments{
# \item{uniquePairs}{ UniquePairs object on which a UniquePairsMatch is created
# or a @data.frame complying with the UniquePairs class internal data frame format.
# In case the UniquePairs object is used as a first argument, it's typically obtained
# from the JointIdMap object by invoking JointIdMap$getUnionIdMap())}
# \item{idMapList}{ the list of ID Maps on which the match is performed during the UniquePairsMatch object creation
# The idMapList typically obtained through the call to the JointIdMap.getIdMapList() of the same JointIdMap object
# as for the first argument to ensure that both arguments are properly aligned.} 
# \item{name}{A @character string representing the name of the given IdMap object. Default is ''}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used.}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \examples{
# #create JointIdMap
# jointIdMap<-JointIdMap(examples$identDfList);
#
# #creaate unique pairs from the union of all IdMaps within JointIdMap
# pairs<-as.UniquePairs(jointIdMap$getUnionIdMap(verbose=TRUE),verbose=TRUE);
#
# #create JointUniquePairs object
# jointPairs<-JointUniquePairs(pairs,jointIdMap$getIdMapList(),verbose=TRUE);
# jointPairs[1:10,];
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setConstructorS3("JointUniquePairs",function(uniquePairs=NULL,idMapList=NULL,name="",verbose=FALSE,...){
	if(is.null(uniquePairs))
		return(extend(IdMapBase(name=name),"JointUniquePairs"));

	if(inherits(uniquePairs,"data.frame"))
		return(extend(IdMapBase(uniquePairs,name=name,secondaryKey=colnames(uniquePairs)[2]),"JointUniquePairs"));


	if (verbose)
		cat("creating joint unique pairs object...\n");
	
	#transform the ID map collection into unique pairs collection 
	uniquePairsList<-UniquePairs$create(idMapList,verbose=verbose);

	#match the unique pairs from jointIdMap with unique pairs collection producing
	#the list of logical inclusion (%in%) vectors
	matchList<-getMatch(uniquePairs,uniquePairsList,verbose=verbose);

	#creating the joint unique pairs object

	DF<-cbind(as.data.frame(uniquePairs),as.data.frame(matchList));
	extend(IdMapBase(DF,name),"JointUniquePairs",
		.secondaryKey=secondaryKey(uniquePairs)
	);

})


###########################################################################/**
# @RdocMethod getMapNames
# 
# @title "Get the names of UniquePairs objects encapsulated within the given JointUniquePairs object"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{\item{...}{Not used}}
#
# \value{A @character vector of IdMap names}
#
# \examples{
# examples$jointUniquePairs$getMapNames();
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getMapNames","JointUniquePairs",function(this,...){
	return(colnames(this)[-c(1,2)]);
})


###########################################################################/**
# @RdocMethod subsetGroups
# 
# @title "Get a JointUniquePairs subset"
#
# \description{
# Creates a new UniquePairsMatch object which represents a subset of UniquePairs in the 
# original object. Only the rows which have at least a single match are kept from the original object,
# so the resulting object events form the unity i.e. full event group.
# }
# 
# @synopsis
#
# \arguments{
# \item{groups}{The subset of groups (DBs) incapsulated within
# the given JointUniquePairs object to be subsetted on.
# If @NULL (default) then all UniquePairs are used.}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{
# JointUniquePairs object representing a full group (unity) for a subset of UniquePairs objects.
# }
#
# \examples{
# jointIdMap<-JointIdMap(examples$identDfList);
# pairs<-as.UniquePairs(jointIdMap$getUnionIdMap());
# jointPairs<-JointUniquePairs(pairs,jointIdMap$getIdMapList());
# jointPairs$getMapNames();
#
# jointPairsSubset<-jointPairs$subsetGroups(c("NetAffx_Q","DAVID_Q","EnSembl_F"));
# jointPairsSubset$getMapNames();
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("subsetGroups","JointUniquePairs",function(x,groups=NULL,verbose=FALSE,...){
	if (is.null(groups)){
		groups<-getMapNames(x);
	}else if (is.list(groups)){
		groups<-names(groups);
	}

	if (verbose && !is.null(groups))
		cat("Subsetting JointUniquePairs object by <",groups,">\n");

	matchSubset<-as.data.frame(x$.df[,groups]);

	inclusion<-(rowSums(matchSubset)>0)
	DF<-cbind(x$.df[inclusion,c(1,2)],matchSubset[inclusion,]);
	colnames(DF)[-c(1,2)]<-groups;

	res<-JointUniquePairs(DF,secondaryKey=secondaryKey(x));
	return(res);
})


###########################################################################/**
# @RdocMethod subsetData
# 
# @title "Subset data on a UniquePairsMatch object"
#
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{data}{@numeric vector to subset. Should be of the same length as ncol(this).}
# \item{expr}{@expression or a @character string representing an expression
# according to which the indicator variable columns of match object will be combined
# to form the logical vector of data vector elements inclusion.}
# \item{...}{Not used}
# }
#
# \value{subset of the original data vector.}
#
# \examples{
# #perform Wilcoxon test for EnVision/Affy corellation data
# data1<-examples$jointUniquePairs$subsetData(examples$corr$getData(),
#		"EnVision_Q & !NetAffx_Q");
# data2<-examples$jointUniquePairs$subsetData(examples$corr$getData(),
#           "!EnVision_Q & NetAffx_Q");
# wilcox<-wilcox.test(data1,data2);
# wilcox[names(wilcox)];
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setMethodS3("subsetData","JointUniquePairs",function(this,data,expr,...){	
	expr<-Misc$to.index.expr(this$.df,expr);
	matches<-eval(expr);
	return(data[matches]);
})



###########################################################################/**
# @RdocMethod getUniquePairs
# 
# @title "Extract the unity UniquePairs object from a given JointUniquePairs object"
#
# \description{
# Extract the UniquePairs object from the JointUniquePairs object
# corresponding to a unity (full group).
# }
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
# uniquePairs<-examples$jointUniquePairs$getUniquePairs();
# uniquePairs[1:10,];
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getUniquePairs","JointUniquePairs",function(this,...){
	return(UniquePairs(this[,c(1,2)]));
})



###########################################################################/**
# @RdocMethod getMatchInfo
# 
# @title "Get match table(s) for a given set of primary IDs"
#
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{IDs}{A @character vector of primary IDs.}
# \item{idMapNames}{A @character vector of Id Map names encapsulated within the JointUniquePairs
# object or @NULL (default),in which case all Id Maps are used.}
# \item{...}{Not used}
# }
#
# \value{
# A @list of data.frames representing a match table(s) for 
# a set of given ID Maps and each particular primary ID.
# }
#
# \examples{
# IDs<-examples$identDfList[["DAVID_Q"]][,1];
# matches<-getMatchInfo(examples$jointUniquePairs,IDs,c("NetAffx_Q","DAVID_Q","EnSembl_F"));
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getMatchInfo","JointUniquePairs",function(this,IDs,idMapNames=NULL,...){
	primaryIDs<-primaryIDs(this);
	IDs<-IDs[IDs %in% primaryIDs];

	if(is.null(idMapNames))
		idMapNames=getMapNames(this);
	
	res<-list();
	for(ID in IDs){
		inds<-which(primaryIDs==ID);
		subset<-this[inds,idMapNames];
		rownames(subset)<-this[inds,2];
		subset<-subset[rowSums(subset)>0,];
		
		match<-array("",dim=dim(subset),dimnames=list(rownames(subset),colnames(subset)));
		for(name in idMapNames){
			match.inds<-which(subset[,name]);
			match[match.inds,name]<-rownames(subset)[match.inds];
		}
		res[[ID]]<-as.data.frame(t(match),stringsAsFactors=FALSE);
	}

	return(res);
})



###########################################################################/**
# @RdocMethod do.glm
# 
# @title "Compute linear regression for the given set of ID Maps"
#
# \description{
# Compute linear regression for the given set of ID Maps using the UniquePairsMatch object 
# and auxiliary data (response.data) from another data object.
# }
# 
# @synopsis
#
# \arguments{
# \item{response.data}{The data on which the regression to be fitted.}
# \item{weights}{An optional vector of 'prior weights' to be used in the fitting process. 
# Should be @NULL or a @numeric vector.}
# \item{idMapNames}{optional list of ID Map names within the pairsMatch object
# on which the fit to be performed. If NULL (default), all ID Maps within the pairsMatch object will be used.}
# \item{...}{Not used}
# }
#
# \value{
# The regression model fitting results (an object of class 'glm')
# }
#
# \examples{
# # perform regression on correlations
# fit<-examples$jointUniquePairs$do.glm(examples$corr$getData(),
#		idMapNames=c("DAVID_Q","EnVision_Q","NetAffx_Q"));
# coefficients(summary(fit));
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setMethodS3("do.glm","JointUniquePairs",function(this,response.data,weights=NULL,idMapNames=NULL,...) {

	pairsMatch<-as.data.frame(this);

	#substitute correlation values with external data if applicable
	if (nrow(pairsMatch)!=length(response.data)){
			warning("invalid external correlation data size");
			return(NULL);
	}

	#process args list
	default_args<-colnames(pairsMatch)[-c(1:2)];

	if (is.null(idMapNames)){
		idMapNames<-default_args;
	} else {
		if(is.list(idMapNames))
			idMapNames<-idMapNames[names(idMapNames) %in% default_args]
		else
			idMapNames<-idMapNames[idMapNames%in% default_args];
	}

	if (length(idMapNames)==0){
		warning("invalid db list");
		return(NULL);
	}

	if(is.list(args)){
		shortNames<-unlist(idMapNames);
		idMapNames<-names(idMapNames);
	}else{
		shortNames<-idMapNames;
	}

	expr<-paste("response.data","~",paste(idMapNames,collapse=" + "));
	glm_out<-with(pairsMatch,{glm(as.formula(expr),weights=weights);});
	return(glm_out);
})


