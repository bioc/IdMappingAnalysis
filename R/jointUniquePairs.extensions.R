
###########################################################################/**
# @set "class=JointUniquePairs"
# @RdocMethod subsetCorr
# 
# @title "Subset the Corr object"
#
# \description{
# Create the Corr object using the pait match information from UniquePairsMatch object
# in such a way that the new Corr object contains the data corresponding to the unity (full event group)
# in a JointUniquePairs subset.
# }
# 
# @synopsis
#
# \arguments{
# @param match UniquePairsMatch object
# @param corr Corr object
# @param groups names of ID Maps within the UniquePairsMatch object which should form a full event group.
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{
# The subsetted Corr object
# }
#
# \examples{
# corr.subset<-examples$jointUniquePairs$subsetCorr(examples$corr,
#			groups=c("NetAffx_Q", "DAVID_Q", "EnVision_Q"),verbose=TRUE);
# corr.subset[1:10,];
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setMethodS3("subsetCorr","JointUniquePairs",function(this,corr,groups=NULL,verbose=FALSE,...){
	if (is.null(groups)){
		groups<-getMapNames(this);
	}

	jointPairs_subset<-subsetGroups(this,groups,verbose);
	whichPairs<-getMatch(getUniquePairs(corr),getUniquePairs(jointPairs_subset));
	corr_subset<-Corr(corr[whichPairs,]);
	return(corr_subset);
})



###########################################################################/**
# @RdocMethod getCorr
# 
# @title "Extract a set of correlation objects from given JointUniquePairs object
# and corresponding Corr object"
#
# \description{
# Generate a list of correlation objects based on a given correlation object 
# treated as a 'joint' object(i.e. produced through the pipeline
# JointIdMap->UniquePairs->Corr and a set of matches for a set of DBs
# stored in a JointUniquePairs object.
# }
# 
# @synopsis
#
# \arguments{
# \item{corr}{Corr object from which the Corr object(s) retrieved
# for each particular DB based on which the patricular Corr object is retrieved.}
# \item{groups}{Optional @character vector of DB names encapsulated within the JointUniquePairs object defining the set of 
# match groups to be used for retrieving the corresponding Corr objects.
# If @NULL (default), all match groups within the given UniquePairsObject are used.}
# \item{full.group}{ Determines if the resulting Corr object list should represent a full group. Default is @FALSE.}
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{
# Corr object or a @list ofCorr  objects with names corresponding to the names of
# particular match groups contained within the given JointUniquePairs object
# }
#
# \examples{
# # Create and plot the set of corrs for a given DB subset treating subset as a full group 
# # and plot the correlation densities including union
# corrSet<- examples$jointUniquePairs$getCorr(examples$corr, 
#	groups=c("union", "NetAffx_Q", "DAVID_Q", "EnVision_Q"),
#     full.group=TRUE, verbose=TRUE);
# Corr$plot(corrSet, title="");
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################
 
setMethodS3("getCorr","JointUniquePairs",function(this,corr,groups=NULL,full.group=FALSE,verbose=FALSE,...){

	if(!equals(getUniquePairs(this),getUniquePairs(corr))){
		warning("Corr.getCorr: unique pairs mismatch");
		return(NULL);
	}

	default_groups<-c("union",getMapNames(this));

	if (verbose)
		cat("creating matching correlations ...\n");

	if(is.null(groups)){
		groups<-default_groups;
	}else{
		groups<-groups[groups %in% default_groups];
	}

	if (is.null(groups)){
		warning("invalid db list");
		return(NULL);
	}

	if (full.group)	{
		match_subset<-subsetGroups(this,groups[!(groups %in% "union")],verbose);
		corr_subset<-subsetCorr(match_subset,corr,verbose=verbose);
	}else{
		match_subset<-this;
		corr_subset<-corr;
	}

	res<-list();	
	for(arg in groups) {
		if (verbose)
			cat(arg,"\n");
		if(arg=="union"){
			res[[arg]]<-corr_subset;
		} else {
			res[[arg]]<-Corr(corr_subset[match_subset[,arg],]);
		}
	}
	return(res);
})



###########################################################################/**
# @RdocMethod getCorrDataFrame
# 
# @title "Merge JointUniquePairs and Corr objects into a single data frame"
#
# \description{
# This is a convinience function allowing to derive a data frame from the JointUniquePairs and Corr object.
# The structure of the resulting data frame is similar to the one of JointUniquePairs object, but @logical values
# indicating the presence of the given match pair for a particular ID Map are replaced whith correlation values 
# from Corr object if the @logical value is @TRUE or by @NA 's otherwise. Note, that the Corr object should
# correspond to the union of all ID Maps in consideration, i.e. should represent a full group.
# }
# 
# @synopsis
#
# \arguments{
# \item{corr}{Corr object from which the Corr object(s) retrieved
# for each particular DB based on which the patricular Corr object is retrieved.}
# \item{groups}{Optional @list of DB names from the JointUniquePairs object defining the set of 
# match groups to be used for retrieving the corresponding Corr objects.
# If @NULL, all match groups within the JointUniquePairs object are used.}
# \item{full.group}{Determines if the resulting Corr object @list should represent a full group. Default is @FALSE.}
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{
# A @data.frame similar to the one encapsultaed wwithin the JointUniquePairs object, but the @logical values
# indicating the presence of the given match pair for a particular ID Map are replaced by correlation values 
# from Corr object if the @logical value is @TRUE or by @NA 's otherwise. 
# }
#
#
# \examples{
# corrDF<- examples$jointUniquePairs$getCorrDataFrame(examples$corr, 
#	groups=c("NetAffx_Q", "DAVID_Q", "EnVision_Q"),
#     full.group=FALSE, verbose=TRUE);
# corrDF[1:10,];
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getCorrDataFrame","JointUniquePairs",function(this,corr,groups=NULL,full.group=FALSE,verbose=FALSE,...){
	if(full.group){
		match_subset<-subsetGroups(this,groups=groups,verbose=verbose);
		corr_subset<-subsetCorr(match_subset,corr,verbose=verbose);
	} else {
		match_subset<-this;
		corr_subset<-corr;
	}
	corrList<-getCorr(match_subset,corr_subset,groups,FALSE,verbose);

	res<-match_subset[,c(1:2)];
	for(i in 1:length(corrList)){
		correlation<-corrList[[i]][,3];
		column<-rep(NA,times=nrow(res));
		column[match_subset[,names(corrList)[i]]]<-correlation;
		res<-cbind(res,column);
	}
	colnames(res)[-c(1,2)]<-names(corrList);
	return(res);
})


###########################################################################/**
# @RdocMethod getMixture
# 
# @title "Extract mixture model object from  JointUniquePairs and Corr objects"
#
# \description{
# Extract mixture model object from  UniquePairsMatch and Corr objects.
# Computes mixture model on Corr object optionally subsetting the Corr object
# on a list of ID Maps from the JointUniquePairs object.
# }
# 
# @synopsis
#
# \arguments{
# \item{corr}{Corr object.}
# \item{groups}{Optional @list of DB names from the JointUniquePairs object defining 
# the set of  match groups to be used for retrieving the corresponding Mixture object.}
# \item{full.group}{ If@ TRUE, uses Corr subset to compute the mixture model or uses the
# original Corr otherwise. Default is @FALSE.}
# \item{G}{ the number of mixture model components. If a numerical vector,
# the optimal mixture model is computed. Default is c(1:5).}
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{
# the Mixture object.
# }
#
# \examples{
# # create and plot mixture  (automatically determining the optimal number of components)
# #for a given DB subset treating the subset as a full group 
# mixture.subset <- examples$jointUniquePairs$getMixture(examples$corr, 
#				groups=c("NetAffx_Q", "DAVID_Q", "EnVision_Q"),
# 				full.group=TRUE, G=c(1:5), verbose=TRUE);
# mixture.subset$plot();
# }
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

 
setMethodS3("getMixture","JointUniquePairs",function(this,corr,groups=NULL,full.group=FALSE,G=c(1:5),verbose=FALSE,...){

	if (full.group)
		corr_subset<-subsetCorr(this,corr,groups,verbose)
	else
		corr_subset<-corr;

	mixture<-Mixture(corr_subset,G=G,verbose=verbose);
	return(mixture);
})


###########################################################################/**
# @RdocMethod getCorrData
# 
# @title "Create CorrData object from the JointUniquePairs object and two experiment sets"
#
# \description{
# Creates CorrData  object from JointUniquePairs object and two experiment sets
# optionally subsetting on a list of ID Maps from the UniquePairsMatch object
# }
# 
# @synopsis
#
# \arguments{
# \item{expSet1}{First ExperimentSet object with primary IDs corresponding (partially intersecting)
# with primaryIDs of the given JointUniquePairs object.}
# \item{expSet2}{Second ExperimentSet object with primary IDs corresponding (partially intersecting)
# with secondaryIDs of the given JointUniquePairs object.}
# \item{groups}{If not @NULL, defines the subset from UniquePairsMatch on which 
# the full event group to be formed. Default is @NULL.}
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{
# the CorrData object
# }
#
# \examples{
# corrData<-examples$jointUniquePairs$getCorrData(
#			examples$msmsExperimentSet,examples$mrnaExperimentSet,
#			groups=c("NetAffx_Q", "DAVID_Q", "EnVision_Q"),verbose=TRUE);
# corrData$getSampleNames();
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setMethodS3("getCorrData","JointUniquePairs",function(this,expSet1,expSet2,groups=NULL,verbose=FALSE,...){
	if(!is.null(groups))
		match_subset<-subsetGroups(this,groups,verbose=verbose)
	else
		match_subset<-this;

	corrData<-CorrData(getUniquePairs(match_subset),
				expSet1,expSet2,verbose=verbose);
	return(corrData);

})



###########################################################################/**
# @RdocMethod getBootstrap
# 
# @title "Create Bootstrap object from JointUniquePairs object and two experiment sets"
#
# \description{
# Creates Bootstrap  object from JointUniquePairs object and two experiment sets
# optionally subsetting on a list of ID Maps from the JointUniquePairs object
# }
# 
# @synopsis
#
# \arguments{
# \item{expSet1}{First ExperimentSet object with primary IDs corresponding (partially intersecting)
# with primaryIDs of the given JointUniquePairs object.}
# \item{expSet2}{Second ExperimentSet object with primary IDs corresponding (partially intersecting)
# with secondaryIDs of the given JointUniquePairs object.}
# \item{groups}{If not @NULL, defines the subset from UniquePairsMatch on which 
# the full event group to be formed. Default is @NULL.}
# \item{Fisher}{If @TRUE, the Fisher transform of data is performed during the bootstrapping. Default is @FALSE.}
# \item{R}{The number of bootstrap replicates. Default is 200.}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{
# the Bootstrap object
# }
#
# \examples{
# #create data structure containing the  bootstrapping results (correlation and sd)
# bootstrap<-examples$jointUniquePairs$getBootstrap(examples$msmsExperimentSet,examples$mrnaExperimentSet,
#		groups=c("NetAffx_Q","DAVID_Q","DAVID_F","EnVision_Q"),R=20,verbose=TRUE);
# bootstrap$plot();
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setMethodS3("getBootstrap","JointUniquePairs",function(this,expSet1,expSet2,groups=NULL,Fisher=FALSE,R=200,verbose=FALSE,...){
	corrData<-getCorrData(this,expSet1,expSet2,groups,verbose);
	bootstrap<-Bootstrap(corrData,Fisher,R,verbose);
	return(bootstrap);
})
