
###########################################################################/**
# @RdocClass IdMapDiff
# \encoding{latin1}
#
# @title "The IdMapDiff class"
#
# \description{
#  @classhierarchy
#
# IdMapDiff constructor implements most time consuming step in comparing
# two DBs and the structure itself stores the results in a compact form.
# The IdMapDiff object encapsultates a @data.frame the first column
# of which contains the primary IDs and the rest of columns contain a disjoint 
# representation of the ID Map pair in the form of 3 columns <A-A*B,A*B,B-A*B>,
# where A and B are secondary ID lists for ID Maps A and B. 
# This class is separated from the IdMapDiffCounts 
# in anticipation of being used by various processing pipelines in a future.
# }
# 
# @synopsis
#
# \arguments{
# \item{idMap1}{The first ID Map object on which IdMapDiff object is constructed.}
# \item{idMap2}{The second ID Map object on which IdMapDiff object is constructed.}
# \item{pairNames}{The character vector of length 2 representing the names of the ID Map pair.
# Default is c('First','Second').}
# \item{verbose}{If @TRUE enables diagnostic messages.}
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
# # get IdMap list aligned of two ID maps aligned by primaryIDs
# idMaps<-jointIdMap$getIdMapList(verbose=TRUE);
#
# #create IdMapDiff object
# diffs<-IdMapDiff(idMaps[["NetAffx_F"]],idMaps[["DAVID_Q"]]);
# diffs[1:10,];
#
# # create IdMapDiff object directly from JointIdMap
# diffs<-jointIdMap$getDiff("NetAffx_F","DAVID_Q",verbose=TRUE);
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setConstructorS3("IdMapDiff",function(idMap1=NULL,idMap2=NULL,pairNames=c("First","Second"),verbose=FALSE,...){
	if(is.null(idMap1) || is.null(idMap2)){
		res<-NULL;
		secondaryKey<-"";
	} else {

		if(!aligned(idMap1,idMap2))
			throw("IdMapDiff constructor: ID Maps mismatch");

		if(verbose)
			cat("creating an IdMapDiff object...\n");

		primaryKey<-primaryKey(idMap1);
		secondaryKey<-secondaryKey(idMap1);

	
		split1 = strsplit(idMap1[,2], ",");
		split2 = strsplit(idMap2[,2], ",");

		diffs<-array(list(),dim=c(nrow(idMap1),3),dimnames<-list(NULL,c("a_b","ab","b_a")));	
		diffs[,"a_b"]<-Misc$CsvList.merge(split1,split2,mergeOp=setdiff,asStrings=FALSE);
		diffs[,"ab"]<-Misc$CsvList.merge(split1,split2,mergeOp=intersect,asStrings=FALSE);
		diffs[,"b_a"]<-Misc$CsvList.merge(split2,split1,mergeOp=setdiff,asStrings=FALSE);

		res<-cbind(primaryIDs(idMap1),as.data.frame(diffs,stringsAsFactors=FALSE));
	
	}

	extend(IdMapBase(res,primaryKey=primaryKey,secondaryKey=secondaryKey),
		"IdMapDiff",
		.pairNames=pairNames
	);
	
})


