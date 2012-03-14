###########################################################################/**
# @RdocClass JointIdMap
# \encoding{latin1}
#
# @title "The Joint ID Map class"
#
# \description{
# @classhierarchy
#
# JointIdMap is an object encapsulating a @data.frame 
# containing the primary ID set in a first column
# while the rest of columns containing the sets of secondary IDs, each column 
# corresponding  to a particular Id Map, keeping all Id Maps properly aligned
# }
#
# @synopsis
# 
# \arguments{
# \item{idMapList}{The @list of ID Maps on which the JointData is constructed.}
# \item{primaryIDs}{The optional @character vector of primary IDs on which an additional
# intersection and reordering are performed.}
# \item{name}{The optional name of a given JointIdMap object. Default is ''}
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FASLE.}
# \item{...}{Not used}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \examples{
# jointIdMap<-JointIdMap(examples$identDfList);
#  
# jointIdMap$primaryKey();
# jointIdMap$secondaryKey();
#
# jointIdMap[1:10,]; 
# }
# \author{Roger Day,Alex Lisovich}
#*/###########################################################################

setConstructorS3("JointIdMap",function(idMapList=list(),primaryIDs=NULL,name="",verbose=FALSE,...){
	if(length(idMapList)>0){
		if (verbose)
			cat("creating joint ID Map object...\n");

		if(is.null(primaryIDs)){
			primaryIDs<-as.data.frame(idMapList[[1]])[,1];
			for(idMap in idMapList){
				primaryIDs<-intersect(primaryIDs,as.data.frame(idMap)[,1]);
			}		
		}
	
		#leave only rows those intersecting with primaryIDs and order by primary IDs
		idMapList<-Subset$byRowNames(idMapList,primaryIDs,verbose=verbose);
		#expand the rows set so it matches the primaryID set
		idMapList<-Subset$byRowNames(idMapList,primaryIDs,mergeOp=union,verbose=verbose);

		primaryKey<-colnames(idMapList[[1]])[1];
		secondaryKey<-colnames(idMapList[[1]])[2];
		DF<-as.data.frame(primaryIDs,stringsAsFactors=FALSE);	
	
		for (idMap in idMapList){
			secondaryIDs<-tolower(gsub(" ","",idMap[,2],fixed=TRUE));
			DF<-cbind(DF,as.data.frame(secondaryIDs,stringsAsFactors=FALSE));
		}
		colnames(DF)<-c(primaryKey,names(idMapList));
	} else {
		DF<-NULL;
		secondaryKey<-"";
	}
		
	extend(IdMapBase(DF,name,secondaryKey=secondaryKey),"JointIdMap");
})


###########################################################################/**
# @RdocMethod getMapNames
# 
# @title "Get the names of IdMap objects encapsulated within the given JointIdMap object"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{\item{...}{Not used}}
#
# \value{A @character vector of IdMap names}
#
# \examples{
# jointIdMap<-JointIdMap(examples$identDfList);
# jointIdMap$getMapNames(); 
# }
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getMapNames","JointIdMap",function(this,...){
	return(colnames(this$.df)[-1]);
})


#' @export as.data.frame.JointIdMap
###########################################################################/**
# @RdocMethod as.data.frame
# 
# @title "Retrieve a data frame encapsulated within the given JointIdMap object"
# \description{@get "title" with additional 'name' and 'secondaryKey' attributes}
# 
# @synopsis
#
# \arguments{
# \item{row.names}{Not used}
# \item{optional}{Not used}
# \item{...}{Not used}
# }
# \value{A @data.frame encapsulated within the given JointIdMap object. 
# The data frame attributes 'name' and 'secondaryKey' contain the 
# values of corresponding JointIdMap object data fields.}
#
# \examples{
# jointIdMap<-JointIdMap(examples$identDfList);
# df<-as.data.frame(jointIdMap);
# dim(df);
# attr(df,"secondaryKey");
# }
# 
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("as.data.frame","JointIdMap",function(x,row.names = NULL, optional = FALSE,...){
	res<-IdMapBase$as.data.frame(x);
	attr(res,"secondaryKey")<-secondaryKey(x);
	return(res);
},createGeneric=FALSE)



###########################################################################/**
# @RdocMethod getIdMapList
# 
# @title " Create an Id Map list from a JointIdMap object"
#
# \description{
# Create an IdMap list from a JointIdMap object ensuring that the resulting
# ID Map list has the same set and order of primary IDs
# }
# 
# @synopsis
#
# \arguments{
# \item{idMapNames}{optional vector of ID Map names within the jointIdMap for which the ID Map list constructed.
# If @NULL (default), then all ID Maps within the jointIdMap will be used.}
# \item{...}{Not used}
# }
#
# \value{A @list of IdMap objects}
#
# \examples{
# jointIdMap<-JointIdMap(examples$identDfList);
# idMaps<-jointIdMap$getIdMapList(idMapNames=jointIdMap$getMapNames()[1:3]);
# class(idMaps[[1]]);
# names(idMaps);
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getIdMapList","JointIdMap",function(this,idMapNames=NULL,verbose=FALSE,...){
	if (verbose)
		cat("creating ID Map list from the joint Map object...\n");
	res<-list();

	if (is.null(idMapNames))
		idMapNames<-getMapNames(this);
	
	inclusion<-idMapNames %in% getMapNames(this);
	if(sum(inclusion)!=length(idMapNames)){
		throw(paste("getIdMapList.JointIdMap: the invalid idMapNames:\n",
				paste(idMapNames[inclusion],collapse=", ")));
	}

	for (name in idMapNames){
		if (verbose)
			cat("creating ID Map:",name,"\n");
		idMap<-cbind(primaryIDs(this),this$.df[,name]);
		colnames(idMap)<-c(primaryKey(this),secondaryKey(this));
		rownames(idMap)<-idMap[,1];
		res[[name]]<-IdMap(as.data.frame(idMap,stringsAsFactors=FALSE),name=name);
	}
	return(res);
})



###########################################################################/**
# @RdocMethod getUnionIdMap
# 
# @title " Create a union IdMap"
#
# \description{
# Create a single IDMap object each secondary ID set of which 
# is a union of all secondary IDs in a JointIdMap object for a given primary ID
# }
# 
# @synopsis
#
# \arguments{
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FASLE.}
# \item{...}{Not used}
# }
#
# \value{A union IdMap object}
#
# \examples{
# jointIdMap<-JointIdMap(examples$identDfList);
# idMap<-jointIdMap$getUnionIdMap(verbose=TRUE);
# idMap[1:10,];
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################
		
setMethodS3("getUnionIdMap","JointIdMap",function(this,verbose=FALSE,...){
	idMapList<-getIdMapList(this,verbose=verbose);
	res<-IdMap$merge(idMapList,mergeOp=union,verbose=verbose);

	return(res);
})



###########################################################################/**
# @RdocMethod getMatchInfo
# 
# @title "Get match table(s) for a given set of primary IDs"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{IDs}{A @character vector of primary IDs}
# \item{idMapNames}{A @character vector of Id Map names encapsulated within the jointIdMap object or NULL,
# in which case all Id Maps are used. Default is NULL.}
# \item{...}{Not used}
# }
#
# \value{A @list of match table(s) for a given set of primary IDs. The match table is a data frame
# where the column set is a union of secondary IDs over all idMapNames for a given primary ID 
# while each row containes the matches between the primary ID and a particular secondary ID
# for a given IdMap.}
#
# \examples{
# jointIdMap<-JointIdMap(examples$identDfList);
# #get match info for Uniprot accessions P48539 and Q5T089
# matches<-jointIdMap$getMatchInfo(c("P48539","Q5T089"));
# print(matches);
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getMatchInfo","JointIdMap",function(this,IDs,idMapNames=NULL,...){

	if(is.null(idMapNames))
		idMapNames=getMapNames(this);

	primaryIDs<-primaryIDs(this);
	IDs<-IDs[IDs %in% primaryIDs];
	
	res<-list();

	for(ID in IDs){
		
		#ind<-which(primaryIDs==ID);
		idInfo<-strsplit(as.character(this$.df[primaryIDs==ID,idMapNames]),split=",");
		names(idInfo)<-idMapNames;
		all<-unique(unlist(idInfo));

		match<-array("",dim=c(length(idMapNames),length(all)),dimnames=list(idMapNames,all));
		for(name in idMapNames){
			db.ids<-idInfo[name];
			includes<-all %in% idInfo[[name]];
			match[name,includes]<-all[includes];
		}
		res[[ID]]<-as.data.frame(match,stringsAsFactors=FALSE);
	}
	return(res);
})



###########################################################################/**
# @RdocMethod getCounts
# 
# @title "Create an IdMapCounts object"
# \description{
# @get "title".
# A fail-safe way to create an IdMapCounts from the ID Map list 
# obtained internally through the call to getIdMapList.JointIdMap method
# and therefore guaranteed to be properly aligned
# }
# 
# @synopsis
#
# \arguments{
# \item{idMapNames}{Subset of ID Maps to be used to create IdCounts object. 
# If @NULL (default) all ID maps within the given object included.}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{An IdMapCounts object containing information about the secondary
# ID counts for each primary ID for all ID Maps encapsulated within the given JointIdMap object}
#
# \examples{
# jointIdMap<-JointIdMap(examples$identDfList);
# cnts<-jointIdMap$getCounts();
# cnts[1:10,];
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getCounts","JointIdMap",function(this,idMapNames=NULL,verbose=FALSE,...){
	if(is.null(idMapNames))
		idMapNames<-getMapNames(this);
	idMapList<-getIdMapList(this,idMapNames,verbose=verbose);
	res<-IdMapCounts(idMapList,verbose=verbose);
	return(res);
})


###########################################################################/**
# @RdocMethod getDiff
# 
# @title "Create an IdMapDiff object"
#
# \description{
# A fail-safe way to create IdMapDiff object from the ID Map pair 
# which is guaranteed to be properly aligned (see also IdMapDiff class)
# }
# 
# @synopsis
#
# \arguments{
# \item{idMapName1}{The name of the first ID Map within the joint ID Map object.}
# \item{idMapName2}{Thee name of the second ID Map within the joint ID Map object.}
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FASLE.}
# \item{...}{Not used}
# }
#
# \value{
# An IdMapDiff object created from two IdMap objects extracted
# from a given JointIdMap object based on their names (idMapName1 and idMapName2).
# }
#
# \examples{
# jointIdMap<-JointIdMap(examples$identDfList);
# mapDiff<-jointIdMap$getDiff("NetAffx_F","DAVID_Q",verbose=TRUE);
# class(mapDiff);
# mapDiff[1:10,];
# }
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setMethodS3("getDiff","JointIdMap",function(this,idMapName1,idMapName2,verbose=FALSE,...){
	idMaps<-getIdMapList(this,c(idMapName1,idMapName2),verbose);
	res<-IdMapDiff(idMaps[[1]],idMaps[[2]],names(idMaps),verbose=verbose);
	return(res);
})



