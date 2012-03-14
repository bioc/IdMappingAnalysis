
###########################################################################/**
# @RdocClass UniquePairs
# \encoding{latin1}
#
# @title "The UniquePairs class"
#
# \description{
#  @classhierarchy
#
# The alternative representation of an IdMap suitable for performing
# the correlation related processing. Contains a data frame with two columns, 
# each row of which represents a unique pair <primary ID, secondary ID>
# where primary ID corresponds to the primaryIDs of an ID Map and secondary ID 
# corresponds to a single ID from a list of comma separated secondary IDs within
# the corresponding ID Map. The column names correspond to the primary/secondary 
# keys of an Id Map ('acc' and 'probeset' for example)
# }
# 
# @synopsis
#
# \arguments{
# \item{DF}{A @data.frame consisting of two columns (primary and secondary IDs) from which the UniquePairs object is to be created.}
# \item{name}{A @character string representing the name of the given UniquePairs object. Default is ''}
# \item{primaryKey}{The name of the primary (first) column of a @data.frame encapsulated within the UniquePairs object.
# If missing then the input data frame first column name is used and if it is not available defaults to 'From'.} 
# \item{secondaryKey}{ The name of secondary (second) column in an ID Map.
# If missing then the input data frame second column name is used and if it is not available defaults to 'To'.}
# \item{...}{Not used.}
# }
#
# \examples{
# DF<-matrix(
#  c("P25685","200664_s_at",
#    "P25685","200666_s_at",
#    "Q6ZV71","205208_at",
#    "Q6ZV71","215798_at",
#    "P05164", "203948_s_at"
#  ),ncol=2,nrow=5,byrow=TRUE);
# colnames(DF)<-c("Uniprot","Affy");
#
# uniquePairs<-UniquePairs(DF);
#}
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setConstructorS3("UniquePairs",function(DF=NULL,name="",
	primaryKey=colnames(DF)[1],secondaryKey=colnames(DF)[2],...){
	if(!is.null(DF)){
		if(ncol(DF)!=2)
			throw("IdMap constructor: Invalid column number (should be two)");
		if(is.null(secondaryKey))
			secondaryKey<-"To";

		colnames(DF)[2]<-secondaryKey;
	}
	extend(IdMapBase(DF,name,primaryKey,secondaryKey),"UniquePairs");
})


###########################################################################/**
# @RdocMethod create
# 
#
# @title "Create a UniquePairs object from a single IdMap or a list of IdMap objects"
#
# \description{
# Create a UniquePairs object by converting a single IdMap or a list of Id Maps into 
# a single or list of unique pairs data structures optionally intersecting
# the secondary ID set with an external secondary ID set if farther data 
# size reduction is necessary
# }
#
# @synopsis
#
# \arguments{
# \item{idMapSet}{an object or a list of object which can be coerced
# into IdMap object(s) to be converted into UniquePairs object (list).}
# \item{secondaryIDs}{optional secondary ID list on which the resulting UniquePairs
# object is intersected. Default is NULL (not present).}
# \item{keepMissing}{ogical indicating if the rows with empty secondary IDs should removed
# from the resulting object. Default is FALSE (keep such rows)}
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{A UniquePairs object or a @list of UniquePairs objects} 
#
# \examples{
# uniquePairs<-UniquePairs$create(IdMap(examples$identDfList[[1]]));
# uniquePairsList<-UniquePairs$create(examples$identDfList,verbose=TRUE);
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setMethodS3("create","UniquePairs",function(static,idMapSet,secondaryIDs=NULL,keepMissing=FALSE,verbose=FALSE,...) {

	.getUniquePairs<-function(idMap,secondaryIDs=NULL,keepMissing=FALSE,verbose=FALSE) {
		if (verbose)
			cat("unfolding ID Map ...\n");

		primaryList<-rownames(idMap);
		trimmedStrings = gsub(" ","",idMap[,2],fixed=TRUE);
		if(keepMissing)
			trimmedStrings[trimmedStrings==""]<-NA;
		secondaryIdList = strsplit(trimmedStrings , ",")

		lengths<-unlist(lapply(secondaryIdList , function(x) length(x)));
		res<-array("",dim=c(sum(lengths),2));
		res[,1]<-rep(idMap[,1],times=lengths);
		res[,2]<-unlist(secondaryIdList);
		if (!is.null(secondaryIDs))
			res<-Subset$byColumn(res,secondaryIDs,2,verbose);
		colnames(res)<-colnames(idMap);
		return (UniquePairs(as.data.frame(res,stringsAsFactors=FALSE),name=idMap$.name));
	}


	if(!inherits(idMapSet,"list")){
		return(.getUniquePairs(idMapSet,secondaryIDs,keepMissing,verbose));
	}

	if (verbose) {
		cat("retrieving unique pairs for <",names(idMapSet),">\n");
	}
	res<-list();
	for (name in names(idMapSet)){
		if (verbose)
			cat(name,":");
		res[[name]]<-.getUniquePairs(idMapSet[[name]],secondaryIDs,keepMissing,verbose);
	}
	return(res);


},static=TRUE)

###########################################################################/**
# @RdocMethod swapKeys
#
# @title "Swap the primary and secondary key columns"
# \description{@get "title".}
#
# @synopsis
#
# \arguments{
# \item{uniquePairs}{The UniquePairs object or a list of UniquePairs objects which keys to be swapped.}
# \item{...}{Not used.}
# }
# \value{UniquePairs object or list of UniquePairs objects with swapped keys.}
#
# \examples{
# pairs<-UniquePairs$create(examples$identDfList[[1]]);
# pairs[1:10,];
# swapped_pairs<-UniquePairs$swapKeys(pairs);
# swapped_pairs[1:10,];
# }
# 
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################



setMethodS3("swapKeys","UniquePairs",function(static,uniquePairs,...){
	res<-uniquePairs;
	res$.df<-res$.df[,c(2,1)];
	return(res);
},static=TRUE)


###########################################################################/**
# @RdocMethod as.IdMap
#
# @title "Convert the UniquePairs object into the IdMap object"
#
# \description{@get "title".}
#
# @synopsis
#
# \arguments{
# \item{keepOrder}{@logical indicating if the original order of primary IDs should be kept. Default is TRUE}
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used.}
# }
# \value{UniquePairs object or list of UniquePairs objects with swapped keys.}
#
# \examples{
# pairs<-UniquePairs$create(examples$identDfList[[2]]);
# idMap<-as.IdMap(pairs);
# idMap[1:10,];
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setMethodS3("as.IdMap","UniquePairs",function(this,keepOrder=TRUE,verbose=FALSE,...){
	if (verbose)
		cat("converting unique pairs into the ID Map ...\n");

	f<-factor(this$.df[,1]);
	splits<-split(this$.df[,2],f);
	secondaryList<-lapply(splits,paste,collapse=",");
	secondaryIDs<-unlist(secondaryList);
	res<-cbind(levels(f),secondaryIDs);

	if(keepOrder) {
		primaryIDs<-unique(this$.df[,1]);
		res<-res[primaryIDs,];
	}
	
	colnames(res)<-colnames(this$.df);
	
	idMap<-IdMap(as.data.frame(res,stringsAsFactors=FALSE),name=getName(this));
	return(idMap);
})


###########################################################################/**
# @RdocMethod equals
#
# @title "Check  if two unique pairs data structures are identical"
#
# \description{@get "title".}
#
# @synopsis
#
# \arguments{
# \item{other}{Another UniquePairs object the given object os compared to}
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used.}
# }
# \value{@logical indicating if two UniquePairs objects are equal}
#
# \examples{
# pairs1<-UniquePairs$create(examples$identDfList[[1]]);
# pairs2<-UniquePairs$create(examples$identDfList[[2]]);
# pairs3<-pairs1;
#
# pairs1$equals(pairs2);
# pairs1$equals(pairs3); 
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("equals","UniquePairs",function(this,other,...){
	if(!setequal(colnames(this$.df),colnames(other$.df)))
		return(FALSE);
	if(nrow(this$.df)!=nrow(other$.df))
		return(FALSE);
	if(sum(getMatch(this,other))!=nrow(this$.df))
		return(FALSE);
	return(TRUE);
})


###########################################################################/**
# @RdocMethod getMatch
#
# @title "Get the logical vector of pair matches of the given UniquePairs object
# in other UniquePair object(s)"
#
# \description{
# Compute the logical vector of matches or a list of such vectors 
# between a given UniquePairs object and another UniquePairs object or a list of such objects.
# Analog of match() or \%in\% for vectors
# }
#
# @synopsis
#
# \arguments{
# \item{other}{Another UniquePairs object or a list of objects the given object os compared to}
# \item{...}{Not used.}
# }
#
# \value{@logical vector or a list of logical vectors indicating if the given
# row in other UniquePairs object(s) is present in a given UniquePairs object.}
#
# \examples{
# pairs1<-UniquePairs$create(examples$identDfList[[1]]);
# pairs2<-UniquePairs$create(examples$identDfList[[2]]);
# matches<-pairs1$getMatch(pairs2);
# matches[1:50];
# }
# 
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getMatch","UniquePairs",function(this,other,verbose=FALSE,...){
	matchInternal<-function(pairs1,pairs2,verbose=FALSE) {
		if(verbose)
			cat("matching unique pairs...\n");
		unique1<-paste(pairs1[,1],pairs1[,2],sep="\r");
		unique2<-paste(pairs2[,1],pairs2[,2],sep="\r");
		res=unique1 %in% unique2;
		return(res);
	}

	if (!inherits(other,"list"))
		return(matchInternal(this,other));
	
	
	res<-list();
	for (name in names(other)){
		if (verbose)
			cat(name,":");
		res[[name]]<-matchInternal(this,other[[name]],verbose);
	}
	return(res);
})

#' @export unique.UniquePairs
###########################################################################/**
# @RdocMethod unique
#
# @title "Extract unique elements"
#
# \description{
# Removes duplicate ID pairs from a given UniquePairs object so the resuting 
# UniquePairs object contains only unique ID pairs
# }
#
# @synopsis
#
# \arguments{
#\ item{incomparables}{Not used (defined for compatibility with generic).}
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used.}
# }
#
# \value{UniquePairs object with dublicate ID pairs removed.}
# 
# \examples{
# pairs1<-UniquePairs$create(examples$identDfList[[1]]);
# pairs2<-UniquePairs$create(examples$identDfList[[2]]);
# dup_pairs<-UniquePairs(rbind(as.data.frame(pairs1),as.data.frame(pairs2)));
# dim(dup_pairs);
# unique_pairs<-dup_pairs$unique();
# dim(unique_pairs);
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("unique","UniquePairs",function(x,incomparables=FALSE,verbose=FALSE,...){
	if(verbose)
		cat("Extracting unique pairs...\n");

	pairs<-as.data.frame(x);

	pairs[pairs[,1]=="",1]<-" ";
	pairs[pairs[,2]=="",2]<-" ";
	uniques<-unique(paste(pairs[,1],pairs[,2],sep="\r"));
	data<-unlist(strsplit(uniques,split="\r",fixed=TRUE));
	inds<-seq(from=1,to=length(data),by=2);
	unique_pairs<-cbind(data[inds],data[inds+1]);
	unique_pairs[unique_pairs[,1]==" ",1]<-"";
	unique_pairs[unique_pairs[,2]==" ",2]<-"";
	colnames(unique_pairs)<-colnames(pairs);

	uniquePairs<-UniquePairs(as.data.frame(unique_pairs,stringsAsFactors=FALSE),name=getName(x));
	
	return(uniquePairs);
},createGeneric=FALSE)


