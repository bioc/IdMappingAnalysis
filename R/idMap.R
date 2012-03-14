

###########################################################################/**
# @RdocClass IdMap
# \encoding{latin1}
#
# @title "The ID Map class"
#
# \description{
#  @classhierarchy
#
# IdMap is an object encapsultating a data frame
# with two columns (Primary ID and Secondary ID) 
# where primaryID is a character string uniquely identifying the ID under consideration 
# (unprot accessions ID or acc, Entrez Gene ID etc) and the Secondary ID is a 
# comma separated list of secondary IDs associated with a given primary ID for a 
# particular DB service. The analysis typically starts from obtaining a set of ID Maps 
# (from the various DB services) which are not assumed to have the same number 
# of rows or the same set of primary IDs. The process of alignment of this ID Maps 
# is performed within the JointIdMap
# }
# 
# @synopsis
#
# \arguments{
# \item{DF}{A @data.frame consisting of two columns (primary and secondary IDs) from which the IdMap object is to be created.}
# \item{name}{A @character string representing the name of the given IdMap object. Default is ''}
# \item{primaryKey}{The name of the primary (first) column in an ID Map.
# If missing then the input data frame first column name is used and if it is not available defaults to 'From'.} 
# \item{secondaryKey}{ The name of secondary (second) column in an ID Map.
# If missing then the input data frame second column name is used and if it is not available defaults to 'To'.}
# \item{...}{Not used.}
# }
#
# \examples{
# obj<-IdMap(examples$identDfList[[2]]);
# obj$primaryKey();
# obj$secondaryKey();
# }
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setConstructorS3("IdMap",function(DF=NULL,name="",primaryKey=colnames(DF)[1],secondaryKey=colnames(DF)[2],...){
	#handling the copy constructor
	if(class(DF)[1]=="IdMap")
		return(IdMap(DF$.df,name,primaryKey,secondaryKey));

	if(!is.null(DF)){
		if(!inherits(DF,"data.frame"))
			throw("IdMap constructor: The first argument is not a data frame");

		if(ncol(DF)!=2)
			throw("IdMap constructor: Invalid column number (should be two)");

		if(length(unique(DF[,1]))!=nrow(DF))
			throw("IdMap constructor: Invalid primary column content (IDs should be unique)");

		if(is.null(primaryKey))
			primaryKey<-colnames(DF)[1];

		if(is.null(secondaryKey))
			secondaryKey<-"To";

		rownames(DF)<-DF[,1];
		colnames(DF)<-c(primaryKey,secondaryKey);
	}

	extend(IdMapBase(DF,name,primaryKey,secondaryKey),"IdMap");
})


#' @export merge.IdMap
###########################################################################/**
# @RdocMethod merge
#
# @title "Merge the IdMap object with a second IdMap object or a list of IdMap objects"
#
# \description{
# Merges the IdMap object (this) with a second IdMap object or a list of IdMap objects on secondary IDs 
# (second column) usind the mergeOp and on row names using the rowMergeOp.
# the result is then ordered by the primary key of 'this' IdMap object.
# The method can be used as object specific as well as static. 
# In the latter case the method merges the list of IdMap objects
# by using IdMap$merge(idMapSet) call signature.
# }
# 
# @synopsis
#
# \arguments{
# \item{idMapSet}{ IdMap object or a list of ID Map objects to merge with.}
# \item{mergeOp}{ merge operation (intersect, union etc) performed on the secondary ID list.}
# \item{rowMergeOp}{ merge operation (intersect, union etc) performed on the rows of merged ID Maps.}
# \item{verbose}{ if @TRUE enables diagnostic messages.}
# \item{...}{Not used.}
# }
# \value{An IdMap object containing the merge result.}
#
# \examples{
# obj1<-IdMap(examples$identDfList[[1]]);
# obj2<-IdMap(examples$identDfList[[2]]);
#
# #intersection
# mergedObj<-merge(obj1,obj2,intersect);
# rbind(dim(obj1),dim(obj2),dim(mergedObj));
#
# #difference 
# mergedObj<-merge(obj1,obj2,setdiff);
# rbind(dim(obj1),dim(obj2),dim(mergedObj));
# 
# #union using the static call
# mergeObj<-IdMap$merge(examples$identDfList,union,verbose=TRUE);
# }
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("merge","IdMap",function(x,y,mergeOp,rowMergeOp=mergeOp,verbose=FALSE,...) {

	mergeInternal<-function(idMap1,idMap2,mergeOp,rowMergeOp=mergeOp,verbose=FALSE) {
		if (verbose)
			cat("merging ID Map...\n");

		rowNames1<-rownames(idMap1);
		rowNames2<-rownames(idMap2);
		rowNames<-rowMergeOp(rowNames1,rowNames2);

		joinArr<-array("",dim=c(length(rowNames),3));
		rownames(joinArr)<-rowNames;
		inAll1<-rowNames1[rowNames1 %in% rowNames];
		inAll2<-rowNames2[rowNames2 %in% rowNames];
	
		joinArr[inAll1,1]<-gsub(" ","",idMap1[inAll1,2],fixed=TRUE);
		joinArr[inAll2,2]<-gsub(" ","",idMap2[inAll2,2],fixed=TRUE);

		splitList1<-strsplit(joinArr[,1],",");
		splitList2<-strsplit(joinArr[,2],",");

		join = sapply(1:length(rowNames),
					function(i) paste(mergeOp(splitList1[[i]], splitList2[[i]]),collapse=","));
	
		res<-cbind(rowNames,join);
		colnames(res)<-colnames(idMap1);
		rownames(res)<-rowNames;
		return(as.data.frame(res,stringsAsFactors=FALSE));
	}

	#handle static method invocation
	if(is.null(x$.df))
		x<-IdMap(as.data.frame(y[[1]]),name=names(y)[1]);

	if(inherits(y,"list")){
		res<-as.data.frame(x);
		for (name in names(y)){
			if (verbose)
				cat(name,": ");
			res<-mergeInternal(res,as.data.frame(y[[name]]),mergeOp,rowMergeOp,verbose);
		}
	}else{
		res<-mergeInternal(as.data.frame(x),as.data.frame(y),mergeOp,rowMergeOp,verbose);
	}

	return(IdMap(res,name=getName(x),primaryKey=primaryKey(x),secondaryKey=secondaryKey(x)));
},createGeneric=FALSE)



###########################################################################/**
# @RdocMethod getCounts
#
# @title "Compute the count of secondaryIDs for each primary ID"
# \description{@get "title".}
#
# @synopsis
#
# \arguments{
# \item{idMap}{The IdMap object on which secondary ID counts should be computed.}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used.}
# }
# \value{@numeric vector of counts of secondary IDs for each primary ID.}
#
# \examples{
# obj<-IdMap(examples$identDfList[[3]]);
# cnts<-obj$getCounts();
# }
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getCounts","IdMap",function(this,verbose=FALSE,...){
	if (verbose)
		cat("computing secondary ID counts...")

	primaryList<-rownames(this$.df);
	trimmedStrings = gsub(" ","",this$.df[,2],fixed=TRUE);
	secondaryIdList = strsplit(trimmedStrings , ",")
	sz<-length(secondaryIdList);
	res<-c(1:sz);
	for (i in 1:sz){
		if (verbose)
			Display$progressMsg("processed:",i,sz);
		res[i]<-length(secondaryIdList[[i]]);
	} 
	return(res);
})


###########################################################################/**
# @RdocMethod as.UniquePairs
# 
#
# @title "Create a UniquePairs object from a given IdMap object"
#
# \description{@get "title".}
#
# @synopsis
#
# \arguments{
# \item{secondaryIDs}{optional secondary ID list on which the resulting UniquePairs
# object is intersected. Default is NULL (not present).}
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{A UniquePairs object} 
#
# \examples{
# idMap<-IdMap(examples$identDfList[[1]]);
# uniquePairs<-as.UniquePairs(idMap);
# dim(uniquePairs);
# uniquePairs[1:10,];
# }
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("as.UniquePairs","IdMap",function(this,secondaryIDs=NULL,verbose=FALSE,...){
	return(UniquePairs$create(this,secondaryIDs=secondaryIDs,keepMissing=FALSE,verbose=verbose));
})

###########################################################################/**
# @RdocMethod swapKeys
#
# @title "Swap the primary and secondary key columns"
# \description{@get "title".}
#
# @synopsis
#
# \arguments{
# \item{idMap}{The IdMap object or a list of IdMap objects which keys to be swapped.}
# \item{verbose}{ If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used.}
# }
# \value{IdMap object or list of IdMap objects with swapped keys.}
#
# \examples{
# uniprot2Affy<-IdMap(examples$identDfList[[1]]);
# uniprot2Affy[1:10,];
#
# affy2Uniprot<-IdMap$swapKeys(uniprot2Affy);
# affy2Uniprot[1:10,];
# }
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("swapKeys","IdMap",function(static,idMap,verbose=FALSE,...) {

  .swapKeys<-function(mapDF,verbose){
	if(verbose)
		cat("Swapping ID Map keys\n");
	pairs<-UniquePairs$swapKeys(as.UniquePairs(mapDF,verbose=verbose));
	mapDF<-as.IdMap(pairs,verbose=verbose);
	return(mapDF);
  }

  if (inherits(idMap,"list")){
	res<-list();
	for (name in names(idMap)){
		if(verbose)
			cat(name,":");
		idMap<-.swapKeys(idMap[[name]],verbose);
		res[[name]]<-idMap;
	}		
	return(res);
  }else{
	idMap<-.swapKeys(idMap,verbose);
	return(idMap);
  }
},static=TRUE)

#' @export as.list.IdMap
###########################################################################/**
# @RdocMethod as.list
# 
# @title "Coerce an object or a list of compatible object
# to the IdMap object or a list of IdMap objects"
#
# \description{
# @get "title".
# The object should be inherited from @matrix, @data.frame, IdMap or UniquePairs,
# otherwise an exception will be thrown.
# }
# 
# @synopsis
#
# \arguments{
# \item{idMapList}{A @list or a single object of compatible type.}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{
# A single IdMap object or a list of IdMap objects.
# }
#
# \examples{
# idMaps<-IdMap$as.list(examples$identDfList);
# names(idMaps);
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setMethodS3("as.list","IdMap",function(x,idMapList,verbose=FALSE,...){
	if(!inherits(idMapList,"list"))
		idMapList<-list(idMapList);

	res<-list();
	if(is.null(names(idMapList)))
		names(idMapList)<-paste("IdMap",c(1:length(idMapList)),sep="_");
	num=1;
	for(i in 1:length(idMapList)){
		name<-names(idMapList)[i];
		if(name==""){
			name<-paste("IdMap",num.sep="_");
			num<-num+1;
		}
		if(verbose)
			cat("coercing item ",i," (",name,")\n");			
		if(inherits(idMapList[[i]],"data.frame") || 
		   inherits(idMapList[[i]],"matrix"))
			res[[name]]<-IdMap(idMapList[[i]])
		else if(inherits(idMapList[[i]],"UniquePairs"))
			res[[name]]<-as.IdMap(idMapList[[i]])
		else if (inherits(idMapList[[i]],"IdMap"))
			res[[name]]<-idMapList[[i]]
		else
			throw("as.IdMap: cannot coerce to IdMap");
	}
	return(res);
},createGeneric=FALSE,static=TRUE)




