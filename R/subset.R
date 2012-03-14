###########################################################################/**
# @RdocClass Subset
# \encoding{latin1}
#
# @title "The Subset class"
#
# \description{
# @classhierarchy
# 
# Serves as a wrapper for data frame subsetting functions 
# defined as static methods of the Subset class.
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

setConstructorS3("Subset",function(...){
	extend(Object(),"Subset");
})



###########################################################################/**
# @RdocMethod byColNames
# 
# @title "Extract subset of columns from a data frame or a list of data frames"
#
# \description{
# @get "title".
# This function outputs a data frame which set of columns is a product of merging according to mergeOp
# of the imput data frame column set with a given set of names (subset) ordred either by the original column set
# or by a a subset parameter according to orderBySubset flag. In case the merging operation produces the columns
# which are not in an original data frame, the new columns are filled with value from na.value parameter.
# }
# 
# @synopsis
#
# \arguments{
# \item{frameData}{Input @data.frame or a list of data frames.}
# \item{subset}{@character @vector of names partially intersecting with given @data.frame 
# column names and defining the set on which merging is to be performed.}
# \item{orderBySubset}{Determines if the resulting data frame(s) columns will be ordered by subset (@TRUE)
# or by the original column set (@FALSE). Default is @TRUE.}
# \item{mergeOp}{The merging operation to be performed on column names. Default in 'intersect'.}
# \item{na.value}{The value which should be used to field the empty columns 
# in the output @data.frame (s). Default is "".}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{
# A @data.frame or list of data frames which column set is a product of
# column merging on a subset according to the mergeOp.
# }
#
# \examples{
# commonSamples<-intersect(colnames(examples$msmsExperimentSet),
#		colnames(examples$msmsExperimentSet));
# mrna.subset<-Subset$byColNames(examples$mrnaExperimentSet,
#		commonSamples,orderBySubset=TRUE);
# mrna.subset[1:10,1:5];
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("byColNames","Subset",function(static,frameData,subset,orderBySubset=TRUE,
			mergeOp=intersect,na.value="",verbose=FALSE,...) {
	subsetByColNamesInternal<-function(dataFrame,subset,orderBySubset=TRUE,verbose=FALSE) {
		if (verbose)
			cat("extracting subset by column names...\n");

		dataFrame<-as.data.frame(dataFrame,stringsAsFactors=FALSE);
		subset<-as.data.frame(subset);
		subset<-as.character(as.matrix(subset));

		names<-colnames(dataFrame);
		if (colnames(dataFrame)[1]!=subset[1])
			subset<-c(colnames(dataFrame)[1],subset);

		if (orderBySubset){
			commonNames<-intersect(subset,names);
		} else {
			commonNames<-intersect(names,subset);
		}

		appCols<-sum(!(commonNames %in% names));
		if (appCols>0){
			commons<-as.data.frame(array(na.value,
				dim=c(nrow(dataFrame),length(commonNames))),stringsAsFactors=FALSE);
			colnames(commons)<-commonNames;
			rownames(commons)<-rownames(dataFrame);
			dataNames<-commonNames[commonNames %in% names];
			commons[,dataNames]<-dataFrame[,dataNames];
		}else{
			commons<-dataFrame[,commonNames];
		}
		return(commons);
	}

	if(!inherits(frameData,"list"))
		return(subsetByColNamesInternal(frameData,subset,orderBySubset,verbose));

	res<-list();
	for (name in names(frameData)){
		if (verbose)
			cat("frame",name,": ");
		res[[name]]<-subsetByColNamesInternal(frameData[[name]],subset,orderBySubset,verbose);
	}
	return(res);

},static=TRUE)



###########################################################################/**
# @RdocMethod byRowNames
# 
# @title "Extract subset of columns from a data frame or a list of data frames"
#
# \description{
# @get "title".
# This function outputs a data frame which set of rows is a product of merging according to mergeOp
# of the imput data frame row set with a given set of names (subset) ordred either by the original row set
# or by a a subset parameter according to orderBySubset flag. In case the merging operation produces the rows
# which are not in an original data frame, the new rows are filled with value from na.value parameter.
# }
# 
# @synopsis
#
# \arguments{
# \item{frameData}{Input @data.frame or a @list of data frames.}
# \item{subset}{@character vector of names partially intersecting with given data frame 
# row names and defining the set on which merging is to be performed.}
# \item{orderBySubset}{Determines if the resulting data frame(s) rows will be ordered by subset (@TRUE
# or by the original row set (@FALSE). Default is @TRUE.}
# \item{mergeOp}{The merging operation to be performed on row names. Default in 'intersect.}
# \item{na.value}{The value which should be used to field the empty rows in the 
# output data frame(s). Default is "".}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{
# A @data.frame or list of data frames which row set is a product of
# row merging on a subset according to the mergeOp.
# }
#
# \examples{
# proteins<-examples$jointIdMap_corr$primaryIDs();
# msms.subset<-Subset$byRowNames(examples$msmsExperimentSet,proteins,orderBySubset=TRUE);
# dim(examples$msmsExperimentSet);
# dim(msms.subset);
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("byRowNames","Subset",function(static,frameData, subset,orderBySubset=TRUE,
			mergeOp=intersect,na.value="",verbose=FALSE,...) {
	subsetByRowNamesInternal<-function(dataFrame, subset,orderBySubset=TRUE,verbose=FALSE) {
		if (verbose)
			cat("extracting subset by row names...\n");

		dataFrame<-as.data.frame(dataFrame,stringsAsFactors=FALSE);
		subset<-as.data.frame(subset);
		subset<-as.character(as.matrix(subset));
		names<-rownames(dataFrame);

		if (orderBySubset){
			commonNames<-mergeOp(subset,names);
		} else {
			commonNames<-mergeOp(names,subset);
		}

		appRows<-sum(!(commonNames %in% names));
		if (appRows>0){
			commons<-as.data.frame(array(na.value,
				dim=c(length(commonNames),ncol(dataFrame))),stringsAsFactors=FALSE);
			rownames(commons)<-commonNames;
			colnames(commons)<-colnames(dataFrame);
			dataNames<-commonNames[commonNames %in% names];
			commons[dataNames,]<-dataFrame[dataNames,];
		}else{
			commons<-dataFrame[commonNames,];
		}
		return(commons); 
	}

	if(!inherits(frameData,"list"))
		return(subsetByRowNamesInternal(frameData,subset,orderBySubset,verbose));

	res<-list();
	for (name in names(frameData)){
		if (verbose)
			cat("frame",name,": ");
		res[[name]]<-subsetByRowNamesInternal(frameData[[name]],subset,orderBySubset,verbose);
	}
	return(res);
},static=TRUE)



###########################################################################/**
# @RdocMethod byRow
# 
# @title "Extract subset of columns from a data frame or a list of data frames
# by intersecting on a particular row"
#
# \description{
# @get "title".
# This function outputs a data frame which set of columns is a product of intersecting on a particular row
# of the imput data frame(s) columns with a given set of names (subset).
# }
# 
# @synopsis
#
# \arguments{
# \item{frameData}{Input @data.frame or a @list of data frames.}
# \item{subset}{@character @vector of names partially intersecting with given @data.frame 
# row and defining the set on which merging is to be performed.}
# \item{row}{The row on which the intersection is to be performed.}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{
# A @data.frame or list of data frames which column set is a product of
# intersecting of a particular row with a (partially intersecting) subset.
# }
#
# \examples{
# #extract msms experiment subset containing only samples
# #for which the event count for Uniprot='P04264' takes predefined values 1,2 and 3
# cnts<-c(1,2,3); 
# subset.cnts<-Subset$byRow(examples$msmsExperimentSet[-1],cnts,row="P04264");
# dim(subset.cnts);
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("byRow","Subset",function(static,frameData, subset,row,verbose=FALSE,...) {
	subsetByRowInternal<-function(dataFrame, subset,row,no.case,verbose=FALSE) {
		if (verbose)
			cat("extracting subset by row...\n");

		dataFrame<-as.data.frame(dataFrame,stringsAsFactors=FALSE);
		subset<-as.data.frame(subset);
		subset<-as.character(as.matrix(subset));
		entries<-dataFrame[row,];

		subset<-intersect(entries,subset);
		indexes<-which(entries %in% subset);
		commons<-dataFrame[,indexes];
		return(commons);
	}

	if(!inherits(frameData,"list"))
		return(subsetByRowInternal(frameData,subset,row,verbose));

	res<-list();
	for (name in names(frameData)){
		if (verbose)
			cat("frame",name,": ");
		res[[name]]<-subsetByRowInternal(frameData[[name]],subset,row,verbose);
	}
	return(res);

},static=TRUE)



###########################################################################/**
# @RdocMethod byColumn
# 
# @title "# Extract subset of rows from a data frame or a list of data frames
# by intersecting on a particular column"
#
# \description{
# @get "title".
# This function outputs a data frame which set of rows is a product of intersecting on a particular column
# of the imput data frame(s) rows with a given set of names (subset).
# }
# 
# @synopsis
#
# \arguments{
# \item{frameData}{Input @data.frame or a @list of data frames.}
# \item{subset}{@character @vector of names partially intersecting with given @data.frame 
# column and defining the set on which merging is to be performed.}
# \item{column}{The column on which the intersection is to be performed.}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{
# A @data.frame or list of data frames which row set is a product of
# intersecting of a particular column with a (partially intersecting) subset.
# }
#
# \examples{
# commonSamples<-intersect(colnames(examples$msmsExperimentSet),
#		colnames(examples$msmsExperimentSet));
# mrna.subset<-Subset$byColNames(examples$mrnaExperimentSet,
#		commonSamples,column=1);
# mrna.subset[1:10,1:5];
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("byColumn","Subset",function(static,frameData, subset,column,verbose=FALSE,...) {
	subsetByColumnInternal<-function(dataFrame, subset,column,no.case,verbose=FALSE) {
		if (verbose)
			cat("extracting subset by column...\n");

		dataFrame<-as.data.frame(dataFrame,stringsAsFactors=FALSE);
		subset<-as.data.frame(subset);
		subset<-as.character(as.matrix(subset));
		entries<-dataFrame[,column];

		subset<-intersect(entries,subset);

		indexes<-which(entries %in% subset);
		commons<-dataFrame[indexes,];
		return(as.data.frame(commons,stringsAsFactors=FALSE));
	}

	if(!inherits(frameData,"list"))
		return(subsetByColumnInternal(frameData,subset,column,verbose));

	res<-list();
	for (name in names(frameData)){
		if (verbose)
			cat("frame",name,": ");
		res[[name]]<-subsetByColumnInternal(frameData[[name]],subset,column,verbose);
	}
	return(res);

},static=TRUE)


