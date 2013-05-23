

###########################################################################/**
# @RdocClass IdMapBase
# \encoding{latin1}
#
# @title "The ID Map base class"
#
# \description{
#  @classhierarchy
#
# IdMapBase is an abstract object encapsultating a data frame
# with at least two columns, the first one (primary) containing 
# character string s identifying the ID under consideration 
# (unprot accessions ID or acc, Entrez Gene ID etc) and the rest of columns containing
# the variousinformation associated with a given primary ID for a 
# particular DB service. 
# }
# 
# @synopsis
#
# \arguments{
# \item{DF}{A @data.frame consisting of two columns (primary and secondary IDs)
# from which the IdMap object is to be created.}
# \item{name}{A @character string representing the name of the given IdMap object. Default is ''}
# \item{primaryKey}{The primary identifier type from which the ID conversion is performed.
# If @NULL (default) then the input data frame first column name is used
# and if it is not available defaults to 'From'.} 
# \item{secondaryKey}{ The secondary identifier type to which conversion is performed. Default is @NULL.}
# \item{...}{Not used.}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \examples{
# DF<-array(0,dim=c(5,2));
# obj<-IdMapBase(DF,primaryKey="primary",secondaryKey="secondary");
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setConstructorS3("IdMapBase",function(DF=NULL,name="",primaryKey=NULL,secondaryKey=NULL,...){
	if(!is.null(DF)){
		if(!(inherits(DF,"matrix") || inherits(DF,"data.frame")))
			throw("IdMapBase constructor: Invalid input type (should be matrix or data frame)");

		if(is.null(primaryKey))
			primaryKey=colnames(DF)[1];

		if(is.null(primaryKey))
			primaryKey<-"From";

		DF<-as.data.frame(DF);
		colnames(DF)[1]<-primaryKey;
	}

	extend(Object(),"IdMapBase",
		.df=DF,
		.name=name,
    .primaryKey=primaryKey,
	  .secondaryKey=secondaryKey
	);
})

#' @export primaryKey.IdMapBase
###########################################################################/**
# @RdocMethod primaryKey
# 
# @title "Retrieves a primary key for a given IdMapBase object"
# \description{@get "title".
# The method can be used as object specific as well as static. In the latter case 
# the method can accept a @data.frame using IdMapBase$primaryKey(<data.frame>) signature
# returning the name of the <data.frame> first column. 
# }
# 
# @synopsis
#
# \arguments{\item{...}{Not used}}
# \value{A @character string representing primary key for given IdMapBase object}
#
# \examples{
# obj<-IdMapBase(examples$identDfList[[1]]);
# primaryKey.IdMapBase(obj);
# }
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("primaryKey", export=TRUE, "IdMapBase",function(this,DF=NULL,...){
	if(is.null(this$.df))
		return(colnames(DF)[1])
	else
		return(colnames(this$.df)[1]);
})

#' @export secondaryKey.IdMapBase
###########################################################################/**
# @RdocMethod secondaryKey
# 
# @title "Retrieves a secondary key for a given IdMapBase object"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{\item{...}{Not used}}
# \value{A @character string representing secondary key for given IdMapBase object}
#
# \examples{
# obj<-IdMapBase(examples$identDfList[[1]]);
# primaryKey.IdMapBase(obj);
# }
## \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("secondaryKey","IdMapBase",function(this,...){
	return(this$.secondaryKey);
})

#' @export primaryIDs.IdMapBase
###########################################################################/**
# @RdocMethod primaryIDs
# 
# @title "Retrieves the primary IDs for a given IdMapBase object"
# \description{@get "title".
# The method can be used as object specific as well as static. In the latter case 
# the method can accept a @data.frame using InMpaBase$primaryIDs(<data.frame>) signature
# returning the <data.frame> first column. 
#}
# 
# @synopsis
#
# \arguments{\item{...}{Not used}}
# \value{A @character vector of primary IDs for given IdMapBase object}
#
# \examples{
# obj<-IdMapBase(examples$identDfList[[1]]);
# obj$primaryIDs()[1:20];
# }
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("primaryIDs","IdMapBase",function(this,DF=NULL,...){
	if(is.null(this$.df))
		return(DF[,1])
	else
		return(this$.df[,1]);
})

#' @export getName.IdMapBase
###########################################################################/**
# @RdocMethod getName
# 
# @title "Get the name a given IdMapBase object"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{\item{...}{Not used}}
# \value{A @character string representing the name of a given IdMapBase object}
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getName","IdMapBase",function(this,...){
	return(this$.name);
})


#' @export as.data.frame.IdMapBase
###########################################################################/**
# @RdocMethod as.data.frame
# 
# @title "Retrieves a data frame encapsulated within the given IdMapBase object"
# \description{
# @get "title". The method can be used as object specific as well as static. 
# In the latter case the method can coerce to @data.frame either IdMapBase or ExpressionSet
# derived object by using IdMapBase$as.data.frame(object) call signature.
# }
# 
# @synopsis
#
# \arguments{
# \item{row.names}{Not used}
# \item{optional}{Not used}
# \item{...}{Not used}
# }
# \value{
# A @data.frame encapsulated within the given IdMapBase object or a @data.frame
# to which another IdMapBase or ExpressionSet derived object is coerced.}
#
# \examples{
# obj<-IdMapBase(examples$msmsExperimentSet);
# DF<-as.data.frame(obj);
# DF[1:20,1:5];
#
# #convert ExpressionSet to data.frame
# exprSet<-new("ExpressionSet",
#    exprs=matrix(runif(1000), nrow=100, ncol=10));
# DF<-IdMapBase$as.data.frame(exprSet);
# DF[1:10,1:5];
# }
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("as.data.frame","IdMapBase",function(x,row.names = NULL, optional = FALSE,...){
	if(!is.null(x$.df)){
		res<-x$.df;
		attr(res,"name")<-getName(x);
	} else {
		if(!is.null(row.names)&& !is.character(row.names)){
			arg<-row.names;
		} else if (!is.logical(optional)){
			arg<-optional;
		} else if(length(...)>0){
			arg<-list(...)[[1]];
		} else {
			throw("IdMapBase::as.data.frame: missing object to coerce from");
		}

		if(inherits(arg,"data.frame")){
			res<-arg;
		} else if(inherits(arg,"IdMapBase")){
			res<-arg$.df;
			attr(res,"name")<-getName(arg);
		} else if (inherits(arg,"ExpressionSet")){
			res<-as(arg,"data.frame");
		} else {
			msg=paste("IdMapBase::as.data.frame: cannot coerce",
					"an object of class",class(arg),"to data.frame");
			throw(msg);
		}
	}

	return(res);
},createGeneric=FALSE)


#' @export dim.IdMapBase
###########################################################################/**
# @RdocMethod dim
# 
# @title "Retrieves dimensions of data frame encapsulated within the given IdMapBase object"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{\item{...}{Not used}}
# \value{Dimensions of a @data.frame encapsulated within the given IdMapBase object}
#
# \examples{
# obj<-IdMapBase(examples$mrnaExperimentSet);
# dim(obj);
# }
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setMethodS3("dim","IdMapBase",function(x){
	return(dim(x$.df));
},appendVarArgs=FALSE)


#' @export dimnames.IdMapBase
###########################################################################/**
# @RdocMethod dimnames
# 
# @title "Retrieve or set the dimnames of data frame encapsulated within the given IdMapBase object"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{\item{...}{Not used}}
# \value{Dimnames of a @data.frame encapsulated within the given IdMapBase object}
#
# \examples{
# obj<-IdMapBase(examples$mrnaExperimentSet);
# rownames(obj)[1:10];
# colnames(obj)[1:5];
# }
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("dimnames","IdMapBase",function(x){
	return(dimnames(x$.df));
},appendVarArgs=FALSE)

#' @export "[.IdMapBase"
###########################################################################/**
# @RdocMethod [
# 
# @title "Access the elements of a data frame encapsulated within the given IdMapBase object using indexation"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{\item{...}{Indexes of elements of a @data.frame encapsulated within the given object}}
# \value{Elements of a @data.frame encapsulated within the given object}
#
# \examples{
# obj<-IdMapBase(examples$msmsExperimentSet);
# obj[5:10,2:6];
# }
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setMethodS3("[","IdMapBase",function(this,...){
	return(this$.df[...]);
})



###########################################################################/**
# @RdocMethod aligned
#
# @title "Checks if two IdMapBase objects match on column names and primary ID set"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{other}{ The second ID Map object to check the matching against.}
# \item{...}{ Not used.}
# }
# \value{@TRUE if two IdMapBase objects are matching, otherwise @FALSE.}
#
# \examples{
# obj1<-IdMapBase(examples$identDfList[[1]]);
# obj2<-IdMapBase(examples$identDfList[[2]]);
# aligned(obj1,obj2);
#
# obj3<-IdMapBase(obj1[1:10,]);
# DF<-cbind(obj1[1:10,1],obj2[1:10,2]);
# colnames(DF)<-colnames(obj3);
# obj4<-IdMapBase(DF);
# aligned(obj3,obj4);
# }
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("aligned","IdMapBase",function(this,other,...){
	
	return(
		inherits(other,"IdMapBase") &&
		setequal(colnames(this$.df),colnames(other$.df)) &&
		setequal(this$.df[,1],other$.df[,1])
	)
})



