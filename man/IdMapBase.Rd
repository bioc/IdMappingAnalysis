%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  idMapBase.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{IdMapBase}
\docType{class}
\alias{IdMapBase}

 \encoding{latin1}

 \title{The ID Map base class}

 \description{
  Package:   \cr
\bold{Class IdMapBase}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{IdMapBase}\cr

\bold{Directly known subclasses:}\cr
\link{Bootstrap}, \link{Corr}, \link{IdMap}, \link{IdMapCounts}, \link{IdMapDiff}, \link{IdMapDiffCounts}, \link{JointIdMap}, \link{JointUniquePairs}, \link{UniquePairs}\cr

public static class \bold{IdMapBase}\cr
extends \link[R.oo]{Object}\cr



 IdMapBase is an abstract object encapsultating a data frame
 with at least two columns, the first one (primary) containing
 character string s identifying the ID under consideration
 (unprot accessions ID or acc, Entrez Gene ID etc) and the rest of columns containing
 the variousinformation associated with a given primary ID for a
 particular DB service.
 }

 \usage{IdMapBase(DF=NULL, name="", primaryKey=NULL, secondaryKey=NULL, ...)}

 \arguments{
 \item{DF}{A \code{\link[base]{data.frame}} consisting of two columns (primary and secondary IDs)
 from which the IdMap object is to be created.}
 \item{name}{A \code{\link[base]{character}} string representing the name of the given IdMap object. Default is ''}
 \item{primaryKey}{The primary identifier type from which the ID conversion is performed.
 If \code{\link[base]{NULL}} (default) then the input data frame first column name is used
 and if it is not available defaults to 'From'.}
 \item{secondaryKey}{ The secondary identifier type to which conversion is performed. Default is \code{\link[base]{NULL}}.}
 \item{...}{Not used.}
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[IdMappingAnalysis:000-LB-.IdMapBase]{[}} \tab Access the elements of a data frame encapsulated within the given IdMapBase object using indexation.\cr
 \tab \code{\link[IdMappingAnalysis:aligned.IdMapBase]{aligned}} \tab Checks if two IdMapBase objects match on column names and primary ID set.\cr
 \tab \code{\link[IdMappingAnalysis:as.data.frame.IdMapBase]{as.data.frame}} \tab Retrieves a data frame encapsulated within the given IdMapBase object.\cr
 \tab \code{\link[IdMappingAnalysis:dim.IdMapBase]{dim}} \tab Retrieves dimensions of data frame encapsulated within the given IdMapBase object.\cr
 \tab \code{\link[IdMappingAnalysis:dimnames.IdMapBase]{dimnames}} \tab Retrieve or set the dimnames of data frame encapsulated within the given IdMapBase object.\cr
 \tab \code{\link[IdMappingAnalysis:getName.IdMapBase]{getName}} \tab Get the name a given IdMapBase object.\cr
 \tab \code{\link[IdMappingAnalysis:primaryIDs.IdMapBase]{primaryIDs}} \tab Retrieves the primary IDs for a given IdMapBase object.\cr
 \tab \code{\link[IdMappingAnalysis:primaryKey.IdMapBase]{primaryKey}} \tab Retrieves a primary key for a given IdMapBase object.\cr
 \tab \code{\link[IdMappingAnalysis:secondaryKey.IdMapBase]{secondaryKey}} \tab Retrieves a secondary key for a given IdMapBase object.\cr
}


 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \examples{
 DF<-array(0,dim=c(5,2));
 obj<-IdMapBase(DF,primaryKey="primary",secondaryKey="secondary");
 }

 \author{Alex Lisovich, Roger Day}
\keyword{classes}
