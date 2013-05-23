%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/idMap.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{IdMap}
\docType{class}
\alias{IdMap}

 \encoding{latin1}

 \title{The ID Map class}

 \description{
  Package:  IdMappingAnalysis \cr
\bold{Class IdMap}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[IdMappingAnalysis]{IdMapBase}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{IdMap}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{IdMap}\cr
extends \link[IdMappingAnalysis]{IdMapBase}\cr



 IdMap is an object encapsultating a data frame
 with two columns (Primary ID and Secondary ID)
 where primaryID is a character string uniquely identifying the ID under consideration
 (unprot accessions ID or acc, Entrez Gene ID etc) and the Secondary ID is a
 comma separated list of secondary IDs associated with a given primary ID for a
 particular DB service. The analysis typically starts from obtaining a set of ID Maps
 (from the various DB services) which are not assumed to have the same number
 of rows or the same set of primary IDs. The process of alignment of this ID Maps
 is performed within the JointIdMap
 }

 \usage{
IdMap(DF=NULL, name="", primaryKey=colnames(DF)[1], secondaryKey=colnames(DF)[2], ...)
}

 \arguments{
 \item{DF}{A \code{\link[base]{data.frame}} consisting of two columns (primary and secondary IDs) from which the IdMap object is to be created.}
 \item{name}{A \code{\link[base]{character}} string representing the name of the given IdMap object. Default is ''}
 \item{primaryKey}{The name of the primary (first) column in an ID Map.
 If missing then the input data frame first column name is used and if it is not available defaults to 'From'.}
 \item{secondaryKey}{ The name of secondary (second) column in an ID Map.
 If missing then the input data frame second column name is used and if it is not available defaults to 'To'.}
 \item{...}{Not used.}
 }

 \examples{
 obj<-IdMap(examples$identDfList[[2]]);
 obj$primaryKey();
 obj$secondaryKey();
 }
 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{as} \tab  -\cr
 \tab \code{\link[IdMappingAnalysis:as.UniquePairs.IdMap]{as.UniquePairs}} \tab Create a UniquePairs object from a given IdMap object.\cr
 \tab \code{\link[IdMappingAnalysis:as.list.IdMap]{as.list}} \tab Coerce an object or a list of compatible object
.\cr
 \tab \code{\link[IdMappingAnalysis:getCounts.IdMap]{getCounts}} \tab Compute the count of secondaryIDs for each primary ID.\cr
 \tab \code{\link[IdMappingAnalysis:merge.IdMap]{merge}} \tab Merge the IdMap object with a second IdMap object or a list of IdMap objects.\cr
 \tab \code{\link[IdMappingAnalysis:swapKeys.IdMap]{swapKeys}} \tab Swap the primary and secondary key columns.\cr
}


 \bold{Methods inherited from IdMapBase}:\cr
[, aligned, as.data.frame, dim, dimnames, getName, primaryIDs, primaryKey, secondaryKey

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \author{Alex Lisovich, Roger Day}
\keyword{classes}
