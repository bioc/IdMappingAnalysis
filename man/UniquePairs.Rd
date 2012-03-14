%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  uniquePairs.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{UniquePairs}
\docType{class}
\alias{UniquePairs}

 \encoding{latin1}

 \title{The UniquePairs class}

 \description{
  Package:   \cr
\bold{Class UniquePairs}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{IdMapBase}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{UniquePairs}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{UniquePairs}\cr
extends \link{IdMapBase}\cr



 The alternative representation of an IdMap suitable for performing
 the correlation related processing. Contains a data frame with two columns,
 each row of which represents a unique pair <primary ID, secondary ID>
 where primary ID corresponds to the primaryIDs of an ID Map and secondary ID
 corresponds to a single ID from a list of comma separated secondary IDs within
 the corresponding ID Map. The column names correspond to the primary/secondary
 keys of an Id Map ('acc' and 'probeset' for example)
 }

 \usage{UniquePairs(DF=NULL, name="", primaryKey=colnames(DF)[1], secondaryKey=colnames(DF)[2], ...)}

 \arguments{
 \item{DF}{A \code{\link[base]{data.frame}} consisting of two columns (primary and secondary IDs) from which the UniquePairs object is to be created.}
 \item{name}{A \code{\link[base]{character}} string representing the name of the given UniquePairs object. Default is ''}
 \item{primaryKey}{The name of the primary (first) column of a \code{\link[base]{data.frame}} encapsulated within the UniquePairs object.
 If missing then the input data frame first column name is used and if it is not available defaults to 'From'.}
 \item{secondaryKey}{ The name of secondary (second) column in an ID Map.
 If missing then the input data frame second column name is used and if it is not available defaults to 'To'.}
 \item{...}{Not used.}
 }

 \examples{
 DF<-matrix(
  c("P25685","200664_s_at",
    "P25685","200666_s_at",
    "Q6ZV71","205208_at",
    "Q6ZV71","215798_at",
    "P05164", "203948_s_at"
  ),ncol=2,nrow=5,byrow=TRUE);
 colnames(DF)<-c("Uniprot","Affy");

 uniquePairs<-UniquePairs(DF);
}

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[IdMappingAnalysis:as.IdMap.UniquePairs]{as.IdMap}} \tab Convert the UniquePairs object into the IdMap object.\cr
 \tab \code{as} \tab  -\cr
 \tab \code{\link[IdMappingAnalysis:create.UniquePairs]{create}} \tab Create a UniquePairs object from a single IdMap or a list of IdMap objects.\cr
 \tab \code{\link[IdMappingAnalysis:equals.UniquePairs]{equals}} \tab Check  if two unique pairs data structures are identical.\cr
 \tab \code{\link[IdMappingAnalysis:getMatch.UniquePairs]{getMatch}} \tab Get the logical vector of pair matches of the given UniquePairs object
 in other UniquePair object(s).\cr
 \tab \code{\link[IdMappingAnalysis:swapKeys.UniquePairs]{swapKeys}} \tab Swap the primary and secondary key columns.\cr
 \tab \code{\link[IdMappingAnalysis:unique.UniquePairs]{unique}} \tab Extract unique elements.\cr
}


 \bold{Methods inherited from IdMapBase}:\cr
[, aligned, as.data.frame, dim, dimnames, getName, primaryIDs, primaryKey, secondaryKey

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \author{Alex Lisovich, Roger Day}
\keyword{classes}
