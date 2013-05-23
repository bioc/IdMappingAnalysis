%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/jointIdMap.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{JointIdMap}
\docType{class}
\alias{JointIdMap}

 \encoding{latin1}

 \title{The Joint ID Map class}

 \description{
 Package:  IdMappingAnalysis \cr
\bold{Class JointIdMap}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[IdMappingAnalysis]{IdMapBase}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{JointIdMap}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{JointIdMap}\cr
extends \link[IdMappingAnalysis]{IdMapBase}\cr



 JointIdMap is an object encapsulating a \code{\link[base]{data.frame}}
 containing the primary ID set in a first column
 while the rest of columns containing the sets of secondary IDs, each column
 corresponding  to a particular Id Map, keeping all Id Maps properly aligned
 }

 \usage{
JointIdMap(idMapList=list(), primaryIDs=NULL, name="", verbose=FALSE, ...)
}

 \arguments{
 \item{idMapList}{The \code{\link[base]{list}} of ID Maps on which the JointData is constructed.}
 \item{primaryIDs}{The optional \code{\link[base]{character}} vector of primary IDs on which an additional
 intersection and reordering are performed.}
 \item{name}{The optional name of a given JointIdMap object. Default is ''}
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is @FASLE.}
 \item{...}{Not used}
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[IdMappingAnalysis:as.data.frame.JointIdMap]{as.data.frame}} \tab Retrieve a data frame encapsulated within the given JointIdMap object.\cr
 \tab \code{\link[IdMappingAnalysis:diffCounts.plot.JointIdMap]{diffCounts.plot}} \tab Interactive wrapper for IdMapDiffCounts$plot.\cr
 \tab \code{\link[IdMappingAnalysis:ecdf.plot.JointIdMap]{ecdf.plot}} \tab Interactive wrapper for IdMapCounts$plot.\cr
 \tab \code{\link[IdMappingAnalysis:getCounts.JointIdMap]{getCounts}} \tab Create an IdMapCounts object.\cr
 \tab \code{\link[IdMappingAnalysis:getDiff.JointIdMap]{getDiff}} \tab Create an IdMapDiff object.\cr
 \tab \code{\link[IdMappingAnalysis:getIdMapList.JointIdMap]{getIdMapList}} \tab  Create an Id Map list from a JointIdMap object.\cr
 \tab \code{\link[IdMappingAnalysis:getMapNames.JointIdMap]{getMapNames}} \tab Get the names of IdMap objects encapsulated within the given JointIdMap object.\cr
 \tab \code{\link[IdMappingAnalysis:getMatchInfo.JointIdMap]{getMatchInfo}} \tab Get match table(s) for a given set of primary IDs.\cr
 \tab \code{\link[IdMappingAnalysis:getUnionIdMap.JointIdMap]{getUnionIdMap}} \tab  Create a union IdMap.\cr
}


 \bold{Methods inherited from IdMapBase}:\cr
[, aligned, as.data.frame, dim, dimnames, getName, primaryIDs, primaryKey, secondaryKey

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \examples{
 jointIdMap<-JointIdMap(examples$identDfList);

 jointIdMap$primaryKey();
 jointIdMap$secondaryKey();

 jointIdMap[1:10,];
 }
 \author{Roger Day,Alex Lisovich}
\keyword{classes}
