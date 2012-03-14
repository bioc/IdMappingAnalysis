%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  idMapCounts.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{IdMapCounts}
\docType{class}
\alias{IdMapCounts}

 \encoding{latin1}

 \title{The IdMapCounts class}

 \description{
  Package:   \cr
\bold{Class IdMapCounts}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{IdMapBase}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{IdMapCounts}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{IdMapCounts}\cr
extends \link{IdMapBase}\cr



 An IdMapCounts object enapsulates a \code{\link[base]{data.frame}}
 where the first column contains the primary ID set
 while the rest of columns contain the counts
 of secondary IDs for each Id Map in a given idMapList object,
 one column per ID Map, each ID Map related column
 having a name representing the given DB data source (i.e. 'NetAffx', 'EnVision' etc.)
 The constructor creates the IdMapCounts object from the list of ID Maps aligned by
 the primary IDs and primary and secondary keys.
 The easest way to obtain the list of properly aligned IdMap objects is to create a JointIdMap
 object from a set of un-aligned ID maps and then invoke the getIdMapList() method on
 this object. The IdMapCounts object can also be created directly from JointIdMap object
 by using the JointIdMap.$getCounts() method.
 }

 \usage{IdMapCounts(idMapList=NULL, verbose=FALSE, ...)}

 \arguments{
 \item{idMapList}{The \code{\link[base]{list}} of ID Maps aligned on primary IDs.}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages.Default is \code{\link[base:logical]{FALSE}}}
 \item{...}{Not used.}
 }

 \examples{
 idMaps<-IdMap$as.list(examples$identDfList[[1]]);
 cnts<-IdMapCounts(IdMap(examples$identDfList[[1]]));
 cnts[1:20,];

 #create IdMapCounts object from aligned IdMap list.
 jointIdMap<-JointIdMap(examples$identDfList);
 idMaps<-jointIdMap$getIdMapList(verbose=TRUE);
 cnts<-IdMapCounts(idMaps);
 cnts[1:20,];

 #create IdMapCounts object directly from the JointIdMap object
 jointIdMap<-JointIdMap(examples$identDfList);
 cnts<-jointIdMap$getCounts(verbose=TRUE);
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[IdMappingAnalysis:getStats.IdMapCounts]{getStats}} \tab Retrieves a set of unique counts of secondary IDs.\cr
 \tab \code{\link[IdMappingAnalysis:plot.IdMapCounts]{plot}} \tab Compute and plot the (inversed) ecdf for each ID Map count entry within the IdMapCounts object.\cr
}


 \bold{Methods inherited from IdMapBase}:\cr
[, aligned, as.data.frame, dim, dimnames, getName, primaryIDs, primaryKey, secondaryKey

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \author{Alex Lisovich, Roger Day}
\keyword{classes}
