%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/idMapDiff.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{IdMapDiff}
\docType{class}
\alias{IdMapDiff}

 \encoding{latin1}

 \title{The IdMapDiff class}

 \description{
  Package:  IdMappingAnalysis \cr
\bold{Class IdMapDiff}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[IdMappingAnalysis]{IdMapBase}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{IdMapDiff}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{IdMapDiff}\cr
extends \link[IdMappingAnalysis]{IdMapBase}\cr



 IdMapDiff constructor implements most time consuming step in comparing
 two DBs and the structure itself stores the results in a compact form.
 The IdMapDiff object encapsultates a \code{\link[base]{data.frame}} the first column
 of which contains the primary IDs and the rest of columns contain a disjoint
 representation of the ID Map pair in the form of 3 columns <A-A*B,A*B,B-A*B>,
 where A and B are secondary ID lists for ID Maps A and B.
 This class is separated from the IdMapDiffCounts
 in anticipation of being used by various processing pipelines in a future.
 }

 \usage{
IdMapDiff(idMap1=NULL, idMap2=NULL, pairNames=c("First", "Second"), verbose=FALSE, ...)
}

 \arguments{
 \item{idMap1}{The first ID Map object on which IdMapDiff object is constructed.}
 \item{idMap2}{The second ID Map object on which IdMapDiff object is constructed.}
 \item{pairNames}{The character vector of length 2 representing the names of the ID Map pair.
 Default is c('First','Second').}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages.}
 \item{...}{Not used.}
 }

 \examples{
 #get primary IDs from an msms experiment set
 IDs<-IdMapBase$primaryIDs(examples$msmsExperimentSet);

 #create JointIdMap object aligned by primaryIDs
 jointIdMap<-JointIdMap(examples$identDfList,primaryIDs=IDs);

 # get IdMap list aligned of two ID maps aligned by primaryIDs
 idMaps<-jointIdMap$getIdMapList(verbose=TRUE);

 #create IdMapDiff object
 diffs<-IdMapDiff(idMaps[["NetAffx_F"]],idMaps[["DAVID_Q"]]);
 diffs[1:10,];

 # create IdMapDiff object directly from JointIdMap
 diffs<-jointIdMap$getDiff("NetAffx_F","DAVID_Q",verbose=TRUE);
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\emph{No methods defined}.


 \bold{Methods inherited from IdMapBase}:\cr
[, aligned, as.data.frame, dim, dimnames, getName, primaryIDs, primaryKey, secondaryKey

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \author{Alex Lisovich, Roger Day}
\keyword{classes}
