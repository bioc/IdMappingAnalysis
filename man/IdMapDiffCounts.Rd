%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  idMapDiffCounts.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{IdMapDiffCounts}
\docType{class}
\alias{IdMapDiffCounts}

 \encoding{latin1}

 \title{The IdMapDiffCounts class}

 \description{
  Package:   \cr
\bold{Class IdMapDiffCounts}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{IdMapBase}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{IdMapDiffCounts}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{IdMapDiffCounts}\cr
extends \link{IdMapBase}\cr



 The IdMapDiffCounts class handles statistics on IdMapDiff object.
 IdMapDiffCounts object encapsulates a data frame with row names corresponding
 to the primary IDs and 6 columns subdivided into pairs <match(TRUE/FALSE),count>
 each pair corresponding to the disjoint events <A-A*B,A*B,B-A*B>, where A and B
 are secondary ID lists for ID Maps A and B from the IdMapDiff object. The 'pairNames' attribute
 of the IdMapDiffCounts contains the names of the source ID Map pair from which the IdMapDiff object was created.
 }

 \usage{IdMapDiffCounts(idMapDiff=NULL, verbose=FALSE, ...)}

 \arguments{
 \item{idMapDiff}{The IdMapDiff on which IdMapDiffCounts is cretated. Default is \code{\link[base]{NULL}}.}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used.}
 }

 \examples{
 #get primary IDs from an msms experiment set
 IDs<-IdMapBase$primaryIDs(examples$msmsExperimentSet);

 #create JointIdMap object aligned by primaryIDs
 jointIdMap<-JointIdMap(examples$identDfList,primaryIDs=IDs);

 #create IdMapDiff object
 diffs<-jointIdMap$getDiff("NetAffx_F","DAVID_Q",verbose=TRUE);

 # create IdMapDiffCounts object
 diffCounts<-IdMapDiffCounts(diffs);
 diffCounts[1:10,];
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[IdMappingAnalysis:getCompoundEvents.IdMapDiffCounts]{getCompoundEvents}} \tab Get compound events.\cr
 \tab \code{\link[IdMappingAnalysis:getCompoundGroups.IdMapDiffCounts]{getCompoundGroups}} \tab Get counts for each compound event in IdMapDiffCounts.\cr
 \tab \code{\link[IdMappingAnalysis:plot.IdMapDiffCounts]{plot}} \tab Produce a fountain plot representing the quantitative relationship of the compound events.\cr
 \tab \code{\link[IdMappingAnalysis:summary.IdMapDiffCounts]{summary}} \tab Get a compaund event counts summary report.\cr
}


 \bold{Methods inherited from IdMapBase}:\cr
[, aligned, as.data.frame, dim, dimnames, getName, primaryIDs, primaryKey, secondaryKey

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \author{Alex Lisovich, Roger Day}
\keyword{classes}
