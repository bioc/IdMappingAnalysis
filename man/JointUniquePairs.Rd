%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/jointUniquePairs.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{JointUniquePairs}
\docType{class}
\alias{JointUniquePairs}

 \encoding{latin1}

 \title{The JointUniquePairs class}

 \description{
  Package:  IdMappingAnalysis \cr
\bold{Class JointUniquePairs}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[IdMappingAnalysis]{IdMapBase}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{JointUniquePairs}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{JointUniquePairs}\cr
extends \link[IdMappingAnalysis]{IdMapBase}\cr



 UniquePairsMatch object encapsultates a data frame the
 first two columns of which contain the unique pairs corresponding to the merge (union)
 of all ID Maps in consideration while the rest of columns contains the match (logical value)
 between the merged unique pairs set and a unique pairs set specific to the particular
 ID Map ('d8', 'enV', 'netAffx' etc), one column per Id Map.
 Used in combination with correlation related data objects (CorrData, Corr, Mixture etc.)
 to aid in on-fly processing related to some classification by a particular match group
 The UniquePairsMatch constructor creates an object from the UniquePairs and an ID Map list
 computing the match (inclusions) for each particular ID Map.
 }

 \usage{
JointUniquePairs(uniquePairs=NULL, idMapList=NULL, name="", verbose=FALSE, ...)
}

 \arguments{
 \item{uniquePairs}{ UniquePairs object on which a UniquePairsMatch is created
 or a \code{\link[base]{data.frame}} complying with the UniquePairs class internal data frame format.
 In case the UniquePairs object is used as a first argument, it's typically obtained
 from the JointIdMap object by invoking JointIdMap$getUnionIdMap())}
 \item{idMapList}{ the list of ID Maps on which the match is performed during the UniquePairsMatch object creation
 The idMapList typically obtained through the call to the JointIdMap.getIdMapList() of the same JointIdMap object
 as for the first argument to ensure that both arguments are properly aligned.}
 \item{name}{A \code{\link[base]{character}} string representing the name of the given IdMap object. Default is ''}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used.}
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[IdMappingAnalysis:boxplot.JointUniquePairs]{boxplot}} \tab  Draw a basic boxplot based on a given JointUniquePairs object and external data.\cr
 \tab \code{\link[IdMappingAnalysis:corr.boxplot.JointUniquePairs]{corr.boxplot}} \tab Boxplot of correlations by match group.\cr
 \tab \code{\link[IdMappingAnalysis:corr.plot.JointUniquePairs]{corr.plot}} \tab Plot the density distributions for a set of correlation objects
.\cr
 \tab \code{\link[IdMappingAnalysis:do.glm.JointUniquePairs]{do.glm}} \tab Compute linear regression for the given set of ID Maps.\cr
 \tab \code{\link[IdMappingAnalysis:getBootstrap.JointUniquePairs]{getBootstrap}} \tab Create Bootstrap object from JointUniquePairs object and two experiment sets.\cr
 \tab \code{\link[IdMappingAnalysis:getCorr.JointUniquePairs]{getCorr}} \tab Extract a set of correlation objects from given JointUniquePairs object
.\cr
 \tab \code{\link[IdMappingAnalysis:getCorrData.JointUniquePairs]{getCorrData}} \tab Create CorrData object from the JointUniquePairs object and two experiment sets.\cr
 \tab \code{\link[IdMappingAnalysis:getCorrDataFrame.JointUniquePairs]{getCorrDataFrame}} \tab Merge JointUniquePairs and Corr objects into a single data frame.\cr
 \tab \code{\link[IdMappingAnalysis:getMapNames.JointUniquePairs]{getMapNames}} \tab Get the names of UniquePairs objects encapsulated within the given JointUniquePairs object.\cr
 \tab \code{\link[IdMappingAnalysis:getMatchInfo.JointUniquePairs]{getMatchInfo}} \tab Get match table(s) for a given set of primary IDs.\cr
 \tab \code{\link[IdMappingAnalysis:getMixture.JointUniquePairs]{getMixture}} \tab Extract mixture model object from  JointUniquePairs and Corr objects.\cr
 \tab \code{\link[IdMappingAnalysis:getUniquePairs.JointUniquePairs]{getUniquePairs}} \tab Extract the unity UniquePairs object from a given JointUniquePairs object.\cr
 \tab \code{\link[IdMappingAnalysis:interactive.corr.boxplot.JointUniquePairs]{interactive.corr.boxplot}} \tab Interactive boxplot of correlations by match group.\cr
 \tab \code{\link[IdMappingAnalysis:interactive.corr.plot.JointUniquePairs]{interactive.corr.plot}} \tab Interactive plot of correlation densities.\cr
 \tab \code{\link[IdMappingAnalysis:interactive.mixture.boxplot.JointUniquePairs]{interactive.mixture.boxplot}} \tab Interactive boxplot of mixture component probabilities by match group.\cr
 \tab \code{\link[IdMappingAnalysis:interactive.mixture.plot.JointUniquePairs]{interactive.mixture.plot}} \tab Interactive plot of mixture model components.\cr
 \tab \code{\link[IdMappingAnalysis:interactive.plot.JointUniquePairs]{interactive.plot}} \tab General purpose JointUniquePairs interactive plot function.\cr
 \tab \code{\link[IdMappingAnalysis:mixture.boxplot.JointUniquePairs]{mixture.boxplot}} \tab Boxplot of a mixture model component by match group.\cr
 \tab \code{\link[IdMappingAnalysis:mixture.plot.JointUniquePairs]{mixture.plot}} \tab Plot the correlation densities of the empirical fit, mixture fit and each
.\cr
 \tab \code{\link[IdMappingAnalysis:subsetCorr.JointUniquePairs]{subsetCorr}} \tab Subset the Corr object.\cr
 \tab \code{\link[IdMappingAnalysis:subsetData.JointUniquePairs]{subsetData}} \tab Subset data on a UniquePairsMatch object.\cr
 \tab \code{\link[IdMappingAnalysis:subsetGroups.JointUniquePairs]{subsetGroups}} \tab Get a JointUniquePairs subset.\cr
}


 \bold{Methods inherited from IdMapBase}:\cr
[, aligned, as.data.frame, dim, dimnames, getName, primaryIDs, primaryKey, secondaryKey

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \examples{
 #create JointIdMap
 jointIdMap<-JointIdMap(examples$identDfList);

 #creaate unique pairs from the union of all IdMaps within JointIdMap
 pairs<-as.UniquePairs(jointIdMap$getUnionIdMap(verbose=TRUE),verbose=TRUE);

 #create JointUniquePairs object
 jointPairs<-JointUniquePairs(pairs,jointIdMap$getIdMapList(),verbose=TRUE);
 jointPairs[1:10,];
 }

 \author{Alex Lisovich, Roger Day}
\keyword{classes}
