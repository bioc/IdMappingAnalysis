%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  corrData.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{CorrData}
\docType{class}
\alias{CorrData}

 \encoding{latin1}

 \title{CorrData class}

 \description{
  Package:   \cr
\bold{Class CorrData}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{CorrData}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{CorrData}\cr
extends \link[R.oo]{Object}\cr



 CorrData object stores the pair of experiments on which the correlation related processing is performed
 (MS/MS and mRNA for example) in such a way that two experiments are aligned by experiment names
 and by the primary keys ensuring the fast correlations.
 Typically, the primary ID of the ID Map set under consideration is a primary key for a first experiment,
 and the secondary ID if the ID Map set is a primary key for a second experiment.
 The alignment of two experiments by primary keys is guaranteed by using the unique pairs object
 to produce a matching pair of primary keys on which both experiments are ordered.
 Represented by a list of two elements with names corresponding to the primary and secondary IDs
 of the unique pairs ('acc' and 'probeset' for example), each element containing a data frame with primary
 or secondary IDs in the first column while the rest of columns contain the experiment data.
 The names of the data columns in both data frames are identical and correspond to the sample IDs.
 The match of sample IDs and an alignment by primary/secondary IDs is ensured by the proper processing
 during the object creation.
 }

 \usage{CorrData(uniquePairs=NULL, expSet1=NULL, expSet2=NULL, verbose=FALSE, ...)}

 \arguments{
 \item{uniquePairs}{ UniquePairs object or a list of such objects
 on which a single or a list of CorrData objects is constructed.}
 \item{expSet1}{ a first ExperimentSet object with primary IDs corresponding (partially intersecting)
 with the content the first column of UniquePairs (uniquePairsData) object.}
 \item{expSet2}{ a second ExperimentSet object with primary IDs corresponding (partially intersecting)
 with the content the second column of UniquePairs (uniquePairsData) object.}
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used.}
 }

 \examples{
 corrData<-CorrData(examples$uniquePairs,
		examples$msmsExperimentSet,examples$mrnaExperimentSet,verbose=TRUE);
 class(corrData);
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[IdMappingAnalysis:as.MultiSet.CorrData]{as.MultiSet}} \tab Convert CorrData object into MultiSet object.\cr
 \tab \code{\link[IdMappingAnalysis:getExperimentSet.CorrData]{getExperimentSet}} \tab Get experiment set data frame for a given modality.\cr
 \tab \code{\link[IdMappingAnalysis:getSampleNames.CorrData]{getSampleNames}} \tab Get experiment sample names.\cr
 \tab \code{\link[IdMappingAnalysis:getUniquePairs.CorrData]{getUniquePairs}} \tab Extract unique pairs from the CorrData object.\cr
 \tab \code{\link[IdMappingAnalysis:interactive.plot.CorrData]{interactive.plot}} \tab Draw a scatterplot of experiment data interactively.\cr
 \tab \code{\link[IdMappingAnalysis:plot.CorrData]{plot}} \tab Scatterplot of experiment data.\cr
 \tab \code{\link[IdMappingAnalysis:primaryKey.CorrData]{primaryKey}} \tab Retrieves a primary key for a given CorrData object.\cr
 \tab \code{\link[IdMappingAnalysis:secondaryKey.CorrData]{secondaryKey}} \tab Retrieves a secondary key for a given CorrData object.\cr
}


 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \author{Alex Lisovich, Roger Day}
\keyword{classes}
