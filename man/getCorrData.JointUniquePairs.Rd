%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/jointUniquePairs.extensions.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getCorrData.JointUniquePairs}
\alias{getCorrData.JointUniquePairs}
\alias{JointUniquePairs.getCorrData}
\alias{getCorrData,JointUniquePairs-method}

 \title{Create CorrData object from the JointUniquePairs object and two experiment sets}

 \description{
 Creates CorrData  object from JointUniquePairs object and two experiment sets
 optionally subsetting on a list of ID Maps from the UniquePairsMatch object
 }

 \usage{
\method{getCorrData}{JointUniquePairs}(this, expSet1, expSet2, groups=NULL, verbose=FALSE, ...)
}

 \arguments{
 \item{expSet1}{First ExperimentSet object with primary IDs corresponding (partially intersecting)
 with primaryIDs of the given JointUniquePairs object.}
 \item{expSet2}{Second ExperimentSet object with primary IDs corresponding (partially intersecting)
 with secondaryIDs of the given JointUniquePairs object.}
 \item{groups}{If not \code{\link[base]{NULL}}, defines the subset from UniquePairsMatch on which
 the full event group to be formed. Default is \code{\link[base]{NULL}}.}
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{
 the CorrData object
 }

 \examples{
 corrData<-examples$jointUniquePairs$getCorrData(
			examples$msmsExperimentSet,examples$mrnaExperimentSet,
			groups=c("NetAffx_Q", "DAVID_Q", "EnVision_Q"),verbose=TRUE);
 corrData$getSampleNames();
 }

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
