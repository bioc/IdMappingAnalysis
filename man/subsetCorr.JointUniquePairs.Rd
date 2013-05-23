%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/jointUniquePairs.extensions.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 \name{subsetCorr.JointUniquePairs}
\alias{subsetCorr.JointUniquePairs}
\alias{JointUniquePairs.subsetCorr}
\alias{subsetCorr,JointUniquePairs-method}

 \title{Subset the Corr object}

 \description{
 Create the Corr object using the pait match information from UniquePairsMatch object
 in such a way that the new Corr object contains the data corresponding to the unity (full event group)
 in a JointUniquePairs subset.
 }

 \usage{
\method{subsetCorr}{JointUniquePairs}(this, corr, groups=NULL, verbose=FALSE, ...)
}

 \arguments{
 @param match UniquePairsMatch object
 @param corr Corr object
 @param groups names of ID Maps within the UniquePairsMatch object which should form a full event group.
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{
 The subsetted Corr object
 }

 \examples{
 corr.subset<-examples$jointUniquePairs$subsetCorr(examples$corr,
			groups=c("NetAffx_Q", "DAVID_Q", "EnVision_Q"),verbose=TRUE);
 corr.subset[1:10,];
 }

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
