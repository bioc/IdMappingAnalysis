%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/jointUniquePairs.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{do.glm.JointUniquePairs}
\alias{do.glm.JointUniquePairs}
\alias{JointUniquePairs.do.glm}
\alias{do.glm,JointUniquePairs-method}

 \title{Compute linear regression for the given set of ID Maps}

 \description{
 Compute linear regression for the given set of ID Maps using the UniquePairsMatch object
 and auxiliary data (response.data) from another data object.
 }

 \usage{
\method{do.glm}{JointUniquePairs}(this, response.data, weights=NULL, idMapNames=NULL, ...)
}

 \arguments{
 \item{response.data}{The data on which the regression to be fitted.}
 \item{weights}{An optional vector of 'prior weights' to be used in the fitting process.
 Should be \code{\link[base]{NULL}} or a \code{\link[base]{numeric}} vector.}
 \item{idMapNames}{optional list of ID Map names within the pairsMatch object
 on which the fit to be performed. If NULL (default), all ID Maps within the pairsMatch object will be used.}
 \item{...}{Not used}
 }

 \value{
 The regression model fitting results (an object of class 'glm')
 }

 \examples{
 # perform regression on correlations
 fit<-examples$jointUniquePairs$do.glm(examples$corr$getData(),
		idMapNames=c("DAVID_Q","EnVision_Q","NetAffx_Q"));
 coefficients(summary(fit));
 }

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
