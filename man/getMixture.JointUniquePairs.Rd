%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/jointUniquePairs.extensions.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getMixture.JointUniquePairs}
\alias{getMixture.JointUniquePairs}
\alias{JointUniquePairs.getMixture}
\alias{getMixture,JointUniquePairs-method}

 \title{Extract mixture model object from  JointUniquePairs and Corr objects}

 \description{
 Extract mixture model object from  UniquePairsMatch and Corr objects.
 Computes mixture model on Corr object optionally subsetting the Corr object
 on a list of ID Maps from the JointUniquePairs object.
 }

 \usage{
\method{getMixture}{JointUniquePairs}(this, corr, groups=NULL, full.group=FALSE, G=c(1:5), verbose=FALSE, ...)
}

 \arguments{
 \item{corr}{Corr object.}
 \item{groups}{Optional \code{\link[base]{list}} of DB names from the JointUniquePairs object defining
 the set of  match groups to be used for retrieving the corresponding Mixture object.}
 \item{full.group}{ If@ TRUE, uses Corr subset to compute the mixture model or uses the
 original Corr otherwise. Default is \code{\link[base:logical]{FALSE}}.}
 \item{G}{ the number of mixture model components. If a numerical vector,
 the optimal mixture model is computed. Default is c(1:5).}
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{
 the Mixture object.
 }

 \examples{
 # create and plot mixture  (automatically determining the optimal number of components)
 #for a given DB subset treating the subset as a full group
 mixture.subset <- examples$jointUniquePairs$getMixture(examples$corr,
				groups=c("NetAffx_Q", "DAVID_Q", "EnVision_Q"),
 				full.group=TRUE, G=c(1:5), verbose=TRUE);
 mixture.subset$plot();
 }
 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
