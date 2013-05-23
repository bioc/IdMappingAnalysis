%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/jointUniquePairs.plots.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{mixture.plot.JointUniquePairs}
\alias{mixture.plot.JointUniquePairs}
\alias{JointUniquePairs.mixture.plot}
\alias{mixture.plot,JointUniquePairs-method}

 \title{Plot the correlation densities of the empirical fit, mixture fit and each
 of the mixture components for a mixture object derived from JointUniquePairsand Corr objects}

 \description{
 Plot the correlation densities of the empirical fit, mixture fit and each of the mixture components
 for a mixture object derived from UniquePairsMatch  and Corr objects with optional subsetting
 by a group of ID Maps. This is achived by first creating a mixture model from the JointUniquePairs
 and Corr objects with optional subsetting by a group of ID Maps encapsulated within the and then calling the MixtureObject.plot()
 on a resulting Mixture object.
 }

 \usage{
\method{mixture.plot}{JointUniquePairs}(this, corr, idMapNames=NULL, subsetting=FALSE, G=c(1:5), verbose=FALSE, ...)
}

 \arguments{
 \item{corr}{Corr object.}
 \item{idMapNames}{If not \code{\link[base]{NULL}}, defines the subset of ID Maps from JointUniquePairs
 on which the full event group is to be formed. Default is \code{\link[base]{NULL}}.}
 \item{subsetting}{If \code{\link[base:logical]{TRUE}}, uses Corr subset to compute the mixture model or uses
 the original Corr otherwise. Default is \code{\link[base:logical]{FALSE}}.}
 \item{G}{The number of mixture model components. If a \code{\link[base]{numeric}} vector,
 the optimal mixture model is computed. Default is c(1:5).}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Additional graphical parameters}
 }

 \value{
 The Mixture object plotted
 }

 \examples{
 #plot the results of mixture fit for a given DB subset and a Corr object (corr.spearman)
 mixtureSet<-examples$jointUniquePairs$mixture.plot(examples$corr,
                idMapNames=c("NetAffx_Q","DAVID_Q","EnVision_Q"),
	          subsetting=TRUE,G=c(1:5),verbose=TRUE);
 }

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
