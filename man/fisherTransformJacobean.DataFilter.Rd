%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/dataFilter.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{DataFilter$fisherTransformJacobean}
\alias{DataFilter$fisherTransformJacobean}
\alias{fisherTransformJacobean.DataFilter}
\alias{DataFilter.fisherTransformJacobean}
\alias{fisherTransformJacobean,DataFilter-method}

 \title{Compute the Fisher transform Jacobean}
 \description{Compute the Fisher transform Jacobean.}

 \usage{
## Static method (use this):
## DataFilter$fisherTransformJacobean(x, ...)

## Don't use the below:
\method{fisherTransformJacobean}{DataFilter}(static, x, ...)
}

 \arguments{
 \item{x}{Input \code{\link[base]{numeric}} \code{\link[base]{vector}}.}
 \item{...}{Not used}
 }

 \value{
 Fisher transform jacobean.
 }

 \examples{\dontrun{
 J<-DataFilter$fisherTransform.Jacobean(x);
 }}

 \seealso{For more information see \code{\link{DataFilter}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
