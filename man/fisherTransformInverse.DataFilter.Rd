%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/dataFilter.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{DataFilter$fisherTransformInverse}
\alias{DataFilter$fisherTransformInverse}
\alias{fisherTransformInverse.DataFilter}
\alias{DataFilter.fisherTransformInverse}
\alias{fisherTransformInverse,DataFilter-method}

 \title{Compute the Fisher inversed transform}
 \description{Compute the Fisher inversed transform.}

 \usage{
## Static method (use this):
## DataFilter$fisherTransformInverse(z, ...)

## Don't use the below:
\method{fisherTransformInverse}{DataFilter}(static, z, ...)
}

 \arguments{
 \item{z}{Input \code{\link[base]{numeric}} \code{\link[base]{vector}}.}
 \item{...}{Not used}
 }

 \value{
 Result of the Fisher inversed transform.
 }

 \examples{\dontrun{
 z<-Filter$fisherTransform.inverse(x);
 }}

 \seealso{For more information see \code{\link{DataFilter}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
