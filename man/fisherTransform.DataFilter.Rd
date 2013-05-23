%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/dataFilter.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{DataFilter$fisherTransform}
\alias{DataFilter$fisherTransform}
\alias{fisherTransform.DataFilter}
\alias{DataFilter.fisherTransform}
\alias{fisherTransform,DataFilter-method}

 \title{Compute the Fisher transform}
 \description{Compute the Fisher transform.}

 \usage{
## Static method (use this):
## DataFilter$fisherTransform(x, ...)

## Don't use the below:
\method{fisherTransform}{DataFilter}(static, x, ...)
}

 \arguments{
 \item{x}{Input \code{\link[base]{numeric}} \code{\link[base]{vector}}.}
 \item{...}{Not used}
 }

 \value{
 Result of the Fisher transform.
 }

 \examples{\dontrun{
 z<-Filter$fisherTransform(x);
 }}

 \seealso{For more information see \code{\link{DataFilter}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
