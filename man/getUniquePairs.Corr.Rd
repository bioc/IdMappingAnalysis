%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/corr.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getUniquePairs.Corr}
\alias{getUniquePairs.Corr}
\alias{Corr.getUniquePairs}
\alias{getUniquePairs,Corr-method}

 \title{Extract unique pairs from the Corr object}

 \description{Extract unique pairs from the Corr object.}

 \usage{
\method{getUniquePairs}{Corr}(this, ...)
}

 \arguments{
 \item{...}{Not used}
 }

 \value{UniquePairs object}

 \examples{
 uniquePairs<-examples$corr$getUniquePairs();
 uniquePairs[1:10,];
 }

 \seealso{For more information see \code{\link{Corr}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
