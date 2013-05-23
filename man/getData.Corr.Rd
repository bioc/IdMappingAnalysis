%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/corr.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getData.Corr}
\alias{getData.Corr}
\alias{Corr.getData}
\alias{getData,Corr-method}

 \title{Extract correlation results from the Corr object}

 \description{Extract correlation results from the Corr object.}

 \usage{
\method{getData}{Corr}(this, ...)
}

 \arguments{
 \item{...}{Not used}
 }

 \value{
 \code{\link[base]{numeric}} vector of correlation results.
 }

 \examples{
 dat<-examples$corr$getData();
 dat[1:20];
 }

 \seealso{For more information see \code{\link{Corr}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
