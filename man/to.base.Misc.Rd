%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/misc.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Misc$to.base}
\alias{Misc$to.base}
\alias{to.base.Misc}
\alias{Misc.to.base}
\alias{to.base,Misc-method}

 \title{Convert number to a numeric vector of a given base}

 \description{Convert number to a numeric vector of a given base.}

 \usage{
## Static method (use this):
## Misc$to.base(n, base=2, r=TRUE, ...)

## Don't use the below:
\method{to.base}{Misc}(static, n, base=2, r=TRUE, ...)
}

 \arguments{
 \item{n}{Number to convert.}
 \item{base}{Base to use for a conversion. Default is 2.}
 \item{r}{Order of the output nnumbers If r=TRUE then big-endian
 otherwise small-endian. Default is \code{\link[base:logical]{TRUE}}.}
 \item{...}{Not used}
 }

 \value{
 \code{\link[base]{numeric}} vector of a given base.
 }

 \examples{
 b<-Misc$to.base(25);
 }

 \seealso{For more information see \code{\link{Misc}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
