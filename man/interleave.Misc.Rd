%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/misc.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Misc$interleave}
\alias{Misc$interleave}
\alias{interleave.Misc}
\alias{Misc.interleave}
\alias{interleave,Misc-method}

 \title{Interleave two matrixes by columns}

 \description{Interleave two matrixes by columns.}

 \usage{
## Static method (use this):
## Misc$interleave(A, B, ...)

## Don't use the below:
\method{interleave}{Misc}(stati, A, B, ...)
}

 \arguments{
 \item{A}{First \code{\link[base]{matrix}}.}
 \item{B}{Second \code{\link[base]{matrix}}.}
 \item{...}{Not used}
 }

 \value{
  \code{\link[base]{matrix}} composed of A and B merged by interleaving of their columns.
 }

 \examples{\dontrun{
 C<-Misc$interleave(A,B);
 }}

 \seealso{For more information see \code{\link{Misc}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
