%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  misc.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{to.binary.logical.Misc}
\alias{to.binary.logical.Misc}
\alias{Misc.to.binary.logical}
\alias{to.binary.logical.Misc}
\alias{to.binary.logical,Misc-method}


 \title{Convert number to a  vector of logicals}

 \description{
 Convert number to a  vector of logicals with predefine length
 }

\usage{Misc$to.binary.logical(n, digits=0, r=TRUE, ...)}

 \arguments{
 \item{n}{Number to convert.}
 \item{digits}{The length of output vector. If digits is less than the length of the output vector,
 the actual length is used. Default is 0.}
 \item{r}{Order of the output nnumbers If r=TRUE then big-endian otherwise small-endian. Default is \code{\link[base:logical]{TRUE}}.}
 \item{...}{Not used}
 }

 \value{
 The \code{\link[base]{logical}} \code{\link[base]{vector}} of 'digits' length. If 'digits' is less than the length of the output vector,
 the actual length is used.
 }

 \examples{
 b<-Misc$to.binary.logical(25,digits=10);
 }

 \seealso{For more information see \code{\link{Misc}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
