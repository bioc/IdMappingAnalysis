%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/jointUniquePairs.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getUniquePairs.JointUniquePairs}
\alias{getUniquePairs.JointUniquePairs}
\alias{JointUniquePairs.getUniquePairs}
\alias{getUniquePairs,JointUniquePairs-method}

 \title{Extract the unity UniquePairs object from a given JointUniquePairs object}

 \description{
 Extract the UniquePairs object from the JointUniquePairs object
 corresponding to a unity (full group).
 }

 \usage{
\method{getUniquePairs}{JointUniquePairs}(this, ...)
}

 \arguments{
 \item{...}{Not used}
 }

 \value{UniquePairs object}

 \examples{
 uniquePairs<-examples$jointUniquePairs$getUniquePairs();
 uniquePairs[1:10,];
 }

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
