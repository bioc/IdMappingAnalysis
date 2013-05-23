%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/uniquePairs.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getMatch.UniquePairs}
\alias{getMatch.UniquePairs}
\alias{UniquePairs.getMatch}
\alias{getMatch,UniquePairs-method}

 \title{Get the logical vector of pair matches of the given UniquePairs object
 in other UniquePair object(s)}

 \description{
 Compute the logical vector of matches or a list of such vectors
 between a given UniquePairs object and another UniquePairs object or a list of such objects.
 Analog of match() or \%in\% for vectors
 }

 \usage{
\method{getMatch}{UniquePairs}(this, other, verbose=FALSE, ...)
}

 \arguments{
 \item{other}{Another UniquePairs object or a list of objects the given object os compared to}
 \item{...}{Not used.}
 }

 \value{\code{\link[base]{logical}} vector or a list of logical vectors indicating if the given
 row in other UniquePairs object(s) is present in a given UniquePairs object.}

 \examples{
 pairs1<-UniquePairs$create(examples$identDfList[[1]]);
 pairs2<-UniquePairs$create(examples$identDfList[[2]]);
 matches<-pairs1$getMatch(pairs2);
 matches[1:50];
 }

 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
