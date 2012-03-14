%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  uniquePairs.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{equals.UniquePairs}
\alias{equals.UniquePairs}
\alias{UniquePairs.equals}
\alias{equals.UniquePairs}
\alias{equals,UniquePairs-method}


\title{Check  if two unique pairs data structures are identical}

\description{Check  if two unique pairs data structures are identical.}

\usage{\method{equals}{UniquePairs}(this, other, ...)}

\arguments{
\item{other}{Another UniquePairs object the given object os compared to}
\item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
\item{...}{Not used.}
}
\value{\code{\link[base]{logical}} indicating if two UniquePairs objects are equal}

\examples{
pairs1<-UniquePairs$create(examples$identDfList[[1]]);
pairs2<-UniquePairs$create(examples$identDfList[[2]]);
pairs3<-pairs1;

pairs1$equals(pairs2);
pairs1$equals(pairs3);
}

\author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
