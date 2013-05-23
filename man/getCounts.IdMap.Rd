%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/idMap.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getCounts.IdMap}
\alias{getCounts.IdMap}
\alias{IdMap.getCounts}
\alias{getCounts,IdMap-method}

\title{Compute the count of secondaryIDs for each primary ID}
\description{Compute the count of secondaryIDs for each primary ID.}

\usage{
\method{getCounts}{IdMap}(this, verbose=FALSE, ...)
}

\arguments{
\item{idMap}{The IdMap object on which secondary ID counts should be computed.}
\item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
\item{...}{Not used.}
}
\value{\code{\link[base]{numeric}} vector of counts of secondary IDs for each primary ID.}

\examples{
obj<-IdMap(examples$identDfList[[3]]);
cnts<-obj$getCounts();
}
\seealso{For more information see \code{\link{IdMap}}.}
\author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
