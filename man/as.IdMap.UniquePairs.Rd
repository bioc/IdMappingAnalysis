%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  uniquePairs.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{as.IdMap.UniquePairs}
\alias{as.IdMap.UniquePairs}
\alias{UniquePairs.as.IdMap}
\alias{as.IdMap.UniquePairs}
\alias{as.IdMap,UniquePairs-method}


\title{Convert the UniquePairs object into the IdMap object}

\description{Convert the UniquePairs object into the IdMap object.}

\usage{\method{as.IdMap}{UniquePairs}(this, keepOrder=TRUE, verbose=FALSE, ...)}

\arguments{
\item{keepOrder}{\code{\link[base]{logical}} indicating if the original order of primary IDs should be kept. Default is TRUE}
\item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
\item{...}{Not used.}
}
\value{UniquePairs object or list of UniquePairs objects with swapped keys.}

\examples{
pairs<-UniquePairs$create(examples$identDfList[[2]]);
idMap<-as.IdMap(pairs);
idMap[1:10,];
}

\author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
