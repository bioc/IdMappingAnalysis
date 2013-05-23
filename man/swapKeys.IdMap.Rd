%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/idMap.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{IdMap$swapKeys}
\alias{IdMap$swapKeys}
\alias{swapKeys.IdMap}
\alias{IdMap.swapKeys}
\alias{swapKeys,IdMap-method}

\title{Swap the primary and secondary key columns}
\description{Swap the primary and secondary key columns.}

\usage{
## Static method (use this):
## IdMap$swapKeys(idMap, verbose=FALSE, ...)

## Don't use the below:
\method{swapKeys}{IdMap}(static, idMap, verbose=FALSE, ...)
}

\arguments{
\item{idMap}{The IdMap object or a list of IdMap objects which keys to be swapped.}
\item{verbose}{ If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
\item{...}{Not used.}
}
\value{IdMap object or list of IdMap objects with swapped keys.}

\examples{
uniprot2Affy<-IdMap(examples$identDfList[[1]]);
uniprot2Affy[1:10,];

affy2Uniprot<-IdMap$swapKeys(uniprot2Affy);
affy2Uniprot[1:10,];
}
\seealso{For more information see \code{\link{IdMap}}.}
\author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
