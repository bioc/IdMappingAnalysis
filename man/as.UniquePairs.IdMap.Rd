%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/idMap.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{as.UniquePairs.IdMap}
\alias{as.UniquePairs.IdMap}
\alias{IdMap.as.UniquePairs}
\alias{as.UniquePairs,IdMap-method}


 \title{Create a UniquePairs object from a given IdMap object}

 \description{Create a UniquePairs object from a given IdMap object.}

 \usage{
\method{as.UniquePairs}{IdMap}(this, secondaryIDs=NULL, verbose=FALSE, ...)
}

 \arguments{
 \item{secondaryIDs}{optional secondary ID list on which the resulting UniquePairs
 object is intersected. Default is NULL (not present).}
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{A UniquePairs object}

 \examples{
 idMap<-IdMap(examples$identDfList[[1]]);
 uniquePairs<-as.UniquePairs(idMap);
 dim(uniquePairs);
 uniquePairs[1:10,];
 }
 \seealso{For more information see \code{\link{IdMap}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
