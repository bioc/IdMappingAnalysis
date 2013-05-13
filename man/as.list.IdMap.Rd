%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  idMap.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{as.list.IdMap}
\alias{as.list.IdMap}
\alias{IdMap.as.list}
\alias{as.list.IdMap}
\alias{as.list,IdMap-method}


 \title{Coerce an object or a list of compatible object
 to the IdMap object or a list of IdMap objects}

 \description{
 Coerce an object or a list of compatible object
 to the IdMap object or a list of IdMap objects.
 The object should be inherited from \code{\link[base]{matrix}}, \code{\link[base]{data.frame}}, IdMap or UniquePairs,
 otherwise an exception will be thrown.
 }

\usage{IdMap$as.list(idMapList, verbose=FALSE, ...)}

 \arguments{
 \item{idMapList}{A \code{\link[base]{list}} or a single object of compatible type.}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{
 A single IdMap object or a list of IdMap objects.
 }

 \examples{
 idMaps<-IdMap$as.list(examples$identDfList);
 names(idMaps);
 }

 \seealso{For more information see \code{\link{IdMap}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
