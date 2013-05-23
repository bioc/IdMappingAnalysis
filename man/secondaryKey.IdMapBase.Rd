%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/idMapBase.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{secondaryKey.IdMapBase}
\alias{secondaryKey.IdMapBase}
\alias{IdMapBase.secondaryKey}
\alias{secondaryKey,IdMapBase-method}

 \title{Retrieves a secondary key for a given IdMapBase object}
 \description{Retrieves a secondary key for a given IdMapBase object.}

 \usage{
\method{secondaryKey}{IdMapBase}(this, ...)
}

 \arguments{\item{...}{Not used}}
 \value{A \code{\link[base]{character}} string representing secondary key for given IdMapBase object}

 \examples{
 obj<-IdMapBase(examples$identDfList[[1]]);
 obj$secondaryKey();
 }
 \seealso{For more information see \code{\link{IdMapBase}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
