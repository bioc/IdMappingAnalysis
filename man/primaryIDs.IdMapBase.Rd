%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  idMapBase.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{primaryIDs.IdMapBase}
\alias{primaryIDs.IdMapBase}
\alias{IdMapBase.primaryIDs}
\alias{primaryIDs.IdMapBase}
\alias{primaryIDs,IdMapBase-method}


 \title{Retrieves the primary IDs for a given IdMapBase object}
 \description{Retrieves the primary IDs for a given IdMapBase object.
 The method can be used as object specific as well as static. In the latter case
 the method can accept a \code{\link[base]{data.frame}} using InMpaBase$primaryIDs(<data.frame>) signature
 returning the <data.frame> first column.
}

 \usage{\method{primaryIDs}{IdMapBase}(this, DF=NULL, ...)}

 \arguments{\item{...}{Not used}}
 \value{A \code{\link[base]{character}} vector of primary IDs for given IdMapBase object}

 \examples{
 obj<-IdMapBase(examples$identDfList[[1]]);
 obj$primaryIDs()[1:20];
 }
 \seealso{For more information see \code{\link{IdMapBase}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
