%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  idMapBase.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{dim.IdMapBase}
\alias{dim.IdMapBase}
\alias{IdMapBase.dim}
\alias{dim.IdMapBase}
\alias{dim,IdMapBase-method}


 \title{Retrieves dimensions of data frame encapsulated within the given IdMapBase object}
 \description{Retrieves dimensions of data frame encapsulated within the given IdMapBase object.}

 \usage{\method{dim}{IdMapBase}(x)}

 \arguments{\item{...}{Not used}}
 \value{Dimensions of a \code{\link[base]{data.frame}} encapsulated within the given IdMapBase object}

 \examples{
 obj<-IdMapBase(examples$mrnaExperimentSet);
 dim(obj);
 }
 \seealso{For more information see \code{\link{IdMapBase}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
