%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  idMapBase.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{primaryKey.IdMapBase}
\alias{primaryKey.IdMapBase}
\alias{IdMapBase.primaryKey}
\alias{primaryKey.IdMapBase}
\alias{primaryKey,IdMapBase-method}


 \title{Retrieves a primary key for a given IdMapBase object}
 \description{Retrieves a primary key for a given IdMapBase object.
 The method can be used as object specific as well as static. In the latter case
 the method can accept a \code{\link[base]{data.frame}} using IdMapBase$primaryKey(<data.frame>) signature
 returning the name of the <data.frame> first column.
 }

 \usage{\method{primaryKey}{IdMapBase}(this, DF=NULL, ...)}

 \arguments{\item{...}{Not used}}
 \value{A \code{\link[base]{character}} string representing primary key for given IdMapBase object}

 \examples{
 obj<-IdMapBase(examples$identDfList[[1]]);
 obj$primaryKey();
 }
 \seealso{For more information see \code{\link{IdMapBase}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
