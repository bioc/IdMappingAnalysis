%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  idMapBase.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{dimnames.IdMapBase}
\alias{dimnames.IdMapBase}
\alias{IdMapBase.dimnames}
\alias{dimnames.IdMapBase}
\alias{dimnames,IdMapBase-method}


 \title{Retrieve or set the dimnames of data frame encapsulated within the given IdMapBase object}
 \description{Retrieve or set the dimnames of data frame encapsulated within the given IdMapBase object.}

 \usage{\method{dimnames}{IdMapBase}(x)}

 \arguments{\item{...}{Not used}}
 \value{Dimnames of a \code{\link[base]{data.frame}} encapsulated within the given IdMapBase object}

 \examples{
 obj<-IdMapBase(examples$mrnaExperimentSet);
 rownames(obj)[1:10];
 colnames(obj)[1:5];
 }
 \seealso{For more information see \code{\link{IdMapBase}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
