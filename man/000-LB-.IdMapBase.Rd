%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  idMapBase.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{[.IdMapBase}
\alias{[.IdMapBase}
\alias{IdMapBase.[}
\alias{[.IdMapBase}
\alias{[,IdMapBase-method}


 \title{Access the elements of a data frame encapsulated within the given IdMapBase object using indexation}
 \description{Access the elements of a data frame encapsulated within the given IdMapBase object using indexation.}

 \usage{\method{[}{IdMapBase}(this, ...)}

 \arguments{\item{...}{Indexes of elements of a \code{\link[base]{data.frame}} encapsulated within the given object}}
 \value{Elements of a \code{\link[base]{data.frame}} encapsulated within the given object}

 \examples{
 obj<-IdMapBase(examples$msmsExperimentSet);
 obj[5:10,2:6];
 }
 \seealso{For more information see \code{\link{IdMapBase}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
