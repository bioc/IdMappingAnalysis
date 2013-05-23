%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/idMapBase.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{as.data.frame.IdMapBase}
\alias{as.data.frame.IdMapBase}
\alias{IdMapBase.as.data.frame}
\alias{as.data.frame,IdMapBase-method}

 \title{Retrieves a data frame encapsulated within the given IdMapBase object}
 \description{
 Retrieves a data frame encapsulated within the given IdMapBase object. The method can be used as object specific as well as static.
 In the latter case the method can coerce to \code{\link[base]{data.frame}} either IdMapBase or ExpressionSet
 derived object by using IdMapBase$as.data.frame(object) call signature.
 }

 \usage{
\method{as.data.frame}{IdMapBase}(x, row.names=NULL, optional=FALSE, ...)
}

 \arguments{
 \item{row.names}{Not used}
 \item{optional}{Not used}
 \item{...}{Not used}
 }
 \value{
 A \code{\link[base]{data.frame}} encapsulated within the given IdMapBase object or a \code{\link[base]{data.frame}}
 to which another IdMapBase or ExpressionSet derived object is coerced.}

 \examples{
 obj<-IdMapBase(examples$msmsExperimentSet);
 DF<-as.data.frame(obj);
 DF[1:20,1:5];

 #convert ExpressionSet to data.frame
 exprSet<-new("ExpressionSet",
    exprs=matrix(runif(1000), nrow=100, ncol=10));
 DF<-IdMapBase$as.data.frame(exprSet);
 DF[1:10,1:5];
 }
 \seealso{For more information see \code{\link{IdMapBase}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
