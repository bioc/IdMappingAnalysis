%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  mixture.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{clust.Mixture}
\alias{clust.Mixture}
\alias{Mixture.clust}
\alias{clust.Mixture}
\alias{clust,Mixture-method}


 \title{Retrieve the custering results data structure}
 \description{Retrieve the custering results data structure.}

 \usage{\method{clust}{Mixture}(this, ...)}

 \arguments{
 \item{...}{Not used}
 }

 \value{The clustering results data structure returned by Mclust()}

 \examples{
 cls<-examples$mixture$clust();
 names(cls);
 }

 \seealso{For more information see \code{\link{Mixture}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
