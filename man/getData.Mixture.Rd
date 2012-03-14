%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  mixture.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getData.Mixture}
\alias{getData.Mixture}
\alias{Mixture.getData}
\alias{getData.Mixture}
\alias{getData,Mixture-method}


 \title{Extract mixture component data from the Mixture object}

 \description{Extract mixture component data from the Mixture object.}

 \usage{\method{getData}{Mixture}(this, G=NULL, ...)}

 \arguments{
 \item{G}{Component number. If NULL (default), the highest component is returned.}
 \item{...}{Not used}
 }

 \value{
 \code{\link[base]{numeric}} vector of mixture component data with 'Component' attribute
 indicating the component number retrieved.
 }

 \examples{
 dat<-examples$mixture$getData();
 dat[1:20];
 attr(dat,"Component");
 }

 \seealso{For more information see \code{\link{Mixture}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
