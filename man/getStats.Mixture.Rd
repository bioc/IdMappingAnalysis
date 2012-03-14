%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  mixture.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getStats.Mixture}
\alias{getStats.Mixture}
\alias{Mixture.getStats}
\alias{getStats.Mixture}
\alias{getStats,Mixture-method}


 \title{Get mixture component model summary info}

 \description{Get mixture component model summary info.}

 \usage{\method{getStats}{Mixture}(this, ...)}

 \arguments{
 \item{...}{Not used}
 }

 \value{
 A \code{\link[base]{data.frame}} with columns containing  mean, sd, weight and range of prevalence
 (range.low,range.high), one column for each model component
 }

 \examples{
 examples$mixture$getStats();
 }

 \seealso{For more information see \code{\link{Mixture}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
