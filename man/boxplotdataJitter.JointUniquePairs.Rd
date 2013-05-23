%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/jointUniquePairs.plots.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 \name{JointUniquePairs$boxplotdataJitter}
\alias{JointUniquePairs$boxplotdataJitter}
\alias{boxplotdataJitter.JointUniquePairs}
\alias{JointUniquePairs.boxplotdataJitter}
\alias{boxplotdataJitter,JointUniquePairs-method}

 \title{Plot a set of points representing the density distribution of a data set}

 \description{
 Plot a set of points representing the density distribution of a data set.
 }

 \usage{
## Static method (use this):
## JointUniquePairs$boxplotdataJitter(y, round, verbose=FALSE, ...)

## Don't use the below:
\method{boxplotdataJitter}{JointUniquePairs}(static, y, round, verbose=FALSE, ...)
}

 \arguments{
 \item{y}{\code{\link[base]{numeric}} vector representing a data set.}
 \item{round}{If \code{\link[base:logical]{TRUE}} (default), then use rounding to reduce the amount of redundant plotting}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Additional graphical parameters}
 }

 \value{
 A \code{\link[base]{data.frame}} with two columns 'x' and 'y' which can be used
 to overlay the box plot with density distribution using plot(x,y)
 }

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
