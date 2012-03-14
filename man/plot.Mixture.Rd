%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  mixture.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{plot.Mixture}
\alias{plot.Mixture}
\alias{Mixture.plot}
\alias{plot.Mixture}
\alias{plot,Mixture-method}


 \title{Plot the results of mixture modeling}

 \description{
 Plot the correlation densities of the
 empirical fit, mixture fit and each of the mixture components
 }

 \usage{\method{plot}{Mixture}(x, title="mixture density fit", cex.main=1.2, cex.lab=1, cex.axis=1, cex.legend=1, lineWidths=c(3, 3, 2), lineTypes=c(1, 2, 3), plot.crossover=TRUE, par.zoom=1, ...)}

 \arguments{
 \item{title}{ main plot title. Default is "nonparametric density fit".}
 \item{cex.main}{ Main title font size. Default is 1.2.}
 \item{cex.lab}{ X and Y titles font size. Default is 1.}
 \item{cex.axis}{ X and Y axis labels font size. Default is 1.}
 \item{cex.legend}{ font size for the plot legend. Default is 1.}
 \item{lineWidths}{ the vector of length 3 defing the line width for mixture fit,
 empirical fit and the micture components curves correspondingly. Default is c(3,3,2).}
 \item{lineTypes}{ the vector of length 3 defing the line type for mixture fit,
 empirical fit and the micture components curves correspondingly. Default is c(1,2,3).}
 \item{plot.crossover}{If \code{\link[base:logical]{TRUE}} (default), the vertical lines corresponding to pairwise intersection
 of mixture components are plotted.}
 \item{par.zoom}{ graphics parameters zoom factor. Scales the graphical parameters
 like cex, lwd, mai etc.}
 \item{...}{Additional graphical parameters}
 }

 \examples{
 examples$mixture$plot();
 }

 \seealso{For more information see \code{\link{Mixture}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
