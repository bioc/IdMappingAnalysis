%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  bootstrap.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{plot.Bootstrap}
\alias{plot.Bootstrap}
\alias{Bootstrap.plot}
\alias{plot.Bootstrap}
\alias{plot,Bootstrap-method}


 \title{Scatterplot of bootstrapped results: sd vs correlation }

 \description{
 Scatterplot of the sd vs correlation values obtained from the pre-computed Bootstrap object
 along with two fitted lines: the first (solid blue) for a bootstrap smooth
 and a second (dashed red) for a normal theory formula
 }

 \usage{\method{plot}{Bootstrap}(x, new.plot=FALSE, file.copy=FALSE, copy.zoom=1, cex.main=1.2, cex.lab=1, cex.axis=1, cex.legend=1, ...)}

 \arguments{
 \item{new.plot}{\code{\link[base]{logical}} indicating if the new graphic device should be created for plot. Default is \code{\link[base:logical]{FALSE}}.}
 \item{file.copy}{\code{\link[base]{logical}} or a \code{\link[base]{character}} string indicating if the plot should be saved to a file.
 If \code{\link[base]{character}} string, it's content used as a file name and if \code{\link[base:logical]{TRUE}}, the default file named is formed and used. Default is \code{\link[base:logical]{FALSE}}.}
 \item{cex.main}{Main title font size. Default is 1.2.}
 \item{cex.lab}{X and Y titles font size. Default is 1.}
 \item{cex.axis}{X and Y axis labels font size. Default is 1.}
 \item{cex.legend}{plot legend font size. Default is par('cex').}
 \item{copy.zoom}{ Zoom factor used when saving plot in a file. Default is 1.}
 \item{...}{Additional graphic parameters}
 }

 \examples{
 examples$bootstrap$plot();
 }

 \seealso{For more information see \code{\link{Bootstrap}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
