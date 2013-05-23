%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/corr.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{plot.Corr}
\alias{plot.Corr}
\alias{Corr.plot}
\alias{plot,Corr-method}

 \title{Plot the density distributions for correlation object(s)}

 \description{
 Plot the density distributions for a given Corr object or list of Corr objects.
 The method can be used as object specific as well as static. In the latter case
 the method can accept a list of Corr objects using Corr$plot(corrList) call signature.
 }

 \usage{
\method{plot}{Corr}(x, dataList=NULL, title="nonparametric density fit", lineColors=NULL, lineStyles=NULL, lineWidths=2, verbose=FALSE, cex.main=1.2, cex.lab=1, cex.axis=1, cex.legend=1, par.zoom=1, ...)
}

 \arguments{
 \item{dataList}{ the list of Corr objects (used if static version of method invoked).
 Default is $NULL.}
 \item{title}{ main plot title. Default is 'nonparametric density fit'.}
 \item{lineColors}{ the vector of line colors (recycled if necessary)
 for plotting the distributions of different Corr objects.
 If \code{\link[base]{NULL}} (default), the predefined set of colors is used.}
 \item{lineStyles}{ the vector of line styles (recycled if necessary)
 for plotting the distributions of different Corr objects.
 If \code{\link[base]{NULL}} (default), the predefined set of line styles is used.}
 \item{lineWidths}{ the vector of line widths (recycled if necessary)
 for plotting the distributions of different Corr objects. Default is 2.}
 \item{verbose}{ if \code{\link[base:logical]{TRUE}} enables diagnostic messages.Default is \code{\link[base:logical]{FALSE}}.}
 \item{cex.main}{ Main title font size. Default is 1.2.}
 \item{cex.lab}{ X and Y titles font size. Default is 1.}
 \item{cex.axis}{ X and Y axis labels font size. Default is 1.}
 \item{cex.legend}{ font size for the plot legend. Default is 1.}
 \item{par.zoom}{ graphics parameters zoom factor. Scales the graphical parameters like cex, lwd, mai etc.
 Used to plot into the file based device with dimensions 'zoom' times bigger than for on-screen device.}
 \item{...}{additional graphical parameters}
 }

 \examples{
 examples$corr$plot();

 #create a set of corr. objects for a given DB subset treating subset
 #as a full group and then plot the correlation densities including union
 corrSet<-examples$jointUniquePairs$getCorr(examples$corr,
		groups=c("union","EnVision_Q","NetAffx_Q","DAVID_Q","EnVision_Q"),
		full.group=TRUE,verbose=TRUE);
 Corr$plot(corrSet);
 }

 \seealso{For more information see \code{\link{Corr}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
