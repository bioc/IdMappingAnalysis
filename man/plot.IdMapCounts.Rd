%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/idMapCounts.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{plot.IdMapCounts}
\alias{plot.IdMapCounts}
\alias{IdMapCounts.plot}
\alias{plot,IdMapCounts-method}

 \title{Compute and plot the (inversed) ecdf for each ID Map count entry within the IdMapCounts object}
 \description{
 Compute and plot the (inversed) ecdf for each ID Map count entry within the IdMapCounts object.
 }

 \usage{
\method{plot}{IdMapCounts}(x, idMapNames=NULL, complement=TRUE, log=TRUE, lineColors=NULL, pointSymbols=NULL, pointColors=NULL, cex.main=1.2, cex.lab=1, cex.axis=1, cex.legend=1, par.zoom=1, ...)
}

 \arguments{
 \item{idMapNames}{ optional subset of ID Map names within the counts object
 for which ecdf's should be plotted. If \code{\link[base]{NULL}} (default), the ecdf's for all ID Maps
 within the object are plotted.}
 \item{complement}{ \code{\link[base]{logical}} indicating wherether the complementary ecdf should be plotted
 The default is \code{\link[base:logical]{TRUE}}}
 \item{log}{ \code{\link[base]{logical}} indicating if log scale should be used. The default is \code{\link[base:logical]{TRUE}}.}
 \item{lineColors}{ Recyclable vector of line colors. If \code{\link[base]{NULL}} (default), a set of predefined colors is used.}
 \item{pointSymbols}{ Character or numeric recyclable vector of symbols used to plotting points.
 If \code{\link[base]{NULL}} (default), a set of predefined symbols is used.}
 \item{pointColors}{ Recyclable vector of point colors. If NULL (default), a set of predefined colors is used.}
 \item{cex.main}{ Font size for plot main title. Default is 1.2.}
 \item{cex.lab}{ Font size for X and Y axis titles. Default is 1.}
 \item{cex.axis}{ Font sixe for axis labels. Default is 1.}
 \item{cex.legend}{ Legend font size. Default is 1.}
 \item{par.zoom}{ graphics parameters zoom factor. Scales the graphical parameters like cex, lwd, mai etc.}
 \item{...}{Additional graphical parameters}
 }

 \examples{
 jointIdMap<-JointIdMap(examples$identDfList);
 cnts<-jointIdMap$getCounts();
 cnts$plot();
 }
 \seealso{For more information see \code{\link{IdMapCounts}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
