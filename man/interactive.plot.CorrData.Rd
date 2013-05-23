%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/corrData.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{interactive.plot.CorrData}
\alias{interactive.plot.CorrData}
\alias{CorrData.interactive.plot}
\alias{interactive.plot,CorrData-method}

 \title{Draw a scatterplot of experiment data interactively}

 \description{Draw a scatterplot of experiment data interactively.}

 \usage{
\method{interactive.plot}{CorrData}(this, input="loop", new.plot=FALSE, file.copy=FALSE, copy.zoom=1, ...)
}

 \arguments{
 \item{input}{ character vector of primary IDs, vector of match pairs, "menu" or "loop".
 In case of "menu" or "loop" the set if primary IDs is determined
 interactively using graphical selection menu, and in case of "loop" the process of plotting repeates until user cancels it.
 If data series for more than one match pair to be displayed (in case input is a vector of primary IDs for example), the set of match pairs
 also selected intactively. Default is "loop".}
 \item{new.plot}{ If \code{\link[base:logical]{TRUE}}, then new graphic device is opened for plotting. Default is \code{\link[base:logical]{FALSE}}.}
 \item{file.copy}{\code{\link[base]{logical}} or a \code{\link[base]{character}} string indicating if the plot should be saved to a file.
 If \code{\link[base]{character}} string, it's content used as a file name and if \code{\link[base:logical]{TRUE}}, the default file named is used. Default is \code{\link[base:logical]{FALSE}}.}
 \item{copy.zoom}{ Zoom factor used when saving plot in a file. Default is 1.}
 \item{...}{Additional graphical parameters including ones from plot.CorrData}
 }

 \examples{
 #non-interactive scatterplot plot with a single primary and secondary IDs pair and outcomes
 examples$corrData$interactive.plot(input=list(c("P07355","1568126_at")),
 		outcomePairs=examples$outcomeMap,proteinNames="ANXA2",
 		cols=c("green","red","darkblue"),cex=1.2,cex.main=1.2,font.main=4,cex.lab=1.2);

 \dontrun{
 #interactive scatterplot with a single primary ID (uniprot) and outcomes
 examples$corrData$interactive.plot(c("P07355"), outcomePairs=examples$outcomeMap, proteinNames="ANXA2");

 #interactive scatterplot with multiple probeset IDs (uniprot) and without outcomes -  ANXA2 (annexin 2)
 examples$corrData$interactive.plot(c("P07355", "P07384", "P09382"));
 }}

 \seealso{For more information see \code{\link{CorrData}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
