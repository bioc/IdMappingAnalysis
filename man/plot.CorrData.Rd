%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  corrData.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{plot.CorrData}
\alias{plot.CorrData}
\alias{CorrData.plot}
\alias{plot.CorrData}
\alias{plot,CorrData-method}


 \title{Scatterplot of experiment data}

 \description{Scatterplot of experiment data.}

 \usage{\method{plot}{CorrData}(x, input, outcomePairs=NULL, xlab="protein count", ylab="mRNA expression (log10)", method="spearman", proteinNames=NULL, cols=brewer.pal(9, "Set1"), cex=1, cex.main=1.2, cex.lab=1, cex.axis=1, font=1, font.main=3, par.zoom=1, ...)}

 \arguments{
 \item{input}{ character vector of primary IDs, or either vector or list of match pairs.}
 \item{outcomePairs}{ The pairs <sample ID, outcome> or \code{\link[base]{NULL}} (default). In the first case the scatterplot
 points are plotted with symbol corresponding to the first letter of the outcome keyword.
 In the second, if there are more than one pair is plotted the point set for each pair is marked as 1, 2, etc.
 and if there is only one pair is present the unfilled circles are used}
 \item{xlab}{ The X axis label. Default is 'protein count'.}
 \item{ylab}{ The Y axis label. Default is 'mRNA expression'.}
 \item{method}{ the method used to compute the correlation coefficient between X and Y data. Default is "spearman".}
 \item{proteinNames}{ extra comments in the plot main title. Default is \code{\link[base]{NULL}} (no extra comments).}
 \item{cols}{ the (recycled) vector of colors to plot each data series with for the particular match pair.
  Default is RColorBrewer::brewer.pal(9,"Set1").}
 \item{cex}{ Plot font size. Default is 1.}
 \item{cex.main}{ Main title font size. Default is 1.2.}
 \item{cex.lab}{ X and Y titles font size. Default is 1.}
 \item{cex.axis}{ X and Y axis labels font size. Default is 1.}
 \item{font}{ data points  and axis labels font. Default is 2.}
 \item{font.main}{ main title font type. Default is 3.}
 \item{par.zoom}{ graphics parameters zoom factor. Scales the graphical parameters
 like cex, lwd, mai etc. Default is 1.}
 \item{...}{Additional graphical parameters}
 }

 \examples{
 #scatterplot with outcome for Uniprot="P07355" (annexin 2), probe set ID="213503_x_at"
 examples$corrData$plot(input=list(c("P07355", "213503_x_at")),
	xlab="spectral count",
	outcomePairs=examples$outcomeMap, proteinNames="ANXA2",
	cols=c("green", "red", "darkblue"));
 }

 \seealso{For more information see \code{\link{CorrData}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
