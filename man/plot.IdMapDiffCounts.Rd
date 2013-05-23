%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/idMapDiffCounts.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{plot.IdMapDiffCounts}
\alias{plot.IdMapDiffCounts}
\alias{IdMapDiffCounts.plot}
\alias{plot,IdMapDiffCounts-method}

 \title{Produce a fountain plot representing the quantitative relationship of the compound events}

 \description{
 Produce a fountain plot representing the quantitative relationship of the compound events
 <'excess both', 'excess left', 'excess right', 'same list', 'in left only', 'in right only', 'in neither' >
 }

 \usage{
\method{plot}{IdMapDiffCounts}(x, valRange=c(-20, 20, 10), reverse=FALSE, pairLabels=NULL, guideline.col="darkgrey", guideline.lty=2, guideline.lwd=par.zoom, cols=c("red", "blue", "green"), sides=2, cex=1, cex.side=0.75 * cex, srt=0, adj=0.5, par.zoom=1, ...)
}

 \arguments{
 \item{valRange}{\code{\link[base]{numeric}} vector of length 3 where the first and second elements
 are minimum and maximum count values to be displayed on horizontal axis and the third
 element is a distance between horizontal axis tick marks. Default is c(-20,20,10).}
 \item{reverse}{logical indicating the plot orientation (top to bottom or bottom to top)}
 \item{pairLabels}{optional decorated names for ID Map pair in consideration. If \code{\link[base]{NULL}} (default),
 the original ID Map names are used.}
 \item{guideline.col}{guideline(horisontal group divider) lines color. Default is 'darkgrey'.}
 \item{guideline.lty}{guideline lines type. Default is 2 (dashed).}
 \item{guideline.lwd}{guideline lines color. Default is par.zoom.}
 \item{cols}{colors corresponding to the <A-A*B, A*B, B-A*B> events from which the plot is composed.}
 \item{sides}{How compound events labels and counts are placed on plot.
 Possible values are 1 or 2. If sides=1 both event labels and counts are placed on one (left) side.
 If sides=2 then labels are placed on left side and counts on the right.}
 \item{cex}{plot title font size.}
 \item{cex.side}{compound events side labels font size.}
 \item{srt}{compound events labels orientation.}
 \item{adj}{compound events labels position adjustment (0 - 1), where 0/1 corresponds
 to the minimum/maximum labels position shift inwards the plot.}
 \item{par.zoom}{graphics parameters zoom factor. Scales the graphical parameters like cex, lwd, mai etc.}
 \item{...}{Additional graphical parameters}
 }

 \examples{
 #create IdMapDiffCounts object
 IDs<-IdMapBase$primaryIDs(examples$msmsExperimentSet);
 jointIdMap<-JointIdMap(examples$identDfList,primaryIDs=IDs);
 diffs<-jointIdMap$getDiff("NetAffx_F","DAVID_Q",verbose=TRUE);
 diffCounts<-IdMapDiffCounts(diffs);

 # fountain plot of DB pair differences
 # two-sided labels
 diffCounts$plot(sides=2);
 # one-sided labels
 diffCounts$plot(sides=1);
 }

 \seealso{For more information see \code{\link{IdMapDiffCounts}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
