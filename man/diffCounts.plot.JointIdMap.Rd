%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  jointIdMap.plots.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 \name{diffCounts.plot.JointIdMap}
\alias{diffCounts.plot.JointIdMap}
\alias{JointIdMap.diffCounts.plot}
\alias{diffCounts.plot.JointIdMap}
\alias{diffCounts.plot,JointIdMap-method}


 \title{Interactive wrapper for IdMapDiffCounts$plot}

 \description{
 Interactive wrapper of the IdMapDiffCounts.plot allowing to
 select the ID Map pairs encapsulated within the given JointIdMap object
 for a fountaine plot through the dialog.
 }

 \usage{\method{diffCounts.plot}{JointIdMap}(this, idMapNames="loop", valRange=c(-20, 20, 10), reverse=FALSE, pairLabels=NULL, new.plot=FALSE, file.copy=FALSE, copy.zoom=1, cols=c("red", "blue", "green"), sides=2, cex=1, cex.side=0.75 * cex, srt=0, adj=0.5, verbose=TRUE, ...)}

 \arguments{
 \item{idMapNames}{ either ID Maps pair names, 'menu' or 'loop'. If ID Map pairs, the functionality is similar to IdMapDiffCounts.plot
 If 'menu' or 'loop', user can select the pair of ID Maps interactively from the graphical selection dialog simultaneously controlling
 the plot destination (same or new graphic device) and wherther or not the plot should be saved in a file. If 'loop', the interactive
 drawing cycle repeats untill the user cancells the process.}
 \item{valRange}{ \code{\link[base]{numeric}}l vector of length 3 where the first and second elements are minimum and maximum count values
 to be displayed on horizontal axis and the third element is a distance between horizontal axis tick marks.
 Default is c(-20,20,10).}
 \item{reverse}{ logical indicating the plot orientation (top to bottom or bottom to top). Default is \code{\link[base:logical]{FALSE}}.}
 \item{pairLabels}{ optional decorated names for ID Map pair in consideration. If \code{\link[base]{NULL}}, the original ID Map names
 are used. Default is@ NULL.}
 \item{cols}{ colors corresponding to the <A-A*B, A*B, B-A*B> events from which the plot is composed.}
 \item{sides}{ (1 or 2): how compound events labels and counts are placed on plot. If sides=1 both event labels and counts
 are placed on one (left) side. If sides=2 then labels are placed on left side and counts on the right.}
 \item{cex}{ plot title font size.}
 \item{cex.side}{ compound events labels font size.}
 \item{srt}{ compound events labels orientation.}
 \item{adj}{ compound events labels position adjustment (0 - 1), where 0/1 corresponds
 to the minimum/maximum labels position shift inwards the plot.}
 \item{new.plot}{ logical indicating if the new graphic device should be created for plot.
 In interactive mode can be selected graphically. Default is \code{\link[base:logical]{FALSE}}.}
 \item{file.copy}{\code{\link[base]{logical}} or a \code{\link[base]{character}} string indicating if the plot should be saved to a file.
 If \code{\link[base]{character}} string, it's content used as a file name and if \code{\link[base:logical]{TRUE}}, the default file named is used
 If \code{\link[base]{logical}}, in interactive mode can be toggled graphically. Default is \code{\link[base:logical]{FALSE}}.}
 \item{copy.zoom}{ Zoom factor used when saving plot in a file.  Default is 1.}
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \examples{
 jointIdMap<-JointIdMap(examples$identDfList);

 #non-interactive fountain plot of differences
 #between two DBs encapsulated within the JointIdMap object
 jointIdMap$diffCounts.plot(idMapNames=c("NetAffx_F","DAVID_Q"));
 \dontrun{
 #interactive plot allowing to choose the DB pair to be plotted
 jointIdMap$diffCounts.plot();
 }}

 \seealso{For more information see \code{\link{JointIdMap}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
