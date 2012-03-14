%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  jointIdMap.plots.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{ecdf.plot.JointIdMap}
\alias{ecdf.plot.JointIdMap}
\alias{JointIdMap.ecdf.plot}
\alias{ecdf.plot.JointIdMap}
\alias{ecdf.plot,JointIdMap-method}


 \title{Interactive wrapper for IdMapCounts$plot}

 \description{
 Interactive wrapper of the IdMapDiff$plot allowing to
 select the ID Map encapsulated within the given JointIdMap object
 for a ecdf plot through the dialog.
 }

 \usage{\method{ecdf.plot}{JointIdMap}(this, idMapNames="loop", complement=TRUE, log=TRUE, new.plot=FALSE, file.copy=FALSE, copy.zoom=1, verbose=TRUE, ...)}

 \arguments{
 \item{jointIdMap}{ JointIdMap object from which ID Map counts information is extracted.}
 \item{idMapNames}{ either ID Map names, 'menu' or 'loop'. If ID Map names,
 the functionality is similar to IdMapCounts.plot If 'menu' or 'loop',
 user can select the ID Maps interactively from the graphical selection
 dialog simultaneously controlling the plot destination (same or new graphic device)
 and wherther or not the plot should be saved in a file. If 'loop', the interactive
 drawing cycle repeats untill the user cancells the process. Default is names of all
 ID Maps within the jointIdMap object.}
 \item{complement}{ \code{\link[base]{logical}} indicating wherether the complementary ecdf should be plotted
 the default is \code{\link[base:logical]{TRUE}}.}
 \item{log}{ \code{\link[base]{logical}} indicating if log scale should be used. The default is \code{\link[base:logical]{TRUE}}.}
 \item{new.plot}{ \code{\link[base]{logical}} indicating if the new graphic device should be created for plot.
 In interactive mode can be selected graphically. Default is \code{\link[base:logical]{FALSE}}.}
 \item{file.copy}{\code{\link[base]{logical}} or a \code{\link[base]{character}} string indicating if the plot should be saved to a file.
 If \code{\link[base]{character}} string, it's content used as a file name and if \code{\link[base:logical]{TRUE}}, the default file named is used.
 In interactive mode can be toggled graphically. Default is \code{\link[base:logical]{FALSE}}.}
 \item{copy.zoom}{ Zoom factor used when saving plot in a file. Default is 1.}
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \examples{
 jointIdMap<-JointIdMap(examples$identDfList);

 #non-interactive ecdf plot
 #of 3 DBs DBs encapsulated within the JointIdMap object
 jointIdMap$ecdf.plot(idMapNames=c("NetAffx_F","DAVID_Q","EnVision_Q"));
 \dontrun{
 #interactive plot allowing to choose the DBs to be plotted
 jointIdMap$ecdf.plot();
 }}

 \seealso{For more information see \code{\link{JointIdMap}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
