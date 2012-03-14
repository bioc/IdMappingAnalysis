%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  jointUniquePairs.interactive.plots.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 \name{interactive.plot.JointUniquePairs}
\alias{interactive.plot.JointUniquePairs}
\alias{JointUniquePairs.interactive.plot}
\alias{interactive.plot.JointUniquePairs}
\alias{interactive.plot,JointUniquePairs-method}


 \title{General purpose JointUniquePairs interactive plot function}

 \description{
 This is an interactive wrapper for JointUniquePairs plot functions allowing to
 subset the data on the UniquePairsMatch object and select customazable drawing parameters
 through the graphical selection dialog.
 }

 \usage{\method{interactive.plot}{JointUniquePairs}(this, data, plotFun, plotType, toggle_keys=list(), keys=list(), idMapNames="loop", new.plot=FALSE, file.copy=FALSE, copy.zoom=1, verbose=TRUE, ...)}

 \arguments{
 \item{data}{The data to be plotted.}
 \item{plotFun}{The plotting function.}
 \item{plotType}{Selection dialog caption}
 \item{toggle_keys}{The \code{\link[base]{list}} of \code{\link[base]{logical}} toggling custom keys controlling the drawing process. Once set, their value
 remains unchanged during subsequent drawing cycles ('loop' mode) until user toggles the key again.
 Internally, this list is automatically prepended by file.copy toggling key allowing to turn
 saving current plot into the file on and off. Default is empty list.}
 \item{keys}{The \code{\link[base]{list}} of \code{\link[base]{logical}} custom keys controlling the drawing process. Unlike toggle_keys, their value should be set
 every time the drawing cycle repeats ('loop' mode). Internally, the list is automatically prepended by new.plot key
 allowing to open a new graphic device for a plot. Default is empty list.}
 \item{idMapNames}{Either a \code{\link[base]{character}} vector of ID Map names, 'menu' or 'loop'. If ID Map names, the data for plotting are generated
 by subsetting the data on a set of ID pairs from the match corresponding to the given ID Map set. If 'menu' or 'loop',
 user can select ID Map set interactively from the graphical selection dialog simultaneously controlling plot destination
 (same or new graphic device), wherther or not the plot should be saved in a file as well as additional control parameters defined
 by toggle_keys and keys. If 'loop', the interactive drawing cycle repeats untill the user cancells the process. Default is 'loop'.}
 \item{new.plot}{\code{\link[base]{logical}} indicating if the new graphic device should be created for plot.
 In interactive mode can be selected graphically. Default is \code{\link[base:logical]{FALSE}}.}
 \item{file.copy}{\code{\link[base]{logical}} or a \code{\link[base]{character}} string indicating if the plot should be saved to a file.
 If \code{\link[base]{character}} string, it's content used as a file name and if \code{\link[base:logical]{TRUE}}, the default file named is formed and used.
 In interactive mode can be toggled graphically. Default is \code{\link[base:logical]{FALSE}}.}
 \item{copy.zoom}{Zoom factor used when saving plot in a file. Default is 1.}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Additional graphical parameters.}
 }

 \examples{\dontrun{
 #plot correlation densities choosing the DB set interactively
 examples$jointUniquePairs@interactive.plot(
			examples$corr,examples$jointUniquePairs$corr.plot,"correlation plot",idMapNames="loop",
			toggle_keys=list(plot.Union=TRUE,subsetting=TRUE),
			keys=list(),
			new.plot=FALSE,file.copy=FALSE,
			lineColors=NULL,lineStyles=NULL,lineWidths=2,verbose=TRUE);
 }}

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
