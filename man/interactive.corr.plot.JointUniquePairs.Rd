%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  jointUniquePairs.interactive.plots.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{interactive.corr.plot.JointUniquePairs}
\alias{interactive.corr.plot.JointUniquePairs}
\alias{JointUniquePairs.interactive.corr.plot}
\alias{interactive.corr.plot.JointUniquePairs}
\alias{interactive.corr.plot,JointUniquePairs-method}


 \title{Interactive plot of correlation densities}

 \description{
 Plots the density distributions for a set of correlation objects derived from JointUniquePairs and Corr objects
 with optional subsetting by a group of ID Maps. This is an interactive version of JointUniquePairs$corr.plot utilizing the
 JointUniquePairs@interactive.plot wrapper functionality.
 }

 \usage{\method{interactive.corr.plot}{JointUniquePairs}(this, corr, groups="loop", new.plot=FALSE, file.copy=FALSE, plot.Union=TRUE, subsetting=FALSE, lineColors=NULL, lineStyles=NULL, lineWidths=2, verbose=TRUE, ...)}

 \arguments{
 \item{corr}{Corr object.}
 \item{groups}{Either ID Maps names, 'menu' or 'loop'. If ID Map names, the functionality is similar to UniquePairsMatch.corr.plot
 If 'menu' or 'loop', user can select the pair of ID Maps interactively from the graphical selection dialog simultaneously controlling
 the plot destination (same or new graphic device), wherther or not the plot should be saved in a file etc. If 'loop', the interactive
 drawing cycle repeats untill the user cancells the process.}
 \item{new.plot}{\code{\link[base]{logical}} indicating if the new graphic device should be created for plot.
 In interactive mode can be selected graphically. Default is \code{\link[base:logical]{FALSE}}.}
 \item{file.copy}{\code{\link[base]{logical}} indicating if the plot should be saved in a file.
 In interactive mode can be toggled graphically. Default is \code{\link[base:logical]{FALSE}}.}
 \item{plot.Union}{If \code{\link[base:logical]{TRUE}} (default), plots also the density of the correlation object corrsesponding
 to the union of a set of correlation objects. In interactive mode can be toggled graphically.}
 \item{subsetting}{If TRUE, subsets the  Corr on a group of ID Maps
 or uses the original Corr otherwise.  In interactive mode can be toggled graphically. Default is \code{\link[base:logical]{FALSE}}.}
 \item{lineColors}{The \code{\link[base]{vector}} of line colors (recycled if necessary)
 for plotting the distributions of different Corr objects.
 If \code{\link[base]{NULL}} (default), the predefined set of colors is used.}
 \item{lineStyles}{The \code{\link[base]{vector}} of line styles (recycled if necessary)
 for plotting the distributions of different Corr objects.
 If \code{\link[base]{NULL}} (default), the predefined set of line styles is used.}
 \item{lineWidths}{The vector of line widths (recycled if necessary)
 for plotting the distributions of different Corr objects. Default is 2.}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Additional graphical parameters.}
 }

 \examples{
 #plot correlation densities for a given DB set
 #treating the given DB set as a full group (subsetting=TRUE)
 examples$jointUniquePairs$interactive.corr.plot(examples$corr,
		groups=c("NetAffx_Q","DAVID_Q","EnVision_Q"),
		subsetting=TRUE,new.plot=TRUE,file.copy=FALSE,verbose=TRUE,
		cex.lab=1.4,cex.legend=1.1);
 \dontrun{
 #plot correlation densities choosing the DB set interactively
 examples$jointUniquePairs$interactive.corr.plot(examples$corr,
		copy.zoom=2,verbose=TRUE,cex.lab=1.4,cex.legend=1.1);
 }}

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
