%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/jointUniquePairs.interactive.plots.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{interactive.corr.boxplot.JointUniquePairs}
\alias{interactive.corr.boxplot.JointUniquePairs}
\alias{JointUniquePairs.interactive.corr.boxplot}
\alias{interactive.corr.boxplot,JointUniquePairs-method}

 \title{Interactive boxplot of correlations by match group}

 \description{
 Creates a boxplot of correlations derived from  JointUniquePairs and Corr objects with optional subsetting
 by a group of ID Maps.  This is an interactive version of JointUniquePairs@corr.boxplot unitilizing the
 JointUniquePairs@interactive.plot wrapper functionality.
 }

 \usage{
\method{interactive.corr.boxplot}{JointUniquePairs}(this, corr, groups="loop", show.None=FALSE, new.plot=FALSE, file.copy=FALSE, subsetting=FALSE, verbose=TRUE, srt=0, adj=0, ...)
}

 \arguments{
 \item{corr}{Corr object.}
 \item{groups}{Either ID Map  names, 'menu' or 'loop'. If ID Map names, the functionality is similar to JointUniquePairs$corr.boxplot
 If 'menu' or 'loop', user can select the ID Map set interactively from the graphical selection dialog simultaneously controlling
 the plot destination (same or new graphic device) wherther or not the plot should be saved in a file etc. If 'loop', the interactive
 drawing cycle repeats untill the user cancells the process.}
 \item{show.None}{If \code{\link[base:logical]{TRUE}}, includes a plot of subset of values not included into any other match group
 with 'None' label. Default is \code{\link[base:logical]{FALSE}}.}
 \item{new.plot}{\code{\link[base]{logical}} indicating if the new graphic device should be created for plot.
 In interactive mode can be selected graphically. Default is \code{\link[base:logical]{FALSE}}.}
 \item{file.copy}{\code{\link[base]{logical}} or a \code{\link[base]{character}} string indicating if the plot should be saved to a file.
 If \code{\link[base]{character}} string, it's content used as a file name and if \code{\link[base:logical]{TRUE}}, the default file named is formed and used.
 In interactive mode can be toggled graphically. Default is \code{\link[base:logical]{FALSE}}.}
 \item{subsetting}{If \code{\link[base:logical]{TRUE}}, interprets Corr subset as a full group
 or uses the original Corr as a full group otherwise.
 In interactive mode can be toggled graphically. Default is \code{\link[base:logical]{FALSE}}.}
 \item{srt}{Match group label orientation. The default is 0.}
 \item{adj}{\code{\link[base]{numeric}} in a range (0..1) determening how close to the x-axis the group labels are,
 1 being the closest and 0 being farthest. The default is 0.}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Additional graphical parameters}
 }

 \examples{
 #boxplot of correlations by match group for a given DB set (using short names)
 #treating the given DB set as a full group (subsetting=TRUE)
  DBs=list("NetAffx_Q"="AffQ","DAVID_Q"="DQ","EnVision_Q"="EnV");

 examples$jointUniquePairs$interactive.corr.boxplot(examples$corr,
		groups=DBs,
		subsetting=TRUE,new.plot=TRUE,file.copy=FALSE,
		srt=30,adj=0,cex=1,cex.lab=1.4,cex.axis=1.2,cex.main=1.4,verbose=TRUE);

 \dontrun{
 #boxplot of correlations by match group choosing the DB set interactively
 examples$jointUniquePairs$interactive.corr.boxplot(examples$corr,multiline=TRUE,
		srt=90,adj=0,cex=1,cex.lab=1,cex.axis=1,cex.main=1.2,verbose=TRUE);

 }}

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
