%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/jointUniquePairs.plots.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{mixture.boxplot.JointUniquePairs}
\alias{mixture.boxplot.JointUniquePairs}
\alias{JointUniquePairs.mixture.boxplot}
\alias{mixture.boxplot,JointUniquePairs-method}

 \title{Boxplot of a mixture model component by match group}

 \description{
 Creates a boxplot of mixture component data from the Mixture object
 by a set of match groups from the JointUniquePairs object,
 utilizing the JointUniquePairs$boxplot function
 }

 \usage{
\method{mixture.boxplot}{JointUniquePairs}(this, corr, idMapNames=NULL, show.None=FALSE, subsetting=FALSE, group.gap=0.2, G=c(1:5), plot.G=NULL, cex.main=1.2, cex.lab=1, srt=0, adj=0, par.zoom=1, main="Component posterior probability by match group", plot=TRUE, verbose=FALSE, ...)
}

 \arguments{
 \item{corr}{Corr object from which the Mixture object is constructed on the fly
 to use one of it'c components for the boxplot corresponding to the best fit is plotted.}
 \item{idMapNames}{Defines a subset of match group names to be plotted.
 If \code{\link[base]{NULL}} (default), all group names within the pairsMatch are used.}
 \item{subsetting}{If \code{\link[base:logical]{TRUE}}, interprets Corr subset as a full group
 or uses the original Corr as a full group otherwise. Default is \code{\link[base:logical]{FALSE}}.}
 \item{show.None}{If not \code{\link[base]{NA}}, includes a plot of subset of values not included
 into any other match group  with 'None' label. Default is \code{\link[base]{NA}}.}
 \item{group.gap}{The gap between match groups. Default is 0.2.}
 \item{cex.main}{Font size for plot main title. Default is 1.2.}
 \item{cex.lab}{Font size for X and Y axis titles. Default is 1.}
 \item{srt}{Match group label orientation (see par('srt')). The default is 0.}
 \item{adj}{\code{\link[base]{numeric}} in a range (0..1) determening how close to the x-axis the group
 labels are, 1 being the closest and 0 is farthest. The default is 0.}
 \item{G}{Number of components in mixture model. If G is a \code{\link[base]{vector}}, the optimal
 number of component is determined. G is a vector (1:5) by default.}
 \item{plot.G}{The mixture component to be used for plotting.
 If \code{\link[base]{NULL}} (default), the highest-order component of the mixture model is used.}
 \item{par.zoom}{Graphics parameters zoom factor. Scales the graphical
  parameters like cex, lwd, mai etc. Default is 1.}
 \item{main}{The main title. Default is
 'Component posterior probability by match group'}
 \item{plot}{if \code{\link[base:logical]{TRUE}} (default) then a boxplot is produced.
 If not, the summaries which the boxplots are based on are returned.}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Additional graphical parameters}
 }

 \value{
 Same as for boxplot in graphics package
 }

 \examples{
 #plot posterior second component probability distributions by match group
 mapNames=list(NetAffx_Q="Affy_Q",DAVID_F="D_F",EnVision_Q="EnQ");
 examples$jointUniquePairs$mixture.boxplot(examples$corr,
	idMapNames=mapNames,
	multiline=TRUE,subsetting=TRUE, plot.G=2,
	srt=35, col.points="red",verbose=TRUE);
 }

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
