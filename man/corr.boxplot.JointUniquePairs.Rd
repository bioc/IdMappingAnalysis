%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  jointUniquePairs.plots.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{corr.boxplot.JointUniquePairs}
\alias{corr.boxplot.JointUniquePairs}
\alias{JointUniquePairs.corr.boxplot}
\alias{corr.boxplot.JointUniquePairs}
\alias{corr.boxplot,JointUniquePairs-method}


 \title{Boxplot of correlations by match group}

 \description{
 Creates a boxplot of correlations data from the Corr object
 by a set of match groups from the UniquePairsMatch object,
 utilizing the JointUniquePairs$boxplot function
 }

 \usage{\method{corr.boxplot}{JointUniquePairs}(this, corr, idMapNames=NULL, show.None=FALSE, subsetting=FALSE, group.gap=0.2, cex.main=1.2, cex.lab=1, srt=0, adj=0, par.zoom=1, main=paste("Correlations (", colnames(corr)[3], ") by match group", sep = ""), plot=TRUE, verbose=FALSE, ...)}

 \arguments{
 \item{corr}{Corr object which correlation values are used for boxplot.}
 \item{idMapNames}{Defines a subset of match group names to be plotted.
 If $NULL (default), all group names within the pairsMatch are used.}
 \item{subsetting}{If \code{\link[base:logical]{TRUE}}, interprets Corr subset as a full group
 or uses the original Corr as a full group otherwise. Default is \code{\link[base:logical]{FALSE}}.}
 \item{show.None}{If \code{\link[base:logical]{TRUE}}, includes a plot of subset of values not included
 into any other match group  with 'None' label. Default is \code{\link[base:logical]{FALSE}}.}
 \item{group.gap}{The gap between match groups. Default is 0.2.}
 \item{cex.main}{Font size for plot main title. Default is 1.2.}
 \item{cex.lab}{Font size for X and Y axis titles. Default is 1.}
 \item{srt}{Match group label orientation (see par('srt')). Default is 0.}
 \item{adj}{\code{\link[base]{numeric}} in a range (0..1) determening how close to the x-axis
 the group labels are, 1 being the closest and 0 is farthest. The default is 0.}
 \item{par.zoom}{Graphics parameters zoom factor. Scales the graphical
 parameters like cex, lwd, mai etc. Default is 1.}
 \item{main}{The main title. Default is
 'Correlations (type) by match group', where type is derived from corr object
 ('spearman' or 'pearson').}
 \item{plot}{if \code{\link[base:logical]{TRUE}} (default) then a boxplot is produced.
 If not, the summaries which the boxplots are based on are returned.}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Additional graphical parameters}
 }

 \value{
 Same as for boxplot in graphics package
 }

 \examples{
 #plot correlation probability distributions (boxplots) by match group
 mapNames=list(NetAffx_Q="Affy_Q",DAVID_F="D_F",EnVision_Q="EnQ");
 data = examples$jointUniquePairs$corr.boxplot(examples$corr,
	        idMapNames=mapNames,subsetting=TRUE,
              multiline=TRUE, srt=30, cex.lab=0.8, col.points="green" , main="");
 }

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
