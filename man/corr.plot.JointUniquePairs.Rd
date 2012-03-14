%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  jointUniquePairs.plots.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{corr.plot.JointUniquePairs}
\alias{corr.plot.JointUniquePairs}
\alias{JointUniquePairs.corr.plot}
\alias{corr.plot.JointUniquePairs}
\alias{corr.plot,JointUniquePairs-method}


 \title{Plot the density distributions for a set of correlation objects
 derived from JointUniquePairs and Corr objects}

 \description{
 Plot the density distributions for a set of correlation objects derived from
 JointUniquePairs and Corr objects with optional subsetting by a group of ID Maps.
 This is achived by first creating a correlation object from the JointUniquePairs
 and Corr objects with optional subsetting by a group of ID Maps and then calling
 the Corr.plot() on a resulting set of correlation objects.
 }

 \usage{\method{corr.plot}{JointUniquePairs}(this, corr, idMapNames=NULL, plot.Union=TRUE, subsetting=FALSE, lineColors=NULL, lineStyles=NULL, lineWidths=2, verbose=FALSE, ...)}

 \arguments{
 \item{corr}{Corr object.}
 \item{idMapNames}{If not \code{\link[base]{NULL}}, defines the subset of ID Maps from JointUniquePairs
 on which the full event group is to be formed. Default is \code{\link[base]{NULL}}.}
 \item{plot.Union}{If \code{\link[base:logical]{TRUE}} (default), plots also the density of the correlation
 object corrsesponding to the union of a set of correlation objects.}
 \item{subsetting}{If \code{\link[base:logical]{TRUE}}, subsets the  Corr on a group of ID Maps
 or uses the original Corr otherwise. Default is \code{\link[base:logical]{FALSE}}.}
 \item{lineColors}{The \code{\link[base]{vector}} of line colors (recycled if necessary)
 for plotting the distributions of different Corr objects.
 If \code{\link[base]{NULL}} (default), the predefined set of colors is used.}
 \item{lineStyles}{The \code{\link[base]{vector}} of line styles (recycled if necessary)
 for plotting the distributions of different Corr objects.
 If \code{\link[base]{NULL}} (default), the predefined set of line styles is used.}
 \item{lineWidths}{The \code{\link[base]{vector}} of line widths (recycled if necessary)
 for plotting the distributions of different Corr objects. Default is 2.}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Additional graphical parameters}
 }

 \value{
 The list of Corr objects which data densities are plotted
 }

 \examples{
 #plot the correlation densities of a Corr object (corr.spearman) on a given DB subset
 corrSet<-examples$jointUniquePairs$corr.plot(examples$corr,
             idMapNames=c("NetAffx_Q","DAVID_Q","EnVision_Q"),
	       plot.Union=TRUE,subsetting=TRUE,verbose=TRUE);
 names(corrSet);
 }

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
