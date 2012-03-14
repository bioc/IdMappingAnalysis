%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  jointUniquePairs.interactive.plots.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{interactive.mixture.plot.JointUniquePairs}
\alias{interactive.mixture.plot.JointUniquePairs}
\alias{JointUniquePairs.interactive.mixture.plot}
\alias{interactive.mixture.plot.JointUniquePairs}
\alias{interactive.mixture.plot,JointUniquePairs-method}


 \title{Interactive plot of mixture model components}

 \description{
 Plots the correlation densities of the empirical fit, mixture fit and each of the mixture components
 for a mixture model derived from  JointUniquePairs and Corr objects with optional subsetting
 by a group of ID Maps. This is an interactive version of JointUniquePairs$mixture.plot unitilizing the
 JointUniquePairs$interactive.plot wrapper functionality.
 }

 \usage{\method{interactive.mixture.plot}{JointUniquePairs}(this, corr, groups="loop", new.plot=FALSE, file.copy=FALSE, subsetting=FALSE, G=c(1:5), verbose=TRUE, ...)}

 \arguments{
 \item{corr}{Corr object.}
 \item{groups}{either ID Map  names, 'menu' or 'loop'. If ID Map names, the functionality is similar to JointUniquePairs$mixture.plot
 If 'menu' or 'loop', user can select the pair of ID Maps interactively from the graphical selection dialog simultaneously controlling
 the plot destination (same or new graphic device) wherther or not the plot should be saved in a file etc. If 'loop', the interactive
 drawing cycle repeats untill the user cancells the process.}
 \item{new.plot}{\code{\link[base]{logical}} indicating if the new graphic device should be created for plot.
 In interactive mode can be selected graphically. Default is \code{\link[base:logical]{FALSE}}.}
 \item{file.copy}{\code{\link[base]{logical}} or a \code{\link[base]{character}} string indicating if the plot should be saved to a file.
 If \code{\link[base]{character}} string, it's content used as a file name and if \code{\link[base:logical]{TRUE}}, the default file named is formed and used.
 In interactive mode can be toggled graphically. Default is \code{\link[base:logical]{FALSE}}.}
 \item{subsetting}{If \code{\link[base:logical]{TRUE}}, subsets the  Corr on a group of ID Maps to compute the mixture model
 or uses the original Corr otherwise.  In interactive mode can be toggled graphically. Default is \code{\link[base:logical]{FALSE}}.}
 \item{G}{The number of mixture model components. If a numerical vector, the optimal mixture model is computed. Default is c(1:5).}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Additional graphical parameters}
 }

 \examples{
 #plot mixture densities for a given DB set
 #treating the given DB set as a full group (subsetting=TRUE)
 examples$jointUniquePairs$interactive.mixture.plot(examples$corr,
		groups=c("NetAffx_Q","DAVID_Q","EnVision_Q"),
		subsetting=TRUE,new.plot=TRUE,file.copy=FALSE,G=c(1:5),
		cex.lab=1.4,cex.legend=1.2,verbose=TRUE);

 \dontrun{
 #plot mixture densities choosing the DB set interactively
 examples$jointUniquePairs$interactive.mixture.plot(examples$corr,
		G=c(1:5),verbose=TRUE,cex.lab=1.4,cex.legend=1.1);
 }}

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
