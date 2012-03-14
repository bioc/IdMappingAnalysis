%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  jointUniquePairs.extensions.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getCorr.JointUniquePairs}
\alias{getCorr.JointUniquePairs}
\alias{JointUniquePairs.getCorr}
\alias{getCorr.JointUniquePairs}
\alias{getCorr,JointUniquePairs-method}


 \title{Extract a set of correlation objects from given JointUniquePairs object
 and corresponding Corr object}

 \description{
 Generate a list of correlation objects based on a given correlation object
 treated as a 'joint' object(i.e. produced through the pipeline
 JointIdMap->UniquePairs->Corr and a set of matches for a set of DBs
 stored in a JointUniquePairs object.
 }

 \usage{\method{getCorr}{JointUniquePairs}(this, corr, groups=NULL, full.group=FALSE, verbose=FALSE, ...)}

 \arguments{
 \item{corr}{Corr object from which the Corr object(s) retrieved
 for each particular DB based on which the patricular Corr object is retrieved.}
 \item{groups}{Optional \code{\link[base]{character}} vector of DB names encapsulated within the JointUniquePairs object defining the set of
 match groups to be used for retrieving the corresponding Corr objects.
 If \code{\link[base]{NULL}} (default), all match groups within the given UniquePairsObject are used.}
 \item{full.group}{ Determines if the resulting Corr object list should represent a full group. Default is \code{\link[base:logical]{FALSE}}.}
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{
 Corr object or a \code{\link[base]{list}} ofCorr  objects with names corresponding to the names of
 particular match groups contained within the given JointUniquePairs object
 }

 \examples{
 # Create and plot the set of corrs for a given DB subset treating subset as a full group
 # and plot the correlation densities including union
 corrSet<- examples$jointUniquePairs$getCorr(examples$corr,
	groups=c("union", "NetAffx_Q", "DAVID_Q", "EnVision_Q"),
     full.group=TRUE, verbose=TRUE);
 Corr$plot(corrSet, title="");
 }

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
