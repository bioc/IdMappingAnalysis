%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/jointUniquePairs.extensions.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getBootstrap.JointUniquePairs}
\alias{getBootstrap.JointUniquePairs}
\alias{JointUniquePairs.getBootstrap}
\alias{getBootstrap,JointUniquePairs-method}

 \title{Create Bootstrap object from JointUniquePairs object and two experiment sets}

 \description{
 Creates Bootstrap  object from JointUniquePairs object and two experiment sets
 optionally subsetting on a list of ID Maps from the JointUniquePairs object
 }

 \usage{
\method{getBootstrap}{JointUniquePairs}(this, expSet1, expSet2, groups=NULL, Fisher=FALSE, R=200, verbose=FALSE, ...)
}

 \arguments{
 \item{expSet1}{First ExperimentSet object with primary IDs corresponding (partially intersecting)
 with primaryIDs of the given JointUniquePairs object.}
 \item{expSet2}{Second ExperimentSet object with primary IDs corresponding (partially intersecting)
 with secondaryIDs of the given JointUniquePairs object.}
 \item{groups}{If not \code{\link[base]{NULL}}, defines the subset from UniquePairsMatch on which
 the full event group to be formed. Default is \code{\link[base]{NULL}}.}
 \item{Fisher}{If \code{\link[base:logical]{TRUE}}, the Fisher transform of data is performed during the bootstrapping. Default is \code{\link[base:logical]{FALSE}}.}
 \item{R}{The number of bootstrap replicates. Default is 200.}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{
 the Bootstrap object
 }

 \examples{
 #create data structure containing the  bootstrapping results (correlation and sd)
 bootstrap<-examples$jointUniquePairs$getBootstrap(examples$msmsExperimentSet,examples$mrnaExperimentSet,
		groups=c("NetAffx_Q","DAVID_Q","DAVID_F","EnVision_Q"),R=20,verbose=TRUE);
 bootstrap$plot();
 }

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
