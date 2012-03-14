%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  jointUniquePairs.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{subsetGroups.JointUniquePairs}
\alias{subsetGroups.JointUniquePairs}
\alias{JointUniquePairs.subsetGroups}
\alias{subsetGroups.JointUniquePairs}
\alias{subsetGroups,JointUniquePairs-method}


 \title{Get a JointUniquePairs subset}

 \description{
 Creates a new UniquePairsMatch object which represents a subset of UniquePairs in the
 original object. Only the rows which have at least a single match are kept from the original object,
 so the resulting object events form the unity i.e. full event group.
 }

 \usage{\method{subsetGroups}{JointUniquePairs}(x, groups=NULL, verbose=FALSE, ...)}

 \arguments{
 \item{groups}{The subset of groups (DBs) incapsulated within
 the given JointUniquePairs object to be subsetted on.
 If \code{\link[base]{NULL}} (default) then all UniquePairs are used.}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{
 JointUniquePairs object representing a full group (unity) for a subset of UniquePairs objects.
 }

 \examples{
 jointIdMap<-JointIdMap(examples$identDfList);
 pairs<-as.UniquePairs(jointIdMap$getUnionIdMap());
 jointPairs<-JointUniquePairs(pairs,jointIdMap$getIdMapList());
 jointPairs$getMapNames();

 jointPairsSubset<-jointPairs$subsetGroups(c("NetAffx_Q","DAVID_Q","EnSembl_F"));
 jointPairsSubset$getMapNames();
 }

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
