%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  jointIdMap.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getUnionIdMap.JointIdMap}
\alias{getUnionIdMap.JointIdMap}
\alias{JointIdMap.getUnionIdMap}
\alias{getUnionIdMap.JointIdMap}
\alias{getUnionIdMap,JointIdMap-method}


 \title{ Create a union IdMap}

 \description{
 Create a single IDMap object each secondary ID set of which
 is a union of all secondary IDs in a JointIdMap object for a given primary ID
 }

 \usage{\method{getUnionIdMap}{JointIdMap}(this, verbose=FALSE, ...)}

 \arguments{
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is @FASLE.}
 \item{...}{Not used}
 }

 \value{A union IdMap object}

 \examples{
 jointIdMap<-JointIdMap(examples$identDfList);
 idMap<-jointIdMap$getUnionIdMap(verbose=TRUE);
 idMap[1:10,];
 }

 \seealso{For more information see \code{\link{JointIdMap}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
