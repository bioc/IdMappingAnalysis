%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  jointIdMap.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getDiff.JointIdMap}
\alias{getDiff.JointIdMap}
\alias{JointIdMap.getDiff}
\alias{getDiff.JointIdMap}
\alias{getDiff,JointIdMap-method}


 \title{Create an IdMapDiff object}

 \description{
 A fail-safe way to create IdMapDiff object from the ID Map pair
 which is guaranteed to be properly aligned (see also IdMapDiff class)
 }

 \usage{\method{getDiff}{JointIdMap}(this, idMapName1, idMapName2, verbose=FALSE, ...)}

 \arguments{
 \item{idMapName1}{The name of the first ID Map within the joint ID Map object.}
 \item{idMapName2}{Thee name of the second ID Map within the joint ID Map object.}
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is @FASLE.}
 \item{...}{Not used}
 }

 \value{
 An IdMapDiff object created from two IdMap objects extracted
 from a given JointIdMap object based on their names (idMapName1 and idMapName2).
 }

 \examples{
 jointIdMap<-JointIdMap(examples$identDfList);
 mapDiff<-jointIdMap$getDiff("NetAffx_F","DAVID_Q",verbose=TRUE);
 class(mapDiff);
 mapDiff[1:10,];
 }
 \seealso{For more information see \code{\link{JointIdMap}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
