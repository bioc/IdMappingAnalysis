%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  jointIdMap.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getIdMapList.JointIdMap}
\alias{getIdMapList.JointIdMap}
\alias{JointIdMap.getIdMapList}
\alias{getIdMapList.JointIdMap}
\alias{getIdMapList,JointIdMap-method}


 \title{ Create an Id Map list from a JointIdMap object}

 \description{
 Create an IdMap list from a JointIdMap object ensuring that the resulting
 ID Map list has the same set and order of primary IDs
 }

 \usage{\method{getIdMapList}{JointIdMap}(this, idMapNames=NULL, verbose=FALSE, ...)}

 \arguments{
 \item{idMapNames}{optional vector of ID Map names within the jointIdMap for which the ID Map list constructed.
 If \code{\link[base]{NULL}} (default), then all ID Maps within the jointIdMap will be used.}
 \item{...}{Not used}
 }

 \value{A \code{\link[base]{list}} of IdMap objects}

 \examples{
 jointIdMap<-JointIdMap(examples$identDfList);
 idMaps<-jointIdMap$getIdMapList(idMapNames=jointIdMap$getMapNames()[1:3]);
 class(idMaps[[1]]);
 names(idMaps);
 }

 \seealso{For more information see \code{\link{JointIdMap}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
