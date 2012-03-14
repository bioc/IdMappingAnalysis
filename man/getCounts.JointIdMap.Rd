%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  jointIdMap.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getCounts.JointIdMap}
\alias{getCounts.JointIdMap}
\alias{JointIdMap.getCounts}
\alias{getCounts.JointIdMap}
\alias{getCounts,JointIdMap-method}


 \title{Create an IdMapCounts object}
 \description{
 Create an IdMapCounts object.
 A fail-safe way to create an IdMapCounts from the ID Map list
 obtained internally through the call to getIdMapList.JointIdMap method
 and therefore guaranteed to be properly aligned
 }

 \usage{\method{getCounts}{JointIdMap}(this, idMapNames=NULL, verbose=FALSE, ...)}

 \arguments{
 \item{idMapNames}{Subset of ID Maps to be used to create IdCounts object.
 If \code{\link[base]{NULL}} (default) all ID maps within the given object included.}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{An IdMapCounts object containing information about the secondary
 ID counts for each primary ID for all ID Maps encapsulated within the given JointIdMap object}

 \examples{
 jointIdMap<-JointIdMap(examples$identDfList);
 cnts<-jointIdMap$getCounts();
 cnts[1:10,];
 }

 \seealso{For more information see \code{\link{JointIdMap}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
