%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  jointIdMap.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getMapNames.JointIdMap}
\alias{getMapNames.JointIdMap}
\alias{JointIdMap.getMapNames}
\alias{getMapNames.JointIdMap}
\alias{getMapNames,JointIdMap-method}


 \title{Get the names of IdMap objects encapsulated within the given JointIdMap object}
 \description{Get the names of IdMap objects encapsulated within the given JointIdMap object.}

 \usage{\method{getMapNames}{JointIdMap}(this, ...)}

 \arguments{\item{...}{Not used}}

 \value{A \code{\link[base]{character}} vector of IdMap names}

 \examples{
 jointIdMap<-JointIdMap(examples$identDfList);
 jointIdMap$getMapNames();
 }
 \seealso{For more information see \code{\link{JointIdMap}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
