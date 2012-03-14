%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  jointIdMap.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{as.data.frame.JointIdMap}
\alias{as.data.frame.JointIdMap}
\alias{JointIdMap.as.data.frame}
\alias{as.data.frame.JointIdMap}
\alias{as.data.frame,JointIdMap-method}


 \title{Retrieve a data frame encapsulated within the given JointIdMap object}
 \description{Retrieve a data frame encapsulated within the given JointIdMap object with additional 'name' and 'secondaryKey' attributes}

 \usage{\method{as.data.frame}{JointIdMap}(x, row.names=NULL, optional=FALSE, ...)}

 \arguments{
 \item{row.names}{Not used}
 \item{optional}{Not used}
 \item{...}{Not used}
 }
 \value{A \code{\link[base]{data.frame}} encapsulated within the given JointIdMap object.
 The data frame attributes 'name' and 'secondaryKey' contain the
 values of corresponding JointIdMap object data fields.}

 \examples{
 jointIdMap<-JointIdMap(examples$identDfList);
 df<-as.data.frame(jointIdMap);
 dim(df);
 attr(df,"secondaryKey");
 }

 \seealso{For more information see \code{\link{JointIdMap}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
