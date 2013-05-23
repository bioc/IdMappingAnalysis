%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/jointUniquePairs.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getMatchInfo.JointUniquePairs}
\alias{getMatchInfo.JointUniquePairs}
\alias{JointUniquePairs.getMatchInfo}
\alias{getMatchInfo,JointUniquePairs-method}

 \title{Get match table(s) for a given set of primary IDs}

 \description{Get match table(s) for a given set of primary IDs.}

 \usage{
\method{getMatchInfo}{JointUniquePairs}(this, IDs, idMapNames=NULL, ...)
}

 \arguments{
 \item{IDs}{A \code{\link[base]{character}} vector of primary IDs.}
 \item{idMapNames}{A \code{\link[base]{character}} vector of Id Map names encapsulated within the JointUniquePairs
 object or \code{\link[base]{NULL}} (default),in which case all Id Maps are used.}
 \item{...}{Not used}
 }

 \value{
 A \code{\link[base]{list}} of data.frames representing a match table(s) for
 a set of given ID Maps and each particular primary ID.
 }

 \examples{
 IDs<-examples$identDfList[["DAVID_Q"]][,1];
 matches<-getMatchInfo(examples$jointUniquePairs,IDs,c("NetAffx_Q","DAVID_Q","EnSembl_F"));
 }

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
