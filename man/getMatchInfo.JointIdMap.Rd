%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/jointIdMap.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getMatchInfo.JointIdMap}
\alias{getMatchInfo.JointIdMap}
\alias{JointIdMap.getMatchInfo}
\alias{getMatchInfo,JointIdMap-method}

 \title{Get match table(s) for a given set of primary IDs}
 \description{Get match table(s) for a given set of primary IDs.}

 \usage{
\method{getMatchInfo}{JointIdMap}(this, IDs, idMapNames=NULL, ...)
}

 \arguments{
 \item{IDs}{A \code{\link[base]{character}} vector of primary IDs}
 \item{idMapNames}{A \code{\link[base]{character}} vector of Id Map names encapsulated within the jointIdMap object or NULL,
 in which case all Id Maps are used. Default is NULL.}
 \item{...}{Not used}
 }

 \value{A \code{\link[base]{list}} of match table(s) for a given set of primary IDs. The match table is a data frame
 where the column set is a union of secondary IDs over all idMapNames for a given primary ID
 while each row containes the matches between the primary ID and a particular secondary ID
 for a given IdMap.}

 \examples{
 jointIdMap<-JointIdMap(examples$identDfList);
 #get match info for Uniprot accessions P48539 and Q5T089
 matches<-jointIdMap$getMatchInfo(c("P48539","Q5T089"));
 print(matches);
 }

 \seealso{For more information see \code{\link{JointIdMap}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
