%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  idMapDiffCounts.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getCompoundGroups.IdMapDiffCounts}
\alias{getCompoundGroups.IdMapDiffCounts}
\alias{IdMapDiffCounts.getCompoundGroups}
\alias{getCompoundGroups.IdMapDiffCounts}
\alias{getCompoundGroups,IdMapDiffCounts-method}


 \title{Get counts for each compound event in IdMapDiffCounts}

 \description{
 Get counts for each compound event in IdMapDiffCounts. Used internally within IdMapDiffCounts$plot().
 }

 \usage{\method{getCompoundGroups}{IdMapDiffCounts}(this, verbose=FALSE, ...)}

 \arguments{
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{
 A list of data frames corresponding to a particular compaund event
 ('excess both', 'excess left' etc.) where each data frame has the same structure as a data frame
 within the IdMapDiffCounts object.
 }

 \examples{
 #create IdMapDiffCounts object
 IDs<-IdMapBase$primaryIDs(examples$msmsExperimentSet);
 jointIdMap<-JointIdMap(examples$identDfList,primaryIDs=IDs);
 diffs<-jointIdMap$getDiff("NetAffx_F","DAVID_Q",verbose=TRUE);
 diffCounts<-IdMapDiffCounts(diffs);

 # get compound events
 groups<-diffCounts$getCompoundGroups();
 names(groups);
 groups$excess.both[1:10,];
 groups$same.list[1:10,];
 }

 \seealso{For more information see \code{\link{IdMapDiffCounts}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
